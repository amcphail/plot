-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot.Render.Plot.Data
-- Copyright   :  (c) A. V. H. McPhail 2010
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- Rendering 'Figure's
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Plot.Render.Plot.Data (
                                       -- * Rendering
                                       renderData
                                       ) where

-----------------------------------------------------------------------------

import Data.List(partition)

--import Data.Packed.Vector
--import Data.Packed.Matrix
--import Data.Packed()
import Numeric.LinearAlgebra

import qualified Data.Array.IArray as A
import qualified Data.Array.MArray as M

import Data.Word

import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as CM

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Maybe

import Graphics.Rendering.Plot.Types

import Graphics.Rendering.Plot.Render.Types
import Graphics.Rendering.Plot.Render.Plot.Glyph

import Prelude hiding(min,max,abs)
import qualified Prelude

-----------------------------------------------------------------------------

findMinIdx, findMaxIdx :: Vector Double -> Double -> Int -> Int -> Int
findMinIdx v x n max
    | n >= max       = error "data not in range"
    | v @> n >= x    = n
    | otherwise      = findMinIdx v x (n+1) max

findMaxIdx v x n min
    | n < 0          = error "data not in range"
    | v @> n <= x    = n
    | otherwise      = findMaxIdx v x (n-1) min

flipVerticalMatrix :: CM.Matrix
flipVerticalMatrix = CM.Matrix 1 0 0 (-1) 0 0

flipVertical :: C.Render ()
flipVertical = C.transform flipVerticalMatrix

-----------------------------------------------------------------------------

greySurfaceFromMatrix :: C.SurfaceData Int Word8 -> Surface -> Int -> Int -> Int -> IO ()
greySurfaceFromMatrix s m stride r' c' = do
                            let r = rows m
                                c = cols m
                            let fm = flatten m
                                mx = maxElement m
                                mn = minElement m
                            mapM_ (\ri -> mapM_ (\(rj,ci) -> do
                                                             let mi = ((rj `div` r')*c) + (ci `div` c')
                                                             let e = round . (* 255) . (/ (mx-mn)) . (\x -> x - mn) $ (fm @> mi)
                                                             let si = (rj*stride) + ci
                                                             M.writeArray s si e) $ zip (repeat ri) [0..((c*c')-1)]) [0..((r*r')-1)]


-----------------------------------------------------------------------------

renderData :: Ranges -> PlotType -> DataSeries -> Render ()
renderData _ _      (DS_Surf m) = do
                                  (BoundingBox x y w h) <- get
                                  let r = rows m
                                      c = cols m
                                  cairo $ do
                                          C.save
                                          --C.setAntialias C.AntialiasNone
                                          let r' = Prelude.min 4 ((round h) `div` r)
                                              c' = Prelude.min 4 ((round w) `div` c) 
                                          s <- liftIO $ C.createImageSurface C.FormatA8 (c*c') (r*r')
                                          p <- liftIO $ C.imageSurfaceGetPixels s
                                          C.surfaceFlush s
                                          stride <- liftIO $ C.imageSurfaceGetStride s
                                          liftIO $ greySurfaceFromMatrix p m stride r' c'
                                          C.surfaceMarkDirty s
                                          C.setSourceSurface s x y
                                          pa <- C.getSource
                                          pm <- liftIO $ C.patternGetMatrix pa
                                          let pm' = CM.scale ((fromIntegral (c*c'))/w) ((fromIntegral (r*r'))/h) pm
                                          liftIO $ C.patternSetMatrix pa pm'
                                          --C.patternSetFilter pa C.FilterBest
                                          C.rectangle x y w h --(fromIntegral c) (fromIntegral r)
                                          C.paint
                                          C.stroke
                                          C.restore
                                  return ()
renderData r Linear ds = do
                         let aos = case ds of
                                           (DS_Y         os') -> zip (repeat AbsFunction) (A.elems os')
                                           (DS_1toN abs' os') -> zip (repeat abs')        (A.elems os') 
                                           (DS_1to1 aos')     -> A.elems aos'
                                           _                  -> error "renderData: DataSeries not handled"
                         let (los,ups) = partition (\(_,DecSeries o _) -> isLower o) aos
                         (BoundingBox x y w h) <- get
                         let (xmin,xmax) = getRanges XAxis Lower r
                         let xscale = w/(xmax-xmin) 
                         cairo $ C.save
                         let (yminl,ymaxl) = getRanges YAxis Lower r
                         let yscalel = h/(ymaxl-yminl) 
                         -- transform to data coordinates
                         cairo $ do 
                                 C.translate x (y+h)
                                 C.scale xscale yscalel
                                 C.translate (-xmin) yminl
                                 flipVertical
                         mapM_ (renderSeries xmin xmax xscale yscalel) los
                         cairo $ C.restore
                         when (not $ null ups)
                              (do
                               cairo $ C.save
                               let (yminu,ymaxu) = getRanges YAxis Upper r
                               let yscaleu = h/(ymaxu-yminu) 
                               -- transform to data coordinates
                               cairo $ do 
                                       C.translate x (y+h)
                                       C.scale xscale yscaleu
                                       C.translate xmin yminu
                                       flipVertical
                               mapM_ (renderSeries xmin xmax xscale yscaleu) ups)
                         -- could filter annotations as well
                         return ()

renderSeries :: Double -> Double -> Double -> Double -> (Abscissae,DecoratedSeries) -> Render ()
renderSeries xmin xmax xscale yscale (abs,(DecSeries o d)) = do
       dat <- case o of
                     (OrdFunction _ f _)            -> do
                                                       (BoundingBox _ _ w _) <- get
                                                       let t = linspace (round w) (xmin,xmax)
                                                       return $ [(t,mapVector f t)]
                     (OrdPoints _ (Plain o') _)     -> do
                                                       let t = case abs of
                                                                        AbsFunction      -> if isHist d
                                                                                               then fromList [0.0..(fromIntegral $ dim o')]
                                                                                               else fromList [1.0..(fromIntegral $ dim o')]
                                                                        AbsPoints t'     -> t'
                                                       return $ [(t,o')]
                     (OrdPoints _ (Error o' (l,h)) _) -> do
                                                       let t = case abs of
                                                                        AbsFunction      -> if isHist d
                                                                                               then fromList [0.0..(fromIntegral $ dim o')]
                                                                                               else fromList [1.0..(fromIntegral $ dim o')]
                                                                        AbsPoints t'     -> t'
                                                       return $ [(t,o'),(t,o'-l),(t,o'+h)]
       case d of
              (DecLine lt)   -> do
                                formatLineSeries lt xscale yscale
                                mapM_ (\(t',y') -> renderSamples xmin xmax Nothing renderLineSample endLineSample t' y') dat
              (DecPoint pt)  -> do
                                (pz,g) <- formatPointSeries pt xscale yscale
                                let gs = g : Bot : Top : []
                                mapM_ (\(g',(t',y')) -> renderSamples xmin xmax Nothing (renderPointSample xscale yscale pz g') endPointSample t' y') (zip gs dat)
              (DecLinPt lt pt) -> do
                                formatLineSeries lt xscale yscale
                                mapM_ (\(t',y') -> renderSamples xmin xmax Nothing renderLineSample endLineSample t' y') dat
                                (pz,g) <- formatPointSeries pt xscale yscale
                                let gs = g : Bot : Top : []
                                mapM_ (\(g',(t',y')) -> renderSamples xmin xmax Nothing (renderPointSample xscale yscale pz g') endPointSample t' y') (zip gs dat)
              (DecImpulse lt) -> do
                                formatLineSeries lt xscale yscale
                                mapM_ (\(t',y') -> renderSamples xmin xmax Nothing renderImpulseSample endImpulseSample t' y') dat
              (DecStep lt) -> do
                              formatLineSeries lt xscale yscale
                              mapM_ (\(t',y') -> renderSamples xmin xmax Nothing renderStepSample endStepSample t' y') dat
              (DecArea lt) -> do
                              formatLineSeries lt xscale yscale
                              let hd = head dat
                                  ln = dim $ fst hd
                                  xmin_ix = findMinIdx (fst hd) xmin 0 (ln-1)
                                  x0 = (fst hd) @> xmin_ix
                                  y0 = (snd hd) @> xmin_ix
                              mapM_ (\(t',y') -> renderSamples xmin xmax Nothing renderAreaSample (endAreaSample x0 y0) t' y') dat
              (DecBar bt)   -> do
                               (bw,bc,c) <- formatBarSeries bt xscale yscale
                               mapM_ (\(t',y') -> renderSamples xmin xmax Nothing (renderBarSample bw bc c) endBarSample t' y') dat
              (DecHist bt)  -> do
                               (bw,bc,c) <- formatBarSeries bt xscale yscale
                               let hd = head dat
                                   ln = dim $ fst hd
                                   xmin_ix = findMinIdx (fst hd) xmin 0 (ln-1)
                                   rest v = subVector 1 (dim v - 1) v
                                   x0 = (fst hd) @> xmin_ix
                                   y0 = 0
                               mapM_ (\(t',y') -> renderSamples xmin xmax (Just $ C.moveTo x0 y0) (renderHistSample bw bc c) endHistSample (rest t') y') dat
       return ()

-----------------------------------------------------------------------------

formatLineSeries' :: [Dash] -> LineWidth -> Color -> C.Render ()
formatLineSeries' ds lw c = do
                            setDashes ds
                            C.setLineWidth lw 
                            setColour c

formatLineSeries :: LineType -> Double -> Double -> Render ()
formatLineSeries NoLine         _      _      = error "line format of NoLine in a line series"
formatLineSeries (ColourLine c) xscale yscale = do
                                                (LineOptions ds lw) <- asks (_lineoptions . _renderoptions)
                                                cairo $ formatLineSeries' ds ((lw)/(xscale+yscale)) c
formatLineSeries (TypeLine (LineOptions ds lw) c) xscale yscale = cairo $ formatLineSeries' ds ((lw)/(xscale+yscale)) c

formatPointSeries' :: Color -> C.Render ()
formatPointSeries' = setColour

formatPointSeries :: PointType -> Double -> Double -> Render (LineWidth,Glyph)
formatPointSeries (FullPoint (PointOptions pz c) g) _ _ = do
                                                          cairo $ formatPointSeries' c
                                                          return (pz,g)

formatBarSeries' :: LineWidth -> C.Render ()
formatBarSeries' lw = C.setLineWidth lw

formatBarSeries :: BarType -> Double -> Double -> Render (Width,Color,Color)
formatBarSeries (ColourBar c) xscale yscale = do
                                let sc = (xscale+yscale)/2
                                (BarOptions bw lw bc) <- asks (_baroptions . _renderoptions)
                                cairo $ formatBarSeries' (lw/sc)
                                return (bw/sc,c,bc)
formatBarSeries (TypeBar (BarOptions bw lw bc) c) xscale yscale = do
                                let sc = (xscale+yscale)/2
                                cairo $ formatBarSeries' (lw/sc)
                                return (bw/sc,c,bc)

-----------------------------------------------------------------------------

renderSamples :: Double -> Double 
              -> Maybe (C.Render ())
              -> (Double -> Double -> C.Render ()) -> C.Render ()
              -> Vector Double -> Vector Double -> Render ()
renderSamples xmin xmax s f e t y = do
                                  (BoundingBox _ _ w _) <- get
                                  let ln = dim t
                                      m = isMonotoneIncreasing t
                                      (xmin_ix,xmax_ix,num_pts) = if m
                                                                     then (findMinIdx t xmin 0 (ln-1)
                                                                           ,findMaxIdx t xmax (ln-1) 0
                                                                           ,xmax_ix - xmin_ix + 1)
                                                                     else (0,ln-1,1)
                                      diff'' = (fromIntegral num_pts)/w
                                      diff' = round $ if diff'' <= 1 then 1 else diff''
                                      diff = if m then diff' else 1
                                  cairo $ do
                                         case s of
                                                Nothing -> C.moveTo (t @> xmin_ix) (y @> xmin_ix)
                                                Just s' -> s'
                                         _ <- runMaybeT $ mapVectorWithIndexM_ (\i y' -> do
                                            when (i >= xmin_ix && i `mod` diff == 0)
                                                     (do
                                                      renderSample i xmax_ix t f e y')
                                            return ()) y
                                         return ()

-----------------------------------------------------------------------------

renderSample :: Int -> Int -> Vector Double 
             -> (Double -> Double -> C.Render ()) -> C.Render () 
             -> Double -> MaybeT C.Render ()
renderSample ix xmax_ix t f e y
    | ix >= xmax_ix            = do
                                lift $ do
                                       f (t @> ix) y
                                       e
                                fail "end of bounded area"
    | otherwise               = do
                                lift $ f (t @> ix) y

-----------------------------------------------------------------------------

renderLineSample :: Double -> Double -> C.Render ()
renderLineSample = C.lineTo

endLineSample :: C.Render ()
endLineSample = C.stroke

renderPointSample :: Double -> Double -> LineWidth -> Glyph -> Double -> Double -> C.Render ()
renderPointSample xscale yscale pz g x y = do
                                           C.moveTo x y
                                           renderGlyph xscale yscale pz g

endPointSample :: C.Render ()
endPointSample = return ()

renderImpulseSample :: Double -> Double -> C.Render ()
renderImpulseSample x y = do
                          C.moveTo x 0
                          C.lineTo x y
                          C.stroke

endImpulseSample :: C.Render ()
endImpulseSample = return ()

renderStepSample :: Double -> Double -> C.Render ()
renderStepSample x y = do
                       (x',_) <- C.getCurrentPoint
                       C.lineTo x' y
                       C.lineTo x  y

endStepSample :: C.Render ()
endStepSample = C.stroke

renderAreaSample :: Double -> Double -> C.Render ()
renderAreaSample = C.lineTo

endAreaSample :: Double -> Double -> C.Render ()
endAreaSample x0 _ = do
                     (x',_) <- C.getCurrentPoint
                     C.lineTo x' 0
                     C.lineTo x0 0
                      -- C.lineTo x0 y0
                     C.closePath
                     C.fill
                     C.stroke

renderBarSample :: Width -> Color -> Color -> Double -> Double -> C.Render ()
renderBarSample bw c bc x y = do
                              setColour bc
                              C.rectangle (x-bw/2) 0 bw y
                              C.strokePreserve
                              setColour c
                              C.fill
                                 

endBarSample :: C.Render ()
endBarSample = return ()

renderHistSample :: Width -> Color -> Color -> Double -> Double -> C.Render ()
renderHistSample bw c bc x y = do
                               (x',_) <- C.getCurrentPoint
                               C.stroke
                               setColour bc
                               C.rectangle x' 0 (x-x') y
                               C.strokePreserve
                               setColour c
                               C.fill
                               C.moveTo x 0

endHistSample :: C.Render ()
endHistSample = return ()

-----------------------------------------------------------------------------

monoStep :: Double -> MaybeT (State Double) ()
monoStep d = do
             dp <- get
             if d >= dp 
                then do
                     put d
                     return ()
                else fail "negative difference"
{-# INLINE monoStep #-}

isMonotoneIncreasing :: Vector Double -> Bool
isMonotoneIncreasing v = maybe False (\_ -> True) $ evalState (runMaybeT $ (mapVectorM_ monoStep v)) (v @> 0)

-----------------------------------------------------------------------------


