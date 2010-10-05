{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

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
--import Prelude.Unicode

import Foreign.Storable 
import Foreign.Ptr

--import Data.Packed.Vector
--import Data.Packed.Matrix
--import Data.Packed()
import Numeric.LinearAlgebra

import qualified Data.Array.IArray as A
--import qualified Data.Array.MArray as M
import qualified Data.Array.Base as B

import Data.Word

import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as CM

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Maybe

import Graphics.Rendering.Plot.Types

import Graphics.Rendering.Plot.Render.Types
import Graphics.Rendering.Plot.Render.Plot.Format
import Graphics.Rendering.Plot.Render.Plot.Glyph
import Graphics.Rendering.Plot.Render.Plot.Annotation

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
                        B.unsafeWrite s si e) $ zip (repeat ri) [0..((c*c')-1)]) [0..((r*r')-1)]


----------------------------------------------------------------------------

renderData :: Ranges -> DataSeries -> Render ()
renderData _    (DS_Surf m) = do 
  (BoundingBox x y w h) <- get
  let r = rows m
      c = cols m
  cairo $ do
    C.save
    --C.setAntialias C.AntialiasNone
    let r'' = Prelude.min 4 ((round h) `div` r)
        c'' = Prelude.min 4 ((round w) `div` c)
        r' = if r'' < 1 then 1 else r''
        c' = if c'' < 1 then 1 else c''
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

renderData r ds = do
  let aos = case ds of
              (DS_Y         os') -> zip (repeat AbsFunction) (A.elems os')
              (DS_1toN abs' os') -> zip (repeat abs')        (A.elems os') 
              (DS_1to1 aos')     -> A.elems aos'
              _                  -> error "renderData: DataSeries not handled"
  let (los,ups) = partition (\(_,DecSeries o _) -> isLower o) aos
  (BoundingBox x y w h) <- get
  let (xsc,xmin',xmax') = getRanges XAxis Lower r
  let (xmin,xmax) = if xsc == Log then (logBase 10 xmin',logBase 10 xmax') else (xmin',xmax')
  let xscale = w/(xmax-xmin) 
  cairo $ C.save
  let (yscl,yminl',ymaxl') = getRanges YAxis Lower r
  let (yminl,ymaxl) = if yscl == Log then (logBase 10 yminl',logBase 10 ymaxl') else (yminl',ymaxl')
  let yscalel = h/(ymaxl-yminl) 
  -- transform to data coordinates
  cairo $ do 
    C.translate x (y+h)
    C.scale xscale yscalel
    C.translate (-xmin) yminl
    flipVertical
  mapM_ (renderSeries xsc yscl xmin xmax xscale yscalel) los
  cairo $ C.restore
  when (not $ null ups)
           (do
             cairo $ C.save
             let (yscu,yminu',ymaxu') = getRanges YAxis Upper r
             let (yminu,ymaxu) = if yscu == Log then (logBase 10 yminu',logBase 10 ymaxu') else (yminu',ymaxu')
             let yscaleu = h/(ymaxu-yminu) 
             -- transform to data coordinates
             cairo $ do 
               C.translate x (y+h)
               C.scale xscale yscaleu
               C.translate xmin yminu
               flipVertical
             mapM_ (renderSeries xsc yscu xmin xmax xscale yscaleu) ups
             cairo $ C.restore)
             -- could filter annotations as well
  return ()

renderSeries :: Scale -> Scale 
             -> Double -> Double -> Double -> Double 
             -> (Abscissae,DecoratedSeries) -> Render ()
renderSeries xsc ysc xmin xmax xscale yscale (abs,(DecSeries o d)) = do
  dat' <- case o of
          (OrdFunction _ f _)            -> do
                 (BoundingBox _ _ w _) <- get
                 let t = linspace (round w) (xmin,xmax)
                 return $ Left [((True,t),mapVector f t)]
          (OrdPoints _ (Plain o') _)     -> do
                 let t = case abs of
                           AbsFunction      -> if isHist d
                                              then (True,fromList [0.0..(fromIntegral $ dim o')])
                                              else (True,fromList [1.0..(fromIntegral $ dim o')])
                           AbsPoints mi t'  -> (mi,t')
                 return $ Left [(t,o')]
          (OrdPoints _ (Error o' (l,h)) _) -> do
                 let t = case abs of
                           AbsFunction      -> if isHist d
                                              then (True,fromList [0.0..(fromIntegral $ dim o')])
                                              else (True,fromList [1.0..(fromIntegral $ dim o')])
                           AbsPoints mi t'  -> (mi,t')
                 return $ Left [(t,o'),(t,o'-l),(t,o'+h)]
          (OrdPoints _ (MinMax o' (Just (l,h))) _) -> do
                 let t = case abs of
                           AbsFunction      -> (True,fromList [1.0..(fromIntegral $ dim l)])
                           AbsPoints mi t'  -> (mi,t')
                 return $ Right [((t,o'),(t,(l,h)))]
  let dat = case dat' of
            Left dat'' → map (\((m,a),b) -> Left (if xsc == Log then (m,logBase 10 a) else (m,a)
                                                ,if ysc == Log then (logBase 10 b) else b)) dat''
            Right dat''' -> map (\(((m1,a),(bl,bu)),((m2,c),(dl,du))) → let (a',c') = if xsc == Log then (logBase 10 a,logBase 10 c) else (a,c)
                                                                            (bl',bu',dl',du') = if ysc == Log then (logBase 10 bl,logBase 10 bu,logBase 10 dl,logBase 10 du) else (bl,bu,dl,du) 
                                                                       in Right (((m1,a'),(bl',bu')),((m2,c'),(dl',du')))) dat'''
  case d of
    (DecLine lt)   -> do
           formatLineSeries lt xscale yscale
           mapM_ (\(t',y') -> renderSamples xmin xmax Nothing 
                             renderLineSample endLineSample t' y') (map (either id (error "MinMax data")) dat)
    (DecPoint pt)  -> do
           (pz,g) <- formatPointSeries pt xscale yscale
           let gs = g : Bot : Top : []
           mapM_ (\(g',(t',y')) -> renderSamples xmin xmax Nothing 
                                  (renderPointSample xscale yscale pz g') endPointSample t' y') 
                     (zip gs (map (either id (error "MinMax data")) dat))
    (DecLinPt lt pt) -> do
           formatLineSeries lt xscale yscale
           mapM_ (\(t',y') -> renderSamples xmin xmax Nothing 
                             renderLineSample endLineSample t' y') (map (either id (error "MinMax data")) dat)
           (pz,g) <- formatPointSeries pt xscale yscale
           let gs = g : Bot : Top : []
           mapM_ (\(g',(t',y')) -> renderSamples xmin xmax Nothing 
                                  (renderPointSample xscale yscale pz g') endPointSample t' y')
                     (zip gs (map (either id  (error "MinMax data")) dat))
    (DecImpulse lt) -> do
           formatLineSeries lt xscale yscale
           mapM_ (\(t',y') -> renderSamples xmin xmax Nothing 
                             renderImpulseSample endImpulseSample t' y') (map (either id (error "MinMax data")) dat)
    (DecStep lt) -> do
           formatLineSeries lt xscale yscale
           mapM_ (\(t',y') -> renderSamples xmin xmax Nothing 
                             renderStepSample endStepSample t' y') (map (either id (error "MinMax data")) dat)
    (DecArea lt) -> do
           formatLineSeries lt xscale yscale
           let Left hd = head dat
               ln = dim $ snd $ fst hd
               xmin_ix = findMinIdx (snd $ fst hd) xmin 0 (ln-1)
               x0 = (snd $ fst hd) @> xmin_ix
               y0 = (snd hd) @> xmin_ix
           mapM_ (\(t',y') -> renderSamples xmin xmax Nothing 
                             renderAreaSample (endAreaSample x0 y0) t' y') (map (either id (error "MinMax data")) dat)
    (DecBar bt)   -> do
           (bw,bc,c) <- formatBarSeries bt xscale yscale
           mapM_ (\(t',y') -> renderSamples xmin xmax Nothing 
                             (renderBarSample bw bc c) endBarSample t' y') (map (either id (error "MinMax data")) dat)
    (DecHist bt)  -> do
           (bw,bc,c) <- formatBarSeries bt xscale yscale
           let Left hd = head dat
               ln = dim $ snd $ fst hd
               xmin_ix = findMinIdx (snd $ fst hd) xmin 0 (ln-1)
               rest (m,v) = (m,subVector 1 (dim v - 1) v)
               x0 = (snd $ fst hd) @> xmin_ix
               y0 = 0
           mapM_ (\(t',y') -> renderSamples xmin xmax (Just $ C.moveTo x0 y0) 
                             (renderHistSample bw bc c) endHistSample (rest t') y') (map (either id (error "MinMax data")) dat)
    (DecCand bt)  → do
      (bw,bc,c) ← formatBarSeries bt xscale yscale
      mapM_ (\((t',y'),(_,e')) → do
               renderMinMaxSamples xmin xmax Nothing
                          (renderWhiskerSample bw bc c False) endWhiskerSample t' e'
               renderMinMaxSamples xmin xmax Nothing
                                 (renderCandleSample bw bc c) endCandleSample t' y'
            ) (map (either (error "Single data") id) dat)
    (DecWhisk bt)  → do
      (bw,bc,c) ← formatBarSeries bt xscale yscale
      mapM_ (\((t',y'),(_,e')) → do
               renderMinMaxSamples xmin xmax Nothing
                          (renderWhiskerSample bw bc c True) endWhiskerSample t' e'
               renderMinMaxSamples xmin xmax Nothing
                                 (renderCandleSample bw bc c) endCandleSample t' y'
            ) (map (either (error "Single data") id) dat)
  return ()

-----------------------------------------------------------------------------

renderSamples :: Double -> Double 
              -> Maybe (C.Render ())
              -> (Double -> Double -> C.Render ()) -> C.Render ()
              -> (Bool,Vector Double) -> Vector Double -> Render ()
renderSamples xmin xmax s f e (mono,t) y = do
                                  (BoundingBox _ _ w _) <- get
                                  let ln = dim t
                                      (xmin_ix,xmax_ix,num_pts) = if mono
                                                                     then (findMinIdx t xmin 0 (ln-1)
                                                                           ,findMaxIdx t xmax (ln-1) 0
                                                                           ,xmax_ix - xmin_ix + 1)
                                                                     else (0,ln-1,ln)
                                      diff'' = floor $ (fromIntegral num_pts)/w
                                      diff' = if diff'' <= 1 then 1 else diff''
                                      diff = if mono then diff' else 1
                                  cairo $ do
                                         case s of
                                                Nothing -> C.moveTo (t @> xmin_ix) (y @> xmin_ix)
                                                Just s' -> s'
                                         _ <- runMaybeT $ do
                                               mapVectorWithIndexM_ (\i y' -> do
                                                 when (i >= xmin_ix && i `mod` diff == 0)
                                                     (renderSample i xmax_ix t f y')
                                                 return ()) y
                                         e

-----------------------------------------------------------------------------

renderMinMaxSamples :: Double -> Double 
              -> Maybe (C.Render ())
              -> (Double -> (Double,Double) -> C.Render ()) -> C.Render ()
              -> (Bool,Vector Double) -> (Vector Double,Vector Double) -> Render ()
renderMinMaxSamples xmin xmax s f e (mono,t) y = do
                                  (BoundingBox _ _ w _) <- get
                                  let ln = dim t
                                      (xmin_ix,xmax_ix,num_pts) = if mono
                                                                     then (findMinIdx t xmin 0 (ln-1)
                                                                           ,findMaxIdx t xmax (ln-1) 0
                                                                           ,xmax_ix - xmin_ix + 1)
                                                                     else (0,ln-1,ln)
                                      diff'' = floor $ (fromIntegral num_pts)/w
                                      diff' = if diff'' <= 1 then 1 else diff''
                                      diff = if mono then diff' else 1
                                  cairo $ do
                                         case s of
                                                Nothing -> C.moveTo (t @> xmin_ix) ((fst $ y) @> xmin_ix)
                                                Just s' -> s'
                                         _ <- runMaybeT $ mapVectorWithIndexM_ (\i t -> do
                                            when (i >= xmin_ix && i `mod` diff == 0)
                                                     (renderMinMaxSample i xmax_ix t f e y)
                                            return ()) t
                                         return ()

-----------------------------------------------------------------------------

renderSample :: Int -> Int -> Vector Double 
             -> (Double -> Double -> C.Render ())
             -> Double -> MaybeT C.Render ()
renderSample ix xmax_ix t f y
    | ix >= xmax_ix            = do
                                lift $ f (t @> ix) y
                                fail "end of bounded area"
    | otherwise               = do
                                lift $ f (t @> ix) y

renderMinMaxSample :: Int -> Int -> Double 
             -> (Double -> (Double,Double) -> C.Render ()) -> C.Render () 
             -> (Vector Double,Vector Double) -> MaybeT C.Render ()
renderMinMaxSample ix xmax_ix t f e (yl,yu)
    | ix >= xmax_ix            = do
                                lift $ do
                                       f t (yl @> ix,yu @> ix)
                                       e
                                fail "end of bounded area"
    | otherwise               = do
                                lift $ f t (yl @> ix,yu @> ix)

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
renderHistSample _ c bc x y = do
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

renderCandleSample :: Width -> Color -> Color -> Double -> (Double,Double) -> C.Render ()
renderCandleSample bw c bc x (yl,yu) = do
                              setColour bc
                              C.rectangle (x-bw/2) yl bw (yu-yl)
                              C.strokePreserve
                              liftIO $ putStrLn $ (show yl) ++ " " ++ (show yu)
                              if (yl < yu)
                                 then do
                                   setColour c
                                 else do
                                   C.setSourceRGBA 1 1 1 1
                              C.fill

endCandleSample :: C.Render ()
endCandleSample = return ()

renderWhiskerSample :: Width -> Color -> Color → Bool -> Double -> (Double,Double) -> C.Render ()
renderWhiskerSample bw _ bc whiskers x (yl,yu) = do
                              setColour bc
                              C.moveTo x yl
                              C.lineTo x yu
                              if whiskers
                                 then do
                                   C.moveTo (x-bw/2) yu
                                   C.lineTo (x+bw/2) yu
                                   C.moveTo (x-bw/2) yl
                                   C.lineTo (x+bw/2) yl
                                 else return ()
                              C.stroke

endWhiskerSample :: C.Render ()
endWhiskerSample = return ()

-----------------------------------------------------------------------------


