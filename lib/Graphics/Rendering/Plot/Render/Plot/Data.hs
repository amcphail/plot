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

import Data.Packed.Vector
import Data.Packed()
import Numeric.LinearAlgebra.Linear

import qualified Data.Array.IArray as A

import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as CM

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Maybe

import Graphics.Rendering.Plot.Types

import Graphics.Rendering.Plot.Render.Types

import Prelude hiding(min,max,abs)

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

renderData :: Ranges -> PlotType -> DataSeries -> Render ()
renderData r Linear ds = do
                         let aos = case ds of
                                           (DS_Y         os') -> zip (repeat AbsFunction) (A.elems os')
                                           (DS_1toN abs' os') -> zip (repeat abs')        (A.elems os') 
                                           (DS_1to1 aos')     -> A.elems aos'
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
                                 C.translate xmin yminl
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
                     (OrdFunction _ f)              -> do
                                                       (BoundingBox _ _ w _) <- get
                                                       let t = linspace (round w) (xmin,xmax)
                                                       return $ [(t,mapVector f t)]
                     (OrdPoints _ (Plain o'))       -> do
                                                       let t = case abs of
                                                                        AbsFunction      -> fromList [1.0..(fromIntegral $ dim o')]
                                                                        AbsPoints t'     -> t'
                                                       return $ [(t,o')]
                     (OrdPoints _ (Error o' (l,h))) -> do
                                                       let t = case abs of
                                                                        AbsFunction      -> fromList [1.0..(fromIntegral $ dim o')]
                                                                        AbsPoints t'     -> t'
                                                       return $ [(t,o'),(t,o'-l),(t,o'+h)]
       case d of
              (DecLine lt)   -> do
                                formatLineSeries lt xscale yscale
                                mapM_ (\(t',y') -> renderSamples xmin xmax renderLineSample endLineSample t' y') dat
              (DecPoint pt)  -> do
                                g <- formatPointSeries pt xscale yscale
                                let gs = g : Bot : Top : []
                                mapM_ (\(g',(t',y')) -> renderSamples xmin xmax (renderPointSample xscale yscale g') endPointSample t' y') (zip gs dat)
              (DecLinPt lt pt) -> do
                                formatLineSeries lt xscale yscale
                                mapM_ (\(t',y') -> renderSamples xmin xmax renderLineSample endLineSample t' y') dat
                                g <- formatPointSeries pt xscale yscale
                                let gs = g : Bot : Top : []
                                mapM_ (\(g',(t',y')) -> renderSamples xmin xmax (renderPointSample xscale yscale g') endPointSample t' y') (zip gs dat)
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

formatPointSeries' :: LineWidth -> Color -> C.Render ()
formatPointSeries' lw c = do
                          C.setLineWidth lw
                          setColour c

formatPointSeries :: PointType -> Double -> Double -> Render Glyph
formatPointSeries (FullPoint (PointOptions pz c) g) xscale yscale = do
                                                                    cairo $ formatPointSeries' ((pz)/((xscale+yscale)/2)) c
                                                                    return g


-----------------------------------------------------------------------------

renderSamples :: Double -> Double 
              -> (Double -> Double -> C.Render ()) -> C.Render ()
              -> Vector Double -> Vector Double -> Render ()
renderSamples xmin xmax f e t y = do
                                  (BoundingBox _ _ w _) <- get
                                  let ln = dim t
                                      xmin_ix = findMinIdx t xmin 0 (ln-1)
                                      xmax_ix = findMaxIdx t xmax (ln-1) 0
                                      num_pts = xmax_ix - xmin_ix + 1
                                      diff' = (fromIntegral num_pts)/w
                                      diff = round $ if diff' <= 1 then 1 else diff'
                                  cairo $ do
                                         C.moveTo (t @> xmin_ix) (y @> xmin_ix)
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

renderPointSample :: Double -> Double -> Glyph -> Double -> Double -> C.Render ()
renderPointSample xscale yscale g x y = do
                                        C.moveTo x y
                                        renderGlyph xscale yscale g

endPointSample :: C.Render ()
endPointSample = return ()

-----------------------------------------------------------------------------

glyphWidth :: Double
glyphWidth = 2*pi

renderGlyph :: Double -> Double -> Glyph -> C.Render ()
renderGlyph xscale yscale Box    = renderGlyphBox xscale yscale 
renderGlyph xscale yscale Cross  = renderGlyphCross xscale yscale 
renderGlyph xscale yscale Diamond = renderGlyphDiamond xscale yscale 
renderGlyph xscale yscale Asterisk = renderGlyphAsterisk xscale yscale 
renderGlyph xscale yscale Triangle = renderGlyphTriangle xscale yscale 
renderGlyph xscale yscale Circle = renderGlyphCircle xscale yscale 
renderGlyph xscale yscale Top    = renderGlyphTop xscale yscale    
renderGlyph xscale yscale Bot    = renderGlyphBot xscale yscale   
--renderGlyph _      _      _      = return ()

difference :: Num a => [a] -> [a]
difference [] = []
difference [_] = []
difference (x0:x1:xs) = (x1-x0):(difference (x1:xs))

renderGlyphBox :: Double -> Double -> C.Render ()
renderGlyphBox xscale yscale = do
                               let x = glyphWidth/xscale
                                   y = glyphWidth/yscale
                               C.relMoveTo (-x/2) (-y/2)
                               C.relLineTo 0 y
                               C.relLineTo x 0
                               C.relLineTo 0 (-y)
                               C.closePath
                               C.stroke

renderGlyphCross :: Double -> Double -> C.Render ()
renderGlyphCross xscale yscale = do
                               let x = glyphWidth/xscale
                                   y = glyphWidth/yscale
                               C.relMoveTo (-x/2) 0
                               C.relLineTo x 0
                               C.relMoveTo (-x/2) (-y/2)
                               C.relLineTo 0 y
                               C.closePath
                               C.stroke

renderGlyphDiamond :: Double -> Double -> C.Render ()
renderGlyphDiamond xscale yscale = do
                                   let x = glyphWidth/xscale
                                       y = glyphWidth/yscale
                                   C.relMoveTo (-x/2) 0
                                   C.relLineTo (x/2) y
                                   C.relLineTo (x/2) (-y)
                                   C.relLineTo (-x/2) (-y)
                                   C.closePath
                                   C.stroke

renderGlyphAsterisk :: Double -> Double -> C.Render ()
renderGlyphAsterisk xscale yscale = do
                                    let radius = glyphWidth/2
                                        angles' = map ((+ 90) . (* (360 `div` 5))) [0..4]
                                        angles = map ((* (2*pi/360)) . fromInteger . (`mod` 360)) angles'
                                        xs = map ((* (radius/xscale)) . cos) angles
                                        ys = map ((* (radius/yscale)) . sin) angles
                                    mapM_ (\(x,y) -> do
                                                       C.relLineTo x y
                                                       C.relMoveTo (-x) (-y)) (zip xs ys)
                                    C.stroke

renderGlyphTriangle :: Double -> Double -> C.Render ()
renderGlyphTriangle xscale yscale = do
                                    let radius = glyphWidth/2
                                        angles' = [90,210,330]
                                        --angles' = map ((flip (+) 90) . (* (360 `div` 3))) [0..2]
                                        angles = map ((* (2*pi/360)) . fromInteger . (`mod` 360)) angles'
                                        x@(sx:_) = map ((* (radius/xscale)) . cos) angles
                                        y@(sy:_) = map ((* (radius/yscale)) . sin) angles
                                        xs = difference x
                                        ys = difference y
                                    C.relMoveTo sx sy
                                    mapM_ (uncurry C.relLineTo) (zip xs ys)
                                    C.closePath
                                    C.stroke

renderGlyphCircle :: Double -> Double -> C.Render ()
renderGlyphCircle xscale yscale = do
                                  let radius = glyphWidth/2
                                      angles = map (*(2*pi/36)) [0..35] 
                                      x@(sx:_) = map ((* (radius/xscale)) . cos) angles
                                      y@(sy:_) = map ((* (radius/yscale)) . sin) angles
                                      xs = difference x
                                      ys = difference y
                                  C.relMoveTo sx sy
                                  mapM_ (uncurry C.relLineTo) (zip xs ys)
                                  C.closePath
                                  C.stroke
                        
renderGlyphTop :: Double -> Double -> C.Render ()
renderGlyphTop xscale yscale = do
                               let x = glyphWidth/xscale
                                   y = glyphWidth/yscale
                               C.relMoveTo (-x/2) 0
                               C.relLineTo x 0
                               C.relMoveTo (-x/2) 0
                               C.relLineTo 0 (-y)
                               C.stroke

renderGlyphBot :: Double -> Double -> C.Render ()
renderGlyphBot xscale yscale = do
                               let x = glyphWidth/xscale
                                   y = glyphWidth/yscale
                               C.relMoveTo (-x/2) 0
                               C.relLineTo x 0
                               C.relMoveTo (-x/2) 0
                               C.relLineTo 0 (y)
                               C.stroke







-----------------------------------------------------------------------------


