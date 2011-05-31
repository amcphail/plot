-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot.Render.Plot.Annotation
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

module Graphics.Rendering.Plot.Render.Plot.Annotation (
                                       -- * Rendering
                                       renderAnnotations
                                       ) where

-----------------------------------------------------------------------------

import qualified Graphics.Rendering.Cairo as C

import Control.Monad.Reader
import Control.Monad.State

import Graphics.Rendering.Plot.Types
--import Graphics.Rendering.Plot.Defaults

--import Graphics.Rendering.Plot.Figure.Text

import Graphics.Rendering.Plot.Render.Types
import Graphics.Rendering.Plot.Render.Text
import Graphics.Rendering.Plot.Render.Plot.Glyph
import Graphics.Rendering.Plot.Render.Plot.Format

--import Prelude hiding(min,max)
--import qualified Prelude(max)

-----------------------------------------------------------------------------

renderAnnotations :: Ranges -> Annotations -> Render ()
renderAnnotations r an = do
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
  put (BoundingBox (-xmin) (yminl) (xmax-xmin) (ymaxl-yminl))
  mapM_ (renderAnnotation xscale yscalel) an
  put (BoundingBox x y w h)
  cairo $ C.restore

-----------------------------------------------------------------------------

renderAnnotation :: Double -> Double -> Annotation -> Render ()
renderAnnotation xscale yscale (AnnArrow h lt (x1',y1') (x2',y2')) = do
  formatLineSeries lt
  let (x1,y1) = (x1'/xscale,y1'/yscale)
  let (x2,y2) = (x2'/xscale,y2'/yscale)
  cairo $ do
    C.moveTo x1 y1
    C.lineTo x2 y2
    C.stroke
    when h (do
             C.moveTo x2 y2
             let theta = atan2 (y2-y1) (x2-x1)
             lw <- C.getLineWidth
             let ln = lw*10
                 cx = x2 - ln * cos theta
                 cy = y2 - ln * sin theta
                 xl = cx + (ln/2) * sin (theta + pi/2)
                 yl = cy + (ln/2) * cos (theta + pi/2)
                 xu = cx + (ln/2) * sin (theta - pi/2)
                 yu = cy + (ln/2) * cos (theta - pi/2)
             C.lineTo xl yl
             C.lineTo xu yu
             C.closePath
             C.fill
           )
    C.stroke
renderAnnotation xscale yscale (AnnOval f b (x1',y1') (x2',y2')) = do
  (_,bc,c) <- formatBarSeries b
  let (x1,y1) = (x1'/xscale,y1'/yscale)
  let (x2,y2) = (x2'/xscale,y2'/yscale)
  let width = x2 - x1
      height = y2 - y1
      x = x1 + width/2
      y = y1 + height/2
  cairo $ do
    C.save
    setColour c
    C.translate  (x + width / 2) (y + height / 2)
    C.scale (1 / (height / 2)) (1 / (width / 2))
    C.arc 0 0 1 0 (2 * pi)
    C.restore
    C.strokePreserve
    when f (do
             setColour bc
             C.fill)
    C.newPath
renderAnnotation xscale yscale (AnnRect f b (x1',y1') (x2',y2')) = do
  (_,bc,c) <- formatBarSeries b
  let (x1,y1) = (x1'/xscale,y1'/yscale)
  let (x2,y2) = (x2'/xscale,y2'/yscale)
  cairo $ do
    C.save
    setColour c
    C.rectangle x1 y1 x2 y2
    C.restore
    C.strokePreserve
    when f (do
             setColour bc
             C.fill)
    C.newPath
renderAnnotation xscale yscale (AnnGlyph pt (x1',y1')) = do
  (pw,g) <- formatPointSeries pt
  let (x1,y1) = (x1'/xscale,y1'/yscale)
  cairo $ do
    C.moveTo x1 y1
    renderGlyph pw g
renderAnnotation xscale yscale (AnnText te (x1',y1')) = do
--  (x,y) <- cairo $ C.userToDevice x1 y1
  let (x1,y1) = (x1'/xscale,y1'/yscale)
  cairo $ do
    C.save
    --C.scale (recip xscale) (recip (-yscale))
  _ <- renderText te TRight TTop (x1*xscale) (y1*yscale)
  cairo $ C.restore
  return ()
renderAnnotation _   _   (AnnCairo r) = do
  (BoundingBox x y w h) <- get
  cairo $ do
    C.save
    r x y w h
    C.restore

-----------------------------------------------------------------------------
