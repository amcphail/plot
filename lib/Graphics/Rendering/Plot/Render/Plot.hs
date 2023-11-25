{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot.Render.Plot
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

module Graphics.Rendering.Plot.Render.Plot (
                                       -- * Rendering
                                       renderPlots
                                       ) where

-----------------------------------------------------------------------------

--import Data.Either

--import Data.Packed.Vector
--import Numeric.LinearAlgebra.Linear

--import Data.Word

--import Data.Maybe

--import Data.Colour.SRGB
--import Data.Colour.Names

import qualified Data.Array.IArray as A

import Data.Colour.Names

import qualified Graphics.Rendering.Cairo as C
--import qualified Graphics.Rendering.Pango as P

--import Control.Monad.Reader
import Control.Monad.State
--import Control.Monad.Trans

import Graphics.Rendering.Plot.Types
--import Graphics.Rendering.Plot.Defaults

--import Graphics.Rendering.Plot.Figure.Text

import Graphics.Rendering.Plot.Render.Types

import Graphics.Rendering.Plot.Render.Text
import Graphics.Rendering.Plot.Render.Plot.Axis
import Graphics.Rendering.Plot.Render.Plot.Data
import Graphics.Rendering.Plot.Render.Plot.Legend
import Graphics.Rendering.Plot.Render.Plot.Annotation

--import qualified Text.Printf as Printf

--import Prelude hiding(min,max)
--import qualified Prelude(max)

#if MIN_VERSION_mtl(2,3,0)
import Control.Monad
#endif

-----------------------------------------------------------------------------

bbPlot :: Int -> Int -> (Int,Int) -> Render ()
bbPlot r c (px,py) = modify (\(BoundingBox x y w h) ->
  let w' = w/(fromIntegral c)
      h' = h/(fromIntegral r)
  in (BoundingBox
      (x+w'*((fromIntegral py)-1))
      (y+h'*((fromIntegral px)-1))
      w' h'))

renderPlots :: Plots -> Render ()
renderPlots d = do
  let ((x,y),(x',y')) = A.bounds d
      rows = x'-x+1
      cols = y'-y+1
  bb <- get
  mapM_ (\(i,e) -> do
    case e of
      Nothing -> return ()
      Just e' -> do
        bbPlot rows cols i
        renderPlot e'
        put bb) (A.assocs d)

renderPlot :: PlotData -> Render ()
renderPlot (Plot b c p hd r a bc sd d l an) = do
  tx <- bbCentreWidth
  ty <- bbTopHeight
  (_,th) <- renderText hd Centre TTop tx ty
  when (th /= 0) $ bbLowerTop (th+textPad)
  legend <- renderLegend l d
  (axes,padding) <- renderAxes p r a
  renderBorder b
  cairo C.save
  clipBoundary
  when (c /= white) (do
    cairo $ do
      setColour c
      C.paint)
  renderData r bc sd d
  renderAnnotations r an
  cairo C.restore
  cairo C.save
  legend padding
  cairo C.restore
  cairo C.save
  axes
  cairo C.restore

renderBorder :: Border -> Render ()
renderBorder False = return ()
renderBorder True  = do
  (BoundingBox x y w h) <- get
  cairo $ do
    C.setLineWidth 0.5
    C.rectangle (x+0.5) (y+0.5) w h
    C.stroke

-----------------------------------------------------------------------------
