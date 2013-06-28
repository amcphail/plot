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
renderPlot (Plot b p hd r a bc d l an) = do
  tx <- bbCentreWidth
  ty <- bbTopHeight
  (_,th) <- renderText hd Centre TTop tx ty
  if th == 0 then return () else bbLowerTop (th+textPad)
{- attempt to have different colour plot area
      (BoundingBox x y w h) <- get
      cairo $ do
             setColour white
             C.moveTo x     y
             C.lineTo (x+w) y
             C.lineTo (x+w) (y+h)
             C.lineTo x     (y+h)
             C.stroke
             C.clip
             C.fill
             C.paint
-}
  legend <- renderLegend l d
  padding <- renderAxes p r a
  renderBorder b
  cairo C.save
  clipBoundary
  renderData r bc d
  renderAnnotations r an
  cairo C.restore
  legend padding

renderBorder :: Border -> Render ()
renderBorder False = return ()
renderBorder True  = do
  (BoundingBox x y w h) <- get
  cairo $ do
    C.setLineWidth 0.5
    C.rectangle (x+0.5) (y+0.5) w h
    C.stroke
                                           
-----------------------------------------------------------------------------


