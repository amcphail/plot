
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

module Graphics.Rendering.Plot.Render.Plot.Format (
                                                   formatLineSeries
                                                  , formatPointSeries
                                                  , formatBarSeries
                                                  ) where

-----------------------------------------------------------------------------

import qualified Graphics.Rendering.Cairo as C

import Graphics.Rendering.Plot.Types

import Graphics.Rendering.Plot.Render.Types

import Control.Monad.Reader

-----------------------------------------------------------------------------

formatLineSeries' :: [Dash] -> LineWidth -> Color -> C.Render ()
formatLineSeries' ds lw c = do
  setDashes ds
  C.setLineWidth lw 
  setColour c

formatLineSeries :: LineType -> Render ()
formatLineSeries NoLine         = error "line format of NoLine in a line series"
formatLineSeries (ColourLine c) = do
  (LineOptions ds lw) <- asks (_lineoptions . _renderoptions)
  cairo $ formatLineSeries' ds lw c
formatLineSeries (TypeLine (LineOptions ds lw) c) =
    cairo $ formatLineSeries' ds lw c

formatPointSeries' :: Color -> C.Render ()
formatPointSeries' = setColour

formatPointSeries :: PointType -> Render (LineWidth,Glyph)
formatPointSeries (FullPoint (PointOptions pz c) g) = do
  cairo $ formatPointSeries' c
  return (pz,g)

formatBarSeries' :: LineWidth -> C.Render ()
formatBarSeries' lw = C.setLineWidth lw

formatBarSeries :: BarType -> Render (Width,Color,Color)
formatBarSeries (ColourBar c) = do
  (BarOptions bw lw bc) <- asks (_baroptions . _renderoptions)
  cairo $ formatBarSeries' lw
  return (bw,c,bc)
formatBarSeries (TypeBar (BarOptions bw lw bc) c) = do
  cairo $ formatBarSeries' lw
  return (bw,c,bc)

-----------------------------------------------------------------------------


