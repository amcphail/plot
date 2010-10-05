
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


