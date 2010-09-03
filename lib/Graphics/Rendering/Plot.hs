-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot
-- Copyright   :  (c) A. V. H. McPhail 2010
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- Graphical plots
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Plot (
                                -- * Example
                                -- $example
                                -- * re-exported for convenience
                                module Graphics.Rendering.Plot.Figure.Simple
                               , module Graphics.Rendering.Plot.Figure
                               , module Graphics.Rendering.Plot.Render
                               ) where


-----------------------------------------------------------------------------

import Graphics.Rendering.Plot.Figure.Simple
import Graphics.Rendering.Plot.Figure
import Graphics.Rendering.Plot.Render

-----------------------------------------------------------------------------
{- $example

Create some data:

> ln = 25
> ts = linspace ln (0,1)
> rs = randomVector 0 Gaussian ln
> 
> ss = sin (15*2*pi*ts)
> ds = 0.25*rs + ss
> es = constant (0.25*(stddev rs)) ln
> 
> fs :: Double -> Double
> fs = sin . (15*2*pi*)

Perform actions in 'Figure a' to create a figure

> test_graph = do
>         withTextDefaults $ setFontFamily "OpenSymbol"
>         withTitle $ setText "Testing plot package:"
>         withSubTitle $ do
>                        setText "with 1 second of a 15Hz sine wave"
>                        setFontSize 10
>         setPlots 1 1
>         withPlot (1,1) $ do
>                          setDataset (ts,[point (ds,es) (Cross,red),line fs blue])
>                          addAxis XAxis (Side Lower) $ withAxisLabel $ setText "time (s)"
>                          addAxis YAxis (Side Lower) $ withAxisLabel $ setText "amplitude"
>                          addAxis XAxis (Value 0) $ return ()
>                          setRangeFromData XAxis Lower
>                          setRange YAxis Lower (-1.25) 1.25
 
Render the graph to a Cairo 'Render ()' action that takes the width
and height of the drawing area

> test_render :: (Double,Double) -> Render ()
> test_render = render test_graph

The 'Render a' action can be used in GTK or with Cairo to write to file in PS, PDF, SVG, or PNG

-}


-----------------------------------------------------------------------------
