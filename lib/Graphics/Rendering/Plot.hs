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
                                -- * Re-exported for convenience
                                module Graphics.Rendering.Plot.Figure.Simple
                               , module Graphics.Rendering.Plot.Figure
                               , module Graphics.Rendering.Plot.Render
                                -- * Example
                                -- $example
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
>                          setRangeFromData XAxis Lower Linear
>                          setRange YAxis Lower Linear (-1.25) 1.25
 
Render the graph to a Cairo 'Render ()' action that takes the width
and height of the drawing area

> test_render :: (Double,Double) -> Render ()
> test_render = render test_graph

The same graph using the 'Simple' interface

> test_graph2 = do
>         plot (ts,[point (ds,es) (Cross,red),line fs blue])
>         title "Testing plot package:"
>         subtitle "with 1 second of a 15Hz sine wave"
>         xlabel "time (s)"
>         ylabel "amplitude"
>         yrange Linear (-1.25) 1.25

The 'Render a' action can be used in GTK or with Cairo to write to file in PS, PDF, SVG, or PNG

Display a greyscale matrix

> ms :: Matrix Double
> ms = buildMatrix 64 64 (\(x,y) -> sin (2*2*pi*(fromIntegral x)/64) * cos (5*2*pi*(fromIntegral y)/64))

> mat_fig = do
>         setPlots 1 1
>         withPlot (1,1) $ do 
>                          setDataset ms
>                          addAxis XAxis (Side Lower) $ setTickLabelFormat "%.0f"
>                          addAxis YAxis (Side Lower) $ setTickLabelFormat "%.0f"
>                          setRangeFromData XAxis Lower Linear
>                          setRangeFromData YAxis Lower Linear

The ODE example from hmatrix:

> import Numeric.GSL
> import Numeric.LinearAlgebra

> xdot t [x,v] = [v, -0.95*x - 0.1*v]
> ts = linspace 100 (0,20)
> sol = odeSolve xdot [10,0] ts

> ode_fig = plot (Line,ts,[sol])

-}

-----------------------------------------------------------------------------
