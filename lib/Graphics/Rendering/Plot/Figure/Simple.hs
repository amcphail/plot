-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot.Figure
-- Copyright   :  (c) A. V. H. McPhail 2010
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- One line 'Figure' creation
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Plot.Figure.Simple (
                                              -- | All function in this module
                                              --   assume a single plot
                                              -- * Plotting
                                              plot
                                             , parametric
                                              -- * Formatting
                                             , title
                                             , subtitle
                                             , grid
                                             , xrange, yrange
                                             , xlabel, ylabel
                                             ) where

-----------------------------------------------------------------------------

import Numeric.Vector(linspace,mapVector)

import Graphics.Rendering.Plot.Figure

-----------------------------------------------------------------------------

-- | create a figure with a single plot 
--   with lower X and Y axes whose ranges are set from the data
plot :: Dataset d => d -> Figure ()
plot ds = do
          setPlots 1 1
          withPlot (1,1) $ do
                           setDataset ds
                           addAxis XAxis (Side Lower) $ return ()
                           addAxis YAxis (Side Lower) $ return ()
                           setRangeFromData XAxis Lower
                           setRangeFromData YAxis Lower

-- | create a figure with a single parametric plot over n points
--   with lower X and Y axes whose ranges are set from the data
parametric :: (Double -> Double,Double -> Double) -> (Double,Double) -> Int -> Figure ()
parametric (fx,fy) (l,h) n = do
                             setPlots 1 1
                             withPlot (1,1) $ do
                                              let t = linspace n (l,h)
                                              setDataset (Line,mapVector fx t,[mapVector fy t])
                                              addAxis XAxis (Side Lower) $ return ()
                                              addAxis YAxis (Side Lower) $ return ()
                                              setRangeFromData XAxis Lower
                                              setRangeFromData YAxis Lower

-----------------------------------------------------------------------------

-- | set the title
title :: String -> Figure ()
title s = withTitle $ setText s

-- | set the subtitle
subtitle :: String -> Figure ()
subtitle s = withSubTitle $ setText s

-- | set the gridlines
grid :: Bool -> Figure ()
grid b = withPlot (1,1) $ do
                          withAxis XAxis (Side Lower) $ setGridlines Major b
                          withAxis YAxis (Side Lower) $ setGridlines Major b

-- | set the x range
xrange :: Double -> Double -> Figure ()
xrange l h = withPlot (1,1) $ setRange XAxis Lower l h 

-- | set the y range
yrange :: Double -> Double -> Figure ()
yrange l h = withPlot (1,1) $ setRange YAxis Lower l h 

-- | set the x label
xlabel :: String -> Figure ()
xlabel s = withPlot (1,1) $ withAxis XAxis (Side Lower) $ withAxisLabel $ setText s

-- | set the y label
ylabel :: String -> Figure ()
ylabel s = withPlot (1,1) $ withAxis YAxis (Side Lower) $ withAxisLabel $ setText s