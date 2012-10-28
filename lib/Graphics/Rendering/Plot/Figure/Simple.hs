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
                                              -- * Plotting
                                              plot
                                             , loglog, semilog, linlog, loglin,
                                             , parametric
                                              -- * Formatting
                                             , title
                                             , subtitle
                                             -- | The following functions can
                                             --   be applied to a figure or a plot.
                                             --   When applied in 'Figure' context
                                             --   a single plot is assumed
                                             , Simple()
                                             , grid
                                             , xrange, yrange
                                             , xautorange, yautorange
                                             , xautorangeLog, yautorangeLog
                                             , xlabel, ylabel
                                             ) where

-----------------------------------------------------------------------------

import Numeric.Container

import Graphics.Rendering.Plot.Figure

-----------------------------------------------------------------------------

-- | create a figure with a single linear plot 
--   with lower X and Y axes whose ranges are set from the data
plot :: Dataset d => d -> Figure ()
plot ds = do
          setPlots 1 1
          withPlot (1,1) $ do
                           setDataset ds
                           addAxis XAxis (Side Lower) $ return ()
                           addAxis YAxis (Side Lower) $ return ()
                           setRangeFromData XAxis Lower Linear
                           setRangeFromData YAxis Lower Linear

-- | create a figure with a single linear-log plot 
--   with lower X and Y axes whose ranges are set from the data
semilog :: Dataset d => d -> Figure ()
semilog = linlog
{-# DEPRECATED semilog "use linlog" #-}

-- | create a figure with a single linear-log plot 
--   with lower X and Y axes whose ranges are set from the data
linlog :: Dataset d => d -> Figure ()
linlog ds = do
         setPlots 1 1
         withPlot (1,1) $ do
                          setDataset ds
                          addAxis XAxis (Side Lower) $ return ()
                          addAxis YAxis (Side Lower) $ return ()
                          setRangeFromData XAxis Lower Linear
                          setRangeFromData YAxis Lower Log

-- | create a figure with a single log-linear plot 
--   with lower X and Y axes whose ranges are set from the data
loglin :: Dataset d => d -> Figure ()
loglin ds = do
         setPlots 1 1
         withPlot (1,1) $ do
                          setDataset ds
                          addAxis XAxis (Side Lower) $ return ()
                          addAxis YAxis (Side Lower) $ return ()
                          setRangeFromData XAxis Lower Log
                          setRangeFromData YAxis Lower Linear

-- | create a figure with a single log-log plot 
--   with lower X and Y axes whose ranges are set from the data
loglog :: Dataset d => d -> Figure ()
loglog ds = do
          setPlots 1 1
          withPlot (1,1) $ do
                           setDataset ds
                           addAxis XAxis (Side Lower) $ return ()
                           addAxis YAxis (Side Lower) $ return ()
                           setRangeFromData XAxis Lower Log
                           setRangeFromData YAxis Lower Log

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
                                              setRangeFromData XAxis Lower Linear
                                              setRangeFromData YAxis Lower Linear

-----------------------------------------------------------------------------

-- | set the title
title :: String -> Figure ()
title s = withTitle $ setText s

-- | set the subtitle
subtitle :: String -> Figure ()
subtitle s = withSubTitle $ setText s

-----------------------------------------------------------------------------

class Simple m where
    simple :: Plot () -> m ()

instance Simple Plot where
    simple m = m

instance Simple Figure where
    simple m = withPlot (1,1) m

-- | set the gridlines
grid :: Simple m => Bool -> m ()
grid b = simple $ do
                  withAxis XAxis (Side Lower) $ setGridlines Major b
                  withAxis YAxis (Side Lower) $ setGridlines Major b

-- | set the x range
xrange :: Simple m => Scale -> Double -> Double -> m ()
xrange s l h = simple $ setRange XAxis Lower s l h 

-- | set the y range
yrange :: Simple m => Scale -> Double -> Double -> m ()
yrange s l h = simple $ setRange YAxis Lower s l h

-- | set the x range from data
xautorange :: Simple m => m ()
xautorange = simple $ setRangeFromData XAxis Lower Linear

-- | set the y range from data
yautorange :: Simple m => m ()
yautorange = simple $ setRangeFromData YAxis Lower Linear

-- | set the x range from data
xautorangeLog :: Simple m => m ()
xautorangeLog = simple $ setRangeFromData XAxis Lower Log

-- | set the y range from data
yautorangeLog :: Simple m => m ()
yautorangeLog = simple $ setRangeFromData YAxis Lower Log

-- | set the x label
xlabel :: Simple m => String -> m ()
xlabel s = simple $ withAxis XAxis (Side Lower) $ withAxisLabel $ setText s

-- | set the y label
ylabel :: Simple m => String -> m ()
ylabel s = simple $ withAxis YAxis (Side Lower) $ withAxisLabel $ setText s

-----------------------------------------------------------------------------
