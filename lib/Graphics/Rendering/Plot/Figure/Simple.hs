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
                                              plot
                                             ) where

-----------------------------------------------------------------------------

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

-----------------------------------------------------------------------------

