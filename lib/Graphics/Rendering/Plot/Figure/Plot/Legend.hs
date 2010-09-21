-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot.Figure.Plot.Legend
-- Copyright   :  (c) A. V. H. McPhail 2010
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- Axis
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Plot.Figure.Plot.Legend (
                                                   Legend
                                                  , LegendBorder
                                                  , LegendLocation(..), LegendOrientation(..)
                                                  , clearLegend
                                                  , setLegend
                                                  , withLegendFormat
                                                  ) where

-----------------------------------------------------------------------------

import Control.Monad.State
import Control.Monad.Reader

import Graphics.Rendering.Plot.Types
import Graphics.Rendering.Plot.Defaults

import Graphics.Rendering.Plot.Figure.Text

-----------------------------------------------------------------------------

-- | clear the legend
clearLegend :: Legend ()
clearLegend = put Nothing

-- | set the legend location (required for there to be a legend)
setLegend :: LegendBorder -> LegendLocation -> LegendOrientation -> Legend ()
setLegend b l o = do
                  to <- ask
                  put $ Just $ Legend b l o (scaleFontSize legendLabelScale to) 

-- | operate on the axis label
withLegendFormat :: Text () -> Legend ()
withLegendFormat m = do
                     l <- get
                     let legend = case l of
                                         Nothing -> defaultLegend
                                         Just l' -> l'
                     to' <- ask
                     let (FontText to _) = execText m to' (FontText to' "")
                     put $ Just $ legend { _leg_fmt = to } 

-----------------------------------------------------------------------------

