-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot.Figure.Plot.Axis
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

module Graphics.Rendering.Plot.Figure.Plot.Axis (
                                            Axis
                                           , AxisType(..),AxisSide(..),AxisPosn(..)
                                           , Tick(..), TickValues, GridLines
                                           , setTicks
                                           , setTickLabelFormat
                                           , withAxisLabel
                                           , withAxisLine
                                    ) where

-----------------------------------------------------------------------------

import Control.Monad.State
import Control.Monad.Reader

import Graphics.Rendering.Plot.Types

-----------------------------------------------------------------------------

changeLineType :: LineType -> AxisData -> AxisData
changeLineType lt ax = ax { _line_type = lt }

changeMinorTicks :: Ticks -> AxisData -> AxisData
changeMinorTicks t ax = ax { _minor_ticks = t }

changeMajorTicks :: Ticks -> AxisData -> AxisData
changeMajorTicks t ax = ax { _major_ticks = t }

changeTickFormat :: TickFormat -> AxisData -> AxisData
changeTickFormat tf ax = ax { _tick_format = tf }

changeLabel :: (TextEntry -> TextEntry) -> AxisData -> AxisData
changeLabel f ax = ax { _label = f (_label ax) }

-----------------------------------------------------------------------------

-- | format the axis line
withAxisLine :: Line () -> Axis ()
withAxisLine m = do
                 l <- gets _line_type
                 lo <- asks _lineoptions
                 let lt = execLine m lo l
                 modify $ \s -> s { _line_type = lt }

-- | format the axis ticks
setTicks :: Tick -> GridLines -> TickValues -> Axis ()
setTicks Minor g ts = modify $ \s -> changeMinorTicks (Ticks g ts) s
setTicks Major g ts = modify $ \s -> changeMajorTicks (Ticks g ts) s

-- | printf format that takes one argument, the tick value
setTickLabelFormat :: String -> Axis ()
setTickLabelFormat tf = modify $ \s -> changeTickFormat tf s

-- | operate on the axis label
withAxisLabel :: Text () -> Axis ()
withAxisLabel m = do
                  ax <- get
                  to <- asks _textoptions
                  put $ ax { _label = execText m to (_label ax) } 

-----------------------------------------------------------------------------

