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
                                           , setGridlines
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

changeMinorTicks :: (Ticks -> Ticks) -> AxisData -> AxisData
changeMinorTicks t ax = ax { _minor_ticks = t (_minor_ticks ax) }

changeMajorTicks :: (Ticks -> Ticks) -> AxisData -> AxisData
changeMajorTicks t ax = ax { _major_ticks = t (_major_ticks ax) }

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
setTicks :: Tick -> TickValues -> Axis ()
setTicks Minor ts = modify $ \s -> changeMinorTicks (setTickValues ts) s
setTicks Major ts = modify $ \s -> changeMajorTicks (setTickValues ts) s

-- | should gridlines be displayed?
setGridlines :: Tick -> GridLines -> Axis ()
setGridlines Minor gl = modify $ \s -> changeMinorTicks (setTickGridlines gl) s
setGridlines Major gl = modify $ \s -> changeMajorTicks (setTickGridlines gl) s

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

