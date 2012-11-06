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
                                                , Tick(..), TickValues(..), GridLines
                                                , setTicks
                                                , setGridlines
                                                , setTickLabelFormat
                                                , setDataLabels
                                                , withDataLabelFormat
                                                , withAxisLabel
                                                , withAxisLine
                                                , withGridLine
                                                ) where

-----------------------------------------------------------------------------

import Data.Maybe

import Control.Monad.State
import Control.Monad.Reader

import Graphics.Rendering.Plot.Types
import Graphics.Rendering.Plot.Defaults

-----------------------------------------------------------------------------

changeLineType :: LineType -> AxisData -> AxisData
changeLineType lt ax = ax { _line_type = lt }

changeMinorTicks :: (Maybe Ticks -> Maybe Ticks) -> AxisData -> AxisData
changeMinorTicks t ax = ax { _minor_ticks = t (_minor_ticks ax) }

changeMajorTicks :: (Maybe Ticks -> Maybe Ticks) -> AxisData -> AxisData
changeMajorTicks t ax = ax { _major_ticks = t (_major_ticks ax) }

changeTickFormat :: TickFormat -> AxisData -> AxisData
changeTickFormat tf ax = ax { _tick_format = tf }

changeLabel :: (TextEntry -> TextEntry) -> AxisData -> AxisData
changeLabel f ax = ax { _label = f (_label ax) }

changeDataLabels :: ([TextEntry] -> [TextEntry]) -> AxisData -> AxisData
changeDataLabels f ax = ax { _data_labels = f (_data_labels ax) }

-----------------------------------------------------------------------------

-- | format the axis line
withAxisLine :: Line () -> Axis ()
withAxisLine m = do
                 l <- gets _line_type
                 lo <- asks _lineoptions
                 let lt = execLine m lo l
                 modify $ \s -> s { _line_type = lt }

-- | format the grid lines
withGridLine :: Tick -> Line () -> Axis ()
withGridLine t m = do
                 lo <- asks _lineoptions
                 (lt',v) <- case t of
                        Minor -> do
                               -- at this point can we guarantee there won't
                               -- be a Nothing?
                               (Just (Ticks lt'' v')) <- gets _minor_ticks
                               return (lt'',v')
                        Major -> do
                               (Just (Ticks lt'' v')) <- gets _major_ticks
                               return (lt'',v')
                 let lt = execLine m lo lt'
                 case t of
                   Minor -> modify $ \s -> s { _minor_ticks = (Just (Ticks lt v)) }
                   Major -> modify $ \s -> s { _major_ticks = (Just (Ticks lt v)) }

-- | format the axis ticks
setTicks :: Tick -> TickValues -> Axis ()
setTicks Minor (TickNumber 0) = modify $ \s -> 
  changeMinorTicks (const Nothing) s
setTicks Minor ts             = modify $ \s -> 
  changeMajorTicks (setTickValues ts) s
setTicks Major (TickNumber 0) = modify $ \s -> 
  changeMajorTicks (const Nothing) s
setTicks Major ts             = modify $ \s -> 
  changeMajorTicks (setTickValues ts) s

-- | should gridlines be displayed?
setGridlines :: Tick -> GridLines -> Axis ()
setGridlines Minor gl = modify $ \s -> changeMinorTicks (setTickGridlines (if gl then defaultGridLine else NoLine)) s
setGridlines Major gl = modify $ \s -> changeMajorTicks (setTickGridlines (if gl then defaultGridLine else NoLine)) s

-- | printf format that takes one argument, the tick value
setTickLabelFormat :: String -> Axis ()
setTickLabelFormat tf = modify $ \s -> changeTickFormat tf s

-- | a list of data labels
setDataLabels :: [String] -> Axis ()
setDataLabels dl = modify $ \s -> 
  changeDataLabels (const (map BareText dl)) s

-- | format the data labels
withDataLabelFormat :: Text () -> Axis ()
withDataLabelFormat m = do
  ax <- get
  to <- asks _textoptions
  put $ ax { _data_labels = map (execText m to) (_data_labels ax) }

-- | operate on the axis label
withAxisLabel :: Text () -> Axis ()
withAxisLabel m = do
                  ax <- get
                  to <- asks _textoptions
                  put $ ax { _label = execText m to (_label ax) } 

-----------------------------------------------------------------------------

