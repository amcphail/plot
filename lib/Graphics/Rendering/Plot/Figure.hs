{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
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
-- Creation and manipulation of 'Figure's
--
-- The same problem of leaked instances as at <http://hackage.haskell.org/packages/archive/graphviz/2999.10.0.1/doc/html/Data-GraphViz-Commands.html#t%3AGraphvizCanvas> occurs here.
--
--
-- /with/, /set/, /clear/, /new/, and /add/ are the operations that can
-- be performed on various elements of a figure.
-- 
-- /glib/\//data-accessor/ abstractions (verbs/modifiers) are planned for future implementations

-----------------------------------------------------------------------------

module Graphics.Rendering.Plot.Figure (
                                       module Data.Colour.Names
                                       -- * Top level operation
                                      , Figure(), FigureState()
                                       -- * Default options
                                      , withTextDefaults
                                      , withLineDefaults 
                                      , withPointDefaults 
                                      , withBarDefaults 
                                      -- * Figures
                                      , newFigure
                                      -- ** Formatting
                                      , setBackgroundColour
                                      , setFigurePadding
                                      , withTitle
                                      , withSubTitle
                                      , setPlots
                                      , withPlot, withPlots  
                                      -- * Sub-plots
                                      ,  Plot()
                                      -- ** Colour
                                      , setPlotBackgroundColour
                                      -- ** Plot elements 
                                      , Border
                                      , setBorder
                                      , setPlotPadding
                                      , withHeading
                                      -- ** Series data
                                      , Function(), Series(), MinMaxSeries(), ErrorSeries()
                                      , Surface()
                                      , SeriesLabel()
                                      , Abscissa(), Ordinate(), Dataset()
                                      , FormattedSeries(), SeriesType(..)
                                      , line, point, linepoint
                                      , impulse, step
                                      , area
                                      , bar
                                      , hist
                                      , candle, whisker
                                      , setDataset
                                      -- * Annotations
                                      , Location, Head, Fill
                                      , Annote()
                                      , arrow
                                      , oval
                                      , rect
                                      , glyph
                                      , text
                                      , cairo
                                      , withAnnotations
                                      -- ** Plot type
                                      , setSeriesType
                                      , setAllSeriesTypes
                                      -- ** Formatting
                                      , PlotFormats()
                                      , withSeriesFormat
                                      , withAllSeriesFormats
                                      -- * Range
                                      , Scale(..)
                                      , setRange
                                      , setRangeFromData
                                      -- * Axes
                                      , Axis
                                      , AxisType(..),AxisSide(..),AxisPosn(..)
                                      , clearAxes
                                      , clearAxis
                                      , addAxis
                                      , withAxis
                                      -- * BarSetting
                                      , BarSetting(..)
                                      , barSetting
                                      -- * Data Sampling
                                      , SampleData
                                      , sampleData
                                      -- * Legend
                                      , Legend
                                      , LegendBorder
                                      , LegendLocation(..), LegendOrientation(..)
                                      , clearLegend
                                      , setLegend
                                      , withLegendFormat
                                      -- ** Formatting
                                      , Tick(..), TickValues(..), GridLines
                                      , TickFormat(..)
                                      , setTicks
                                      , setGridlines
                                      , setTickLabelFormat
                                      , setTickLabels
                                      , withTickLabelsFormat
                                      , withAxisLabel
                                      , withAxisLine
                                      , withGridLine
                                       -- * Lines
                                      , Line(), LineFormat()
                                      , DashStyle,Dash(..),LineWidth
                                      , clearLineFormat
                                      , setDashStyle
                                      , setLineWidth
                                      , setLineColour
                                      -- * Points
                                      , Point(), PointFormat()
                                      , Glyph(..)
                                      , PointSize
                                      , setGlyph
                                      , setPointSize
                                      , setPointColour
                                      -- * Bars
                                      , Bar(), BarFormat()
                                      , clearBarFormat
                                      , setBarWidth
                                      , setBarColour
                                      , setBarBorderWidth
                                      , setBarBorderColour
                                      -- * Text labels
                                      , Text()
                                      , FontFamily,FontSize,Color
                                      -- | A text element must exist for formatting to work
                                      , clearText
                                      , clearTextFormat
                                      , setText
                                      , setFontFamily
                                      , setFontStyle
                                      , setFontVariant
                                      , setFontWeight
                                      , setFontStretch
                                      , setFontSize
                                      , setFontColour
                                      ) where

-----------------------------------------------------------------------------

--import Data.Packed.Vector
--import Numeric.LinearAlgebra.Linear

--import Data.Word
--import Data.Colour.SRGB
import Data.Colour.Names

import qualified Data.Array.IArray as A

--import qualified Graphics.Rendering.Cairo as C
--import qualified Graphics.Rendering.Pango as P

--import Control.Monad.State
--import Control.Monad.Reader

import Prelude hiding(min,max)

import Graphics.Rendering.Plot.Figure.Text
import Graphics.Rendering.Plot.Figure.Line
import Graphics.Rendering.Plot.Figure.Point
import Graphics.Rendering.Plot.Figure.Bar
import Graphics.Rendering.Plot.Figure.Plot

import Graphics.Rendering.Plot.Types
import Graphics.Rendering.Plot.Defaults

-----------------------------------------------------------------------------

-- | perform some actions on the text defaults, must be run before other text element modifications
withTextDefaults :: Text () -> Figure ()
withTextDefaults m = do
  o <- getDefaults
  let to' = _textoptions o
  let (FontText to _) = execText m to' (FontText to' "")    
  modifyDefaults $ \s -> s { _textoptions = to }

-- | perform some actions on the line defaults, must be run before other line element modifications
withLineDefaults :: Line () -> Figure ()
withLineDefaults m = do
  o <- getDefaults
  let lo' = _lineoptions o
  let (TypeLine lo _) = execLine m lo' (TypeLine lo' black)
  modifyDefaults $ \s -> s { _lineoptions = lo }
                     
-- | perform some actions on the point defaults, must be run before other point modifications
withPointDefaults :: Point () -> Figure ()
withPointDefaults m = do
  o <- getDefaults
  let po' = _pointoptions o
  let (FullPoint po _) = execPoint m po' (FullPoint po' defaultGlyph)
  modifyDefaults $ \s -> s { _pointoptions = po }

-- | perform some actions on the bar defaults, must be run before other point modifications
withBarDefaults :: Bar () -> Figure ()
withBarDefaults m = do
  o <- getDefaults
  let bo' = _baroptions o
  let (TypeBar bo _) = execBar m bo' (TypeBar bo' black)
  modifyDefaults $ \s -> s { _baroptions = bo }
                     
-----------------------------------------------------------------------------

-- | create a new blank 'Figure'
newFigure :: Figure ()
newFigure = putFigure $ Figure defaultFigureBackgroundColour 
                          defaultFigurePadding NoText NoText
                          (A.listArray ((1,1),(1,1)) [Nothing]) 
{-
newLineFigure :: DataSeries                      -- ^ the y series
              -> FigureData
newLineFigure d@(DS_1toN _ _) = let ((xmin,xmax),(ymin,ymax)) = calculateRanges d
                                    plot = Plot False defaultPlotPadding NoText
                                           (defaultRanges xmin xmax ymin ymax) 
                                           [defaultXAxis,defaultYAxis] 
                                           Nothing Line d []
                                in Figure defaultFigurePadding NoText NoText
                                       (A.listArray ((1,1),(1,1)) [Just plot]) 
-}
{-
-- | create a new 'Figure'
newFigure :: PlotType -> DataSeries -> Figure ()
newFigure Line   d@(DS_1toN _ _) = putFigure $ newLineFigure d
--newFigure _      _               = error "Figure type not implemented"
-}


-----------------------------------------------------------------------------

-- | set the background colour of the figure
setBackgroundColour :: Color -> Figure ()
setBackgroundColour c = modifyFigure $ \s -> s { _back_clr = c }

-- | set the padding of the figure
setFigurePadding :: Double -> Double -> Double -> Double -> Figure ()
setFigurePadding l r b t = modifyFigure $ \s -> 
                             s { _fig_pads = Padding l r b t }

-- | operate on the title
withTitle :: Text () -> Figure ()
withTitle m = do
  o <- getDefaults
  modifyFigure $ \s -> 
    s { _title = execText m (_textoptions o) (_title s) }

-- | operate on the sub-title
withSubTitle :: Text () -> Figure ()
withSubTitle m = do
  o <- getDefaults
  modifyFigure $ \s -> 
    s { _subtitle = execText m (_textoptions o) (_title s) }

-- | set the shape of the plots, losing all current plots
setPlots :: Int      -- ^ rows
         -> Int      -- ^ columns
         -> Figure ()
setPlots r c = modifyFigure $ \s -> 
                 s { _plots = A.listArray ((1,1),(r,c)) 
                      (replicate (r*c) Nothing) }

-- | perform some actions on the specified subplot
withPlot :: (Int,Int) -> Plot () -> Figure ()
withPlot i m = do
  o <- getDefaults
  s <- getSupplies
  modifyFigure $ \p -> 
    p { _plots = let plots = _plots p
                     plot' = plots A.! i
                     plot = case plot' of
                              Nothing -> emptyPlot
                              Just p' -> p'
                                                    -- we revert supplies to the original here
                                                    -- since we might want the same colour
                                                    -- order for all plots 
                                                    -- HOWEVER: need a better execPlot group
                 in plots A.// [(i,Just $ execPlot m s o plot)] }
               
-- | perform some actions all subplots
withPlots :: Plot () -> Figure ()
withPlots m = do
  o <- getDefaults
  s <- getSupplies
  modifyFigure $ \p -> 
    p { _plots = let plots = _plots p
                     plot p' = case p' of
                                 Nothing  -> emptyPlot
                                 Just p'' -> p''
                 in plots A.// map (\(i,e) -> 
                          (i,Just $ execPlot m s o (plot e))) (A.assocs plots) }
               
-----------------------------------------------------------------------------

