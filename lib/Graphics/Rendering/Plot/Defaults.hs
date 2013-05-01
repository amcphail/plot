-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot.Defaults
-- Copyright   :  (c) A. V. H. McPhail 2010
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- Default values
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Plot.Defaults where

-----------------------------------------------------------------------------

import Data.Colour.Names

import qualified Data.Array.IArray as A

import qualified Graphics.Rendering.Pango as P

import Graphics.Rendering.Plot.Figure.Text

import Graphics.Rendering.Plot.Types

-----------------------------------------------------------------------------

defaultXAxisSideLowerRange :: (Double,Double)
defaultXAxisSideLowerRange = (-1,1)

-----------------------------------------------------------------------------

defaultColourList :: [Color]
defaultColourList = [blue,red,green,yellow,violet,sienna,royalblue
                    ,pink,tomato,lavender,cyan,crimson,darkgreen
                    ,cadetblue,darkred,yellowgreen]
                    ++ defaultColourList

-----------------------------------------------------------------------------

defaultGlyphList :: [Glyph]
defaultGlyphList = [Box, Diamond, Asterisk, Triangle, Circle] 
                   ++ defaultGlyphList

-----------------------------------------------------------------------------

defaultPointOptions :: PointOptions
defaultPointOptions = PointOptions 1 black

defaultGlyph :: Glyph
defaultGlyph = Circle

defaultPointType :: PointType
defaultPointType = FullPoint defaultPointOptions defaultGlyph

-----------------------------------------------------------------------------

defaultDashStyle :: DashStyle
defaultDashStyle = []

defaultLineWidth :: LineWidth
defaultLineWidth = 1

defaultLineOptions :: LineOptions
defaultLineOptions = LineOptions defaultDashStyle defaultLineWidth

defaultLineType :: LineType
defaultLineType = ColourLine black

defaultGridLine :: LineType
defaultGridLine = ColourLine grey 

-----------------------------------------------------------------------------

defaultBarWidth :: Double
defaultBarWidth = 5

defaultBarBorderWidth :: Double
defaultBarBorderWidth = 1

defaultBarBorderColour :: Color
defaultBarBorderColour = black

defaultBarOptions :: BarOptions
defaultBarOptions = BarOptions defaultBarWidth defaultBarBorderWidth defaultBarBorderColour

defaultBarType :: BarType
defaultBarType = ColourBar red

-----------------------------------------------------------------------------

defaultFontFamily :: FontFamily 
defaultFontFamily = "Sans"
             
defaultFontStyle :: P.FontStyle
defaultFontStyle = P.StyleNormal

defaultFontVariant :: P.Variant
defaultFontVariant = P.VariantNormal

defaultFontWeight :: P.Weight
defaultFontWeight = P.WeightNormal

defaultFontStretch :: P.Stretch
defaultFontStretch = P.StretchNormal

defaultFontOptions :: FontOptions 
defaultFontOptions = FontOptions defaultFontFamily defaultFontStyle defaultFontVariant
                                 defaultFontWeight defaultFontStretch

defaultFontSize :: Double
defaultFontSize = 16

defaultFontColour :: Color
defaultFontColour = black

defaultTextOptions :: TextOptions
defaultTextOptions = TextOptions defaultFontOptions defaultFontSize defaultFontColour

-----------------------------------------------------------------------------

defaultBounding :: BoundingBox
defaultBounding = BoundingBox 0 0 1 1

-----------------------------------------------------------------------------

defaultRanges :: Double -> Double -> Double -> Double -> Ranges
defaultRanges xmin xmax ymin ymax = Ranges (Left (Range Linear xmin xmax)) (Left (Range Linear ymin ymax))

-----------------------------------------------------------------------------

zeroPadding, defaultPadding, defaultFigurePadding, defaultPlotPadding :: Padding
zeroPadding = Padding 0 0 0 0
defaultPadding = Padding 10 10 10 10 
defaultFigurePadding = Padding 10 10 10 10
defaultPlotPadding = Padding 10 10 10 10

-----------------------------------------------------------------------------

solid, empty :: Solid
solid = True
empty = False

-----------------------------------------------------------------------------

defaultOptions :: Options
defaultOptions = Options defaultLineOptions 
                         defaultPointOptions 
                         defaultBarOptions
                         defaultTextOptions

-----------------------------------------------------------------------------

minorTickLength, majorTickLength, tickLabelScale :: Double
minorTickLength = 5.0
majorTickLength = 7.0
tickLabelScale = 0.75

defaultMinorTicks :: Maybe Ticks
defaultMinorTicks = Just $ Ticks NoLine (TickNumber 41)

defaultMajorTicks :: Maybe Ticks
defaultMajorTicks = Just $ Ticks NoLine (TickNumber 5)

defaultTickFormat :: TickFormat
defaultTickFormat = DefaultTickFormat

defaultAxis :: AxisType -> AxisPosn -> AxisData
defaultAxis at axp = Axis at axp 
  defaultLineType defaultMinorTicks defaultMajorTicks
  defaultTickFormat 
  [] NoText

defaultXAxis, defaultYAxis :: AxisData
defaultXAxis = defaultAxis XAxis (Side Lower)
defaultYAxis = defaultAxis YAxis (Side Lower)

-----------------------------------------------------------------------------

defaultLegend :: LegendData
defaultLegend = Legend True East Outside (scaleFontSize legendLabelScale defaultTextOptions) 

legendLabelScale :: Double
legendLabelScale = 0.7

legendSampleWidth :: Double
legendSampleWidth = 10

-----------------------------------------------------------------------------

defaultSupply :: SupplyData
defaultSupply = SupplyData defaultColourList defaultGlyphList 

-----------------------------------------------------------------------------

emptyPlot :: PlotData
emptyPlot = Plot False defaultPlotPadding NoText (Ranges (Left (Range Linear (-1) 1)) (Left (Range Linear (-1) 1)))
                 [] undefined Nothing []

-----------------------------------------------------------------------------

emptyPlots :: Plots
emptyPlots = (A.listArray ((0,0),(0,0)) [])

-----------------------------------------------------------------------------

emptyFigure :: FigureData
emptyFigure = Figure defaultFigurePadding NoText NoText emptyPlots

-----------------------------------------------------------------------------

defaultFigureState :: FigureState
defaultFigureState = FigureState undefined 
                                 defaultSupply
                                 undefined

-----------------------------------------------------------------------------
