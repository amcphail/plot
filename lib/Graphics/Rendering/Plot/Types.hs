{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot.Types
-- Copyright   :  (c) A. V. H. McPhail 2010
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- Types
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Plot.Types where

-----------------------------------------------------------------------------

import Data.Packed.Vector
import Data.Packed.Matrix

import Data.Colour.SRGB
import Data.Colour()

import qualified Data.Array.IArray as A

import qualified Graphics.Rendering.Pango as P

import Control.Monad.State
import Control.Monad.Reader

import Control.Monad.Supply

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

type Color = Colour Double

-----------------------------------------------------------------------------

-- x,y,w,h
data BoundingBox = BoundingBox { _bbX :: Double, _bbY :: Double
                               , _bbW :: Double, _bbH :: Double }

-----------------------------------------------------------------------------

type FontFamily = String
type FontSize = Double
data FontOptions = FontOptions FontFamily P.FontStyle P.Variant P.Weight P.Stretch
data TextOptions = TextOptions FontOptions FontSize Color
data TextEntry = NoText 
               | BareText String
               | SizeText FontSize Color String
               | FontText TextOptions String

-----------------------------------------------------------------------------

newtype Text a = FT { runText :: ReaderT TextOptions (State TextEntry) a}
    deriving(Monad, MonadReader TextOptions, MonadState TextEntry)

execText :: Text a -> TextOptions -> TextEntry -> TextEntry
execText m r = execState (runReaderT (runText m) r) 

-----------------------------------------------------------------------------

type Solid = Bool

type PointSize = Double
data Glyph = Box | Cross | Diamond | Asterisk | Triangle | Circle | Bullet | Top | Bot
--data GlyphType = Glyph Glyph Solid 
data PointOptions = PointOptions PointSize Color
data PointType = FullPoint PointOptions Glyph

-----------------------------------------------------------------------------

newtype Point a = FG { runPoint :: ReaderT PointOptions (State PointType) a}
    deriving(Monad, MonadReader PointOptions, MonadState PointType)

execPoint :: Point a -> PointOptions -> PointType -> PointType
execPoint m r = execState (runReaderT (runPoint m) r)

-----------------------------------------------------------------------------

data Dash = Dot | Dash
type DashStyle = [Dash]
type LineWidth = Double
-- not using line join
-- not using line cap
-- do we want arrows?
data LineOptions = LineOptions DashStyle LineWidth

data LineType = NoLine
              | ColourLine Color
              | TypeLine LineOptions Color

-----------------------------------------------------------------------------

newtype Line a = FL { runLine :: ReaderT LineOptions (State LineType) a}
    deriving(Monad, MonadReader LineOptions, MonadState LineType)

execLine :: Line a -> LineOptions -> LineType -> LineType
execLine m r = execState (runReaderT (runLine m) r) 

-----------------------------------------------------------------------------

type Width = Double
data BarOptions = BarOptions Width LineWidth Color

data BarType = ColourBar Color
             | TypeBar BarOptions Color

-----------------------------------------------------------------------------

newtype Bar a = FB { runBar :: ReaderT BarOptions (State BarType) a}
    deriving(Monad, MonadReader BarOptions, MonadState BarType)

execBar :: Bar a -> BarOptions -> BarType -> BarType
execBar m r = execState (runReaderT (runBar m) r) 

-----------------------------------------------------------------------------

type Length = Double
type Location = (Double,Double)
type Orientation = Double -- angle
type Arrow = Bool

-- extra glyphs and so on that can be put in a chart
data AnnoteType = AT_Text TextEntry
                | AT_Glyph Glyph
                | AT_Arrow Arrow LineOptions Length

data Annotation = Annotation AnnoteType Location Orientation Color
type Annotations = [Annotation]

-----------------------------------------------------------------------------

data Scale = Linear | Log deriving(Eq)

data Range = Range { _range_scale ∷ Scale, _range_min :: Double, _range_max :: Double }

data Ranges = Ranges (Either Range (Range,Range)) (Either Range (Range,Range))

getRanges :: AxisType -> AxisSide -> Ranges -> (Scale,Double,Double)
getRanges XAxis Lower (Ranges (Left (Range scale xmin xmax)) _)    = (scale,xmin,xmax)
getRanges XAxis Lower (Ranges (Right (Range scale xmin xmax,_)) _) = (scale,xmin,xmax)
getRanges XAxis Upper (Ranges (Right (_,Range scale xmin xmax)) _) = (scale,xmin,xmax)
getRanges XAxis Upper (Ranges (Left _) _)                          = error "no upper range defined"
getRanges YAxis Lower (Ranges _ (Left (Range scale ymin ymax)))    = (scale,ymin,ymax)
getRanges YAxis Lower (Ranges _ (Right (Range scale ymin ymax,_))) = (scale,ymin,ymax)
getRanges YAxis Upper (Ranges _ (Right (_,Range scale ymin ymax))) = (scale,ymin,ymax)
getRanges YAxis Upper (Ranges _ (Left _))                          = error "no upper range defined"

-----------------------------------------------------------------------------

data AxisType  = XAxis | YAxis deriving(Eq)
data AxisSide  = Lower | Upper deriving(Eq)
data AxisPosn  = Side AxisSide
               | Value Double 
  deriving(Eq)

data Tick = Minor | Major deriving(Eq)

type GridLines = Bool
type TickValues = Either Int (Vector Double) -- ^ Either (number of ticks) (tick values)
data Ticks = Ticks GridLines TickValues

setTickGridlines :: GridLines -> Ticks -> Ticks
setTickGridlines gl (Ticks _ tv) = Ticks gl tv

setTickValues :: TickValues -> Ticks -> Ticks
setTickValues tv (Ticks gl _) = Ticks gl tv

type TickFormat = String

data AxisData = Axis {
                  _axis_type     :: AxisType
                  , _position    :: AxisPosn
                  , _line_type   :: LineType
                  , _minor_ticks :: Ticks
                  , _major_ticks :: Ticks
                  , _tick_format :: TickFormat
                  , _label       :: TextEntry
                 }
-- want line styles, so that, e.g., axes in centre of chart are grey or dashed etc.

-----------------------------------------------------------------------------

newtype Axis a = FA { runAxis :: ReaderT Options (State AxisData) a}
    deriving(Monad, MonadReader Options, MonadState AxisData)

execAxis :: Axis a -> Options -> AxisData -> AxisData
execAxis m r = execState (runReaderT (runAxis m) r) 

-----------------------------------------------------------------------------

type LegendBorder = Bool

data LegendLocation = North | NorthEast | East | SouthEast | South 
                    | SouthWest | West | NorthWest
                      deriving(Eq)
data LegendOrientation = Inside | Outside

-- need to have same number of entries as data series
data LegendData = Legend {
                      _bounded    :: Bool   -- is there a box around the legend?
                      , _location :: LegendLocation
                      , _orient   :: LegendOrientation
                      , _leg_fmt  :: TextOptions
                     }
-- do we want a toggle for legends so the labels don't get destroyed?

-----------------------------------------------------------------------------

newtype Legend a = FE { runLegend :: ReaderT TextOptions (State (Maybe LegendData)) a}
    deriving(Monad, MonadReader TextOptions, MonadState (Maybe LegendData))

execLegend :: Legend a -> TextOptions -> (Maybe LegendData) -> (Maybe LegendData)
execLegend m r = execState (runReaderT (runLegend m) r) 

-----------------------------------------------------------------------------

-- simply padding for left, right, bottom, and top
data Padding = Padding Double Double Double Double

-----------------------------------------------------------------------------

data Options = Options {
                        _lineoptions    :: LineOptions
                        , _pointoptions :: PointOptions 
                        , _baroptions   :: BarOptions
                        , _textoptions  :: TextOptions
                       }

-----------------------------------------------------------------------------

data SeriesType = Line | Point | LinePoint | Impulse | Step | Area 
                | Bar | Hist | Candle | Whisker

-----------------------------------------------------------------------------

type Series = Vector Double
type Surface = Matrix Double
type ErrorSeries = Series
type MinMaxSeries = (Series,Series)
type Function = (Double -> Double)

type SeriesLabel = String

--instance Show Function where show _ = "<<function>>"

data OrdSeries = Plain Series
               | Error Series (ErrorSeries,ErrorSeries)
               | MinMax MinMaxSeries (Maybe (ErrorSeries,ErrorSeries))

getOrdData :: OrdSeries -> Series
getOrdData (Plain o)   = o
getOrdData (Error o _) = o
getOrdData (MinMax (o,_) _) = o

getMinMaxData ∷ OrdSeries → Either MinMaxSeries (MinMaxSeries,(ErrorSeries,ErrorSeries))
getMinMaxData (MinMax o Nothing)  = Left o
getMinMaxData (MinMax o (Just e)) = Right (o,e)

data Abscissae = AbsFunction 
               | AbsPoints Series

data Ordinates = OrdFunction AxisSide Function  (Maybe SeriesLabel)
               | OrdPoints   AxisSide OrdSeries (Maybe SeriesLabel)

getOrdLabel :: Ordinates -> (Maybe SeriesLabel)
getOrdLabel (OrdFunction _ _ sl) = sl
getOrdLabel (OrdPoints   _ _ sl) = sl

isLower :: Ordinates -> Bool
isLower (OrdFunction Lower _ _) = True
isLower (OrdPoints   Lower _ _) = True
isLower _                       = False

isUpper :: Ordinates -> Bool
isUpper = not . isLower

data Decoration = DecLine    LineType
                | DecPoint   PointType
                | DecLinPt   LineType  PointType
                | DecImpulse LineType
                | DecStep    LineType
                | DecArea    LineType
                | DecBar     BarType
                | DecHist    BarType
                | DecCand    BarType
                | DecWhisk   BarType

isHist :: Decoration -> Bool
isHist (DecLine _)    = False
isHist (DecPoint _)   = False
isHist (DecLinPt _ _) = False
isHist (DecImpulse _) = False
isHist (DecStep _)    = False
isHist (DecArea _)    = False
isHist (DecBar _)     = False
isHist (DecHist _)    = True
isHist (DecCand _)    = False
isHist (DecWhisk _)   = False

decorationGetLineType :: Decoration -> Maybe LineType
decorationGetLineType (DecLine lt)    = Just lt
decorationGetLineType (DecPoint _)    = Nothing
decorationGetLineType (DecLinPt lt _) = Just lt
decorationGetLineType (DecImpulse lt) = Just lt
decorationGetLineType (DecStep lt)    = Just lt
decorationGetLineType (DecArea lt)    = Just lt
decorationGetLineType (DecBar _)      = Nothing
decorationGetLineType (DecHist _)     = Nothing
decorationGetLineType (DecCand _)     = Nothing
decorationGetLineType (DecWhisk _)    = Nothing
                        
decorationGetPointType :: Decoration -> Maybe PointType
decorationGetPointType (DecLine _)     = Nothing
decorationGetPointType (DecPoint pt)   = Just pt
decorationGetPointType (DecLinPt _ pt) = Just pt
decorationGetPointType (DecImpulse _)  = Nothing
decorationGetPointType (DecStep _)     = Nothing
decorationGetPointType (DecArea _)     = Nothing
decorationGetPointType (DecBar _)      = Nothing
decorationGetPointType (DecHist _)     = Nothing
decorationGetPointType (DecCand _)     = Nothing
decorationGetPointType (DecWhisk _)    = Nothing
                        
decorationGetBarType :: Decoration -> Maybe BarType
decorationGetBarType (DecLine _)     = Nothing
decorationGetBarType (DecPoint _)    = Nothing
decorationGetBarType (DecLinPt _ _)  = Nothing
decorationGetBarType (DecImpulse _)  = Nothing
decorationGetBarType (DecStep _)     = Nothing
decorationGetBarType (DecArea _)     = Nothing
decorationGetBarType (DecBar bt)     = Just bt
decorationGetBarType (DecHist bt)    = Just bt
decorationGetBarType (DecCand bt)    = Just bt
decorationGetBarType (DecWhisk bt)   = Just bt
                        
data DecoratedSeries = DecSeries Ordinates Decoration
--                     BarSeries   Abscissae Ordinates BarType

data DataSeries = DS_Y    (A.Array Int DecoratedSeries)
                | DS_1toN Abscissae (A.Array Int DecoratedSeries)
                | DS_1to1 (A.Array Int (Abscissae,DecoratedSeries))
                | DS_Surf Surface

-----------------------------------------------------------------------------

newtype Data a = FD { runData :: SupplyT SupplyData (ReaderT Options (State DataSeries)) a }
    deriving(Monad, MonadSupply SupplyData, MonadReader Options, MonadState DataSeries)

execData :: Data a -> SupplyData -> Options -> DataSeries -> DataSeries
execData m r s = execState (runReaderT (runSupplyT (runData m) r) s)
 
type FormattedSeries = Data DecoratedSeries

-----------------------------------------------------------------------------

type Border = Bool

-----------------------------------------------------------------------------

data SupplyData = SupplyData {
                      _colours  :: [Color]
                      , _glyphs :: [Glyph]
                     }

instance Supply SupplyData Color where
    nextSupply (SupplyData []     _ ) = error "Empty supply"
    nextSupply (SupplyData (c:cs) gs) = (c,SupplyData cs gs)
instance Supply SupplyData Glyph where
    nextSupply (SupplyData _      []) = error "Empty supply"
    nextSupply (SupplyData cs (g:gs)) = (g,SupplyData cs gs)

-----------------------------------------------------------------------------

-- | a plot 
data PlotData = Plot { 
                  _border      :: Border
                  , _plot_pads :: Padding
                  , _heading   :: TextEntry
                  , _ranges    :: Ranges
                  , _axes      :: [AxisData]
                  , _data      :: DataSeries
                  , _legend    :: Maybe LegendData
                  , _annote    :: Annotations
                 }

-----------------------------------------------------------------------------

type Plots = A.Array (Int,Int) (Maybe PlotData) 

-----------------------------------------------------------------------------

newtype Plot a = FP { runPlot :: SupplyT SupplyData (ReaderT Options (State PlotData)) a }
    deriving(Monad, MonadReader Options, MonadSupply SupplyData, MonadState PlotData)

execPlot :: Plot a -> SupplyData -> Options -> PlotData -> PlotData
execPlot m s r = execState (runReaderT (runSupplyT (runPlot m) s) r)
 
-----------------------------------------------------------------------------

dataInPlot' :: State DataSeries a -> State PlotData a
dataInPlot' m = State $ \s -> let (a,d') = runState m (_data s)
                                  in (a,s { _data = d'})

dataInPlot :: Data a -> Plot a
dataInPlot m = FP $ mapSupplyT (mapReaderT dataInPlot') (runData m)

-----------------------------------------------------------------------------

legendInPlot' :: State (Maybe LegendData) a -> State PlotData a
legendInPlot' m = State $ \s -> let l = _legend s
                                    (a,legend) = runState m l
                                in (a,s { _legend = legend})

legendInPlot :: Legend a -> Plot a
legendInPlot m = FP $ lift $ (withReaderT _textoptions . mapReaderT legendInPlot') (runLegend m)

-----------------------------------------------------------------------------

-- | a chart has a title and contains one or more plots
data FigureData = Figure { 
                        _fig_pads    :: Padding
                        , _title     :: TextEntry
                        , _subtitle  :: TextEntry
                        , _plots     :: Plots
                       }

-----------------------------------------------------------------------------

data FigureState = FigureState {
                           _defaults   :: Options
                           , _supplies :: SupplyData
                           , _figure   :: FigureData
                          }

newtype Figure a = FC { runFigure :: State FigureState a }
    deriving(Monad, MonadState FigureState)

-----------------------------------------------------------------------------

execFigure :: Figure a -> FigureState -> FigureState 
execFigure g = execState (runFigure g)
 
getFigure :: Figure FigureData
getFigure = gets _figure

getDefaults :: Figure Options
getDefaults = gets _defaults 

getSupplies :: Figure SupplyData
getSupplies = gets _supplies

putFigure :: FigureData -> Figure ()
putFigure  p = modify $ \s -> s { _figure = p }

putDefaults :: Options -> Figure ()
putDefaults p = modify $ \s -> s { _defaults = p }

putSupplies :: SupplyData -> Figure ()
putSupplies p = modify $ \s -> s { _supplies = p }

modifyFigure :: (FigureData -> FigureData) -> Figure ()
modifyFigure m = modify $ \s -> s { _figure = m (_figure s) }

modifyDefaults :: (Options -> Options) -> Figure ()
modifyDefaults m = modify $ \s -> s { _defaults = m (_defaults s) }

-----------------------------------------------------------------------------
{-TODO
  * eeglab-like data offset in channels up x-axis
-}
-----------------------------------------------------------------------------
