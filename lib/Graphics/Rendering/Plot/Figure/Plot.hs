-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot.Figure.Plot
-- Copyright   :  (c) A. V. H. McPhail 2010
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- Creation and manipulation of 'Plot's
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Plot.Figure.Plot (
                                            Plot
                                           -- * Plot elements 
                                           , Border
                                           , setBorder
                                           , setPlotPadding
                                           , withHeading
                                           -- * Series data
                                           , D.Abscissa(), D.Ordinate(), D.Dataset()
                                           , SeriesLabel
                                           , D.FormattedSeries()
                                           , D.line, D.point, D.linepoint
                                           , D.impulse, D.step
                                           , D.area
                                           , D.bar
                                           , D.hist
                                           , D.candle, D.whisker
                                           , setDataset
                                           -- * Annotations
                                           , Location, Head, Fill
                                           , AN.arrow
                                           , AN.oval
                                           , AN.rect
                                           , AN.glyph
                                           , AN.text
                                           , AN.cairo
                                           , withAnnotations
                                           -- ** Plot type
                                           , setSeriesType
                                           , setAllSeriesTypes
                                           -- ** Formatting
                                           , D.PlotFormats(..)
                                           , withSeriesFormat
                                           , withAllSeriesFormats
                                           -- * Range
                                           , Scale(..)
                                           , setRange
                                           , setRangeFromData
                                           -- * Axes
                                           , AX.Axis
                                           , AxisType(..),AxisSide(..),AxisPosn(..)
                                           , clearAxes
                                           , clearAxis
                                           , addAxis
                                           , withAxis
                                           -- * Legend
                                           , L.Legend
                                           , LegendBorder
                                           , L.LegendLocation(..), L.LegendOrientation(..)
                                           , clearLegend
                                           , setLegend
                                           , withLegendFormat
                                            -- ** Formatting
                                            , Tick(..), TickValues(..), GridLines
                                           , TickFormat(..)
                                           , AX.setTicks
                                           , AX.setGridlines
                                           , AX.setTickLabelFormat
                                           , AX.setTickLabels
                                           , AX.withTickLabelFormat
                                           , AX.withAxisLabel
                                           , AX.withAxisLine
                                           , AX.withGridLine
                                           ) where

-----------------------------------------------------------------------------

--import Data.Eq.Unicode
--import Data.Bool.Unicode
--import Data.Ord.Unicode

--import Data.Packed.Vector
--import Data.Packed.Matrix
import Numeric.Container

--import Numeric.LinearAlgebra(eps)

import qualified Data.Array.IArray as A

import Control.Monad.State
import Control.Monad.Reader
--import Control.Monad.Supply

import Prelude hiding(min,max)
import qualified Prelude as Prelude

import Graphics.Rendering.Plot.Types
import Graphics.Rendering.Plot.Defaults
import qualified Graphics.Rendering.Plot.Figure.Text as T
import qualified Graphics.Rendering.Plot.Figure.Plot.Data as D
import qualified Graphics.Rendering.Plot.Figure.Plot.Axis as AX
import qualified Graphics.Rendering.Plot.Figure.Plot.Legend as L
import qualified Graphics.Rendering.Plot.Figure.Plot.Annotation as AN

-----------------------------------------------------------------------------

-- | whether to draw a boundary around the plot area
setBorder :: Border -> Plot ()
setBorder b = modify $ \s -> s { _border = b }

-- | set the padding of the subplot
setPlotPadding :: Double -> Double -> Double -> Double -> Plot ()
setPlotPadding l r b t = modify $ \s -> s { _plot_pads = Padding l r b t }

-- | set the heading of the subplot
withHeading :: Text () -> Plot ()
withHeading m = do
                o <- asks _textoptions
                modify $ \s -> s { _heading = execText m o (_heading s) }

-----------------------------------------------------------------------------

-- | set the axis range
setRange :: AxisType -> AxisSide -> Scale -> Double -> Double -> Plot ()
setRange XAxis sd sc min max = modify $ \s -> s { _ranges = setXRanges' (_ranges s) }
    where setXRanges' r
              | sc == Log && min <= 0 = error "non-positive logarithmic range"
              | otherwise          = setXRanges sd r
          setXRanges Lower (Ranges (Left _) yr)       = Ranges (Left (Range sc min max)) yr
          setXRanges Lower (Ranges (Right (_,xr)) yr) = Ranges (Right ((Range sc min max,xr))) yr
          setXRanges Upper (Ranges (Left xr) yr)      = Ranges (Right (xr,Range sc min max)) yr
          setXRanges Upper (Ranges (Right (_,xr)) yr) = Ranges (Right (Range sc min max,xr)) yr
setRange YAxis sd sc min max = modify $ \s -> s { _ranges = setYRanges' (_ranges s) }
    where setYRanges' r
              | sc == Log && min <= 0 = error "non-positive logarithmic range"
              | otherwise          = setYRanges sd r
          setYRanges Lower (Ranges xr (Left _))       = Ranges xr (Left (Range sc min max))
          setYRanges Lower (Ranges xr (Right (_,yr))) = Ranges xr (Right ((Range sc min max,yr)))
          setYRanges Upper (Ranges xr (Left yr))      = Ranges xr (Right (yr,Range sc min max))
          setYRanges Upper (Ranges xr (Right (_,yr))) = Ranges xr (Right ((Range sc min max,yr)))

-- | set the axis ranges to values based on dataset
setRangeFromData :: AxisType -> AxisSide -> Scale -> Plot ()
setRangeFromData ax sd sc = do
  ds <- gets _data
  let ((xmin,xmax),(ymin,ymax)) = calculateRanges ds
  case ax of
    XAxis -> setRange ax sd sc (if sc == Log then if xmin == 0 then 1 else xmin else xmin) xmax
    YAxis -> setRange ax sd sc (if sc == Log then if ymin == 0 then 1 else ymin else ymin) ymax
                    
-----------------------------------------------------------------------------

withAnnotations :: Annote () -> Plot ()
withAnnotations = annoteInPlot

-----------------------------------------------------------------------------

-- | clear the axes of a subplot
clearAxes :: Plot ()
clearAxes = modify $ \s -> s { _axes = [] }

-- | clear an axis of a subplot
clearAxis :: AxisType -> AxisPosn -> Plot ()
clearAxis at axp = do
                   ax <- gets _axes
                   modify $ \s -> s { _axes = filter (\(Axis at' axp' _ _ _ _ _ _) -> not (at == at' && axp == axp')) ax } 

-- | add an axis to the subplot
addAxis :: AxisType -> AxisPosn -> AX.Axis () -> Plot ()
addAxis at axp m  = do
                    ax' <- gets _axes
                    o <- ask
                    let ax = execAxis m o (defaultAxis at axp)
                    modify $ \s -> s { _axes = ax : ax' }

-- | operate on the given axis
withAxis :: AxisType -> AxisPosn -> AX.Axis () -> Plot ()
withAxis at axp m = do
                    axes' <- gets _axes
                    o <- ask
                    modify $ \s -> s { _axes = map (\a@(Axis at' ap' _ _ _ _ _ _) 
                                                    -> if at == at' && axp == ap' then execAxis m o a else a) axes' }

-----------------------------------------------------------------------------

-- | clear the legend
clearLegend :: Plot ()
clearLegend = withLegend $ L.clearLegend

-- | set the legend location and orientation
setLegend :: L.LegendBorder -> L.LegendLocation -> L.LegendOrientation -> Plot()
setLegend b l o = withLegend $ L.setLegend b l o

-- | format the legend text
withLegendFormat :: T.Text () -> Plot ()
withLegendFormat f = withLegend $ L.withLegendFormat f

-- | operate on the legend
withLegend :: L.Legend () -> Plot ()
withLegend = legendInPlot

-----------------------------------------------------------------------------

-- | operate on the data
withData :: D.Data () -> Plot ()
withData = dataInPlot

{- | set the data series of the subplot

   The data series are either 'FormattedSeries' or plain data series.
   A plain data series must carry a 'SeriesType'.

  A dataset may or may not have an abscissa series, and if so, it is paired
  with either a list of ordinate series or a single ordinate series.

  The abscissa series (if present) is of type 'Vector Double'.

  An ordinate series be a function (@Double -> Double@) or a series of points,
  a 'Vector Double' with optional error series, y axis preference, and labels.

  To specify decoration options for an ordinate series, use the appropriate function, such
  as 'linespoints', with the ordinate series and decoration formatting ('LineFormat',
  'PointFormat', and 'BarFormat') as arguments.

> setDataset (ts,[linespoints (xs,(le,ue),Upper,"data") (([Dash,Dash],3,blue),(Diamond,green))])

  has abscissa @ts@ paired with a list of ordinate series, the single element of which is a
  'FormattedSeries', @linespoints@ where the ordinate is @xs@ with error series @le@ and @ue@,
  to be graphed against the upper y-range with label \"data\".  The line element is formatted
  to be dashed, of width 3, and blue and the point element is to be a green diamond.
-}
setDataset :: D.Dataset a => a -> Plot ()
setDataset d = withData $ D.setDataSeries d

-- | set the plot type of a given data series
setSeriesType :: Int -> SeriesType -> Plot ()
setSeriesType i t = withData $ D.setSeriesType t i
                       
-- | change the plot type of all data series
setAllSeriesTypes :: SeriesType -> Plot ()
setAllSeriesTypes t = withData $ D.setAllSeriesTypes t

-- | format the plot elements of a given series
withSeriesFormat :: D.PlotFormats m => Int -> m () -> Plot ()
withSeriesFormat i f = withData $ D.withSeriesFormat i f

{- |
  format the plot elements of all series

      the operation to modify the formats is passed the series index.
      This allows, for example, colours to be selected from a list
      that gets indexed by the argument
 
> setColour = withAllSeriesFormats (\i -> do
>                                         setLineColour $ [black,blue,red,green,yellow] !! i
>                                         setLineWidth 1.0)
-}
withAllSeriesFormats :: D.PlotFormats m => (Int -> m ()) -> Plot ()
withAllSeriesFormats f = withData $ D.withAllSeriesFormats f

-----------------------------------------------------------------------------

findMinMax :: Abscissae -> Ordinates -> (Double,Double)
findMinMax AbsFunction (OrdFunction _ f _) = let v = mapVector f (linspace 100 (-1,1))
                                           in (minElement v,maxElement v)
findMinMax (AbsPoints _ x) (OrdFunction _ f _) = let v = mapVector f x
                                             in (minElement v,maxElement v)
                                           -- what if errors go beyond plot?
findMinMax _ (OrdPoints _ (Plain o) _)    = (minElement o,maxElement o)
findMinMax _ (OrdPoints _ (Error o _) _)  = (minElement o,maxElement o)
findMinMax _ (OrdPoints _ (MinMax (o,p) _) _) = (Prelude.min (minElement o) (minElement p)
                                                ,Prelude.max (maxElement o) (maxElement p))

abscMinMax :: Abscissae -> (Double,Double)
abscMinMax AbsFunction        = defaultXAxisSideLowerRange
abscMinMax (AbsPoints _ x)      = (minElement x,maxElement x)


ordDim :: Ordinates -> Int
ordDim (OrdFunction _ _ _)  = 1
ordDim (OrdPoints _ o _)    = dim $ getOrdData o


calculateRanges :: DataSeries -> ((Double,Double),(Double,Double))
calculateRanges (DS_Y ys)      = let xmax = maximum $ map (\(DecSeries o _) -> fromIntegral $ ordDim o) $ A.elems ys
                                     ym = unzip $ map (\(DecSeries o _) -> findMinMax AbsFunction o) $ A.elems ys
                                     ymm = (minimum $ fst ym,maximum $ snd ym)
                                 in ((0,xmax),ymm)
calculateRanges (DS_1toN x ys) = let ym = unzip $ map (\(DecSeries o _) -> findMinMax x o) $ A.elems ys
                                     ymm = (minimum $ fst ym,maximum $ snd ym)
                                     xmm = abscMinMax x
                                 in (xmm,ymm)
calculateRanges (DS_1to1 ys)   = let (xm',ym') = unzip $ A.elems ys
                                     ym = unzip $ map (\(x,(DecSeries o _)) -> findMinMax x o) (zip xm' ym')
                                     ymm = (minimum $ fst ym,maximum $ snd ym)
                                     xm = unzip $ map abscMinMax xm'
                                     xmm = (minimum $ fst xm,maximum $ snd xm) 
                                 in (xmm,ymm)
calculateRanges (DS_Surf m)     = ((0,fromIntegral $ cols m),(fromIntegral $ rows m,0))

-----------------------------------------------------------------------------
                          
