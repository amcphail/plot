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
                                           , PlotType(..)
                                           -- * Plot elements 
                                           , Border
                                           , setBorder
                                           , setPlotPadding
                                           , withHeading
                                           -- * Series data
                                           , D.Abscissa(), D.Ordinate(), D.Dataset()
                                           , D.FormattedSeries()
                                           , D.line, D.point, D.linepoint
                                           , D.impulse, D.step
                                           , D.area
                                           , setDataset
                                           -- ** Plot type
                                           , setSeriesType
                                           , setAllSeriesTypes
                                           -- ** Formatting
                                           , D.PlotFormats(..)
                                           , withSeriesFormat
                                           , withAllSeriesFormats
                                           -- * Range
                                           , setRange
                                           , setRangeFromData
                                           -- * Axes
                                           , AX.Axis
                                           , AxisType(..),AxisSide(..),AxisPosn(..)
--                                           , clearAxes
                                           , addAxis
--                                           , withAxis
                                           -- * Legend
                                           , L.Legend
                                           , L.LegendBorder
                                           , L.LegendLocation(..), L.LegendOrientation(..)
                                           , setLegend
                                           , withLegendFormat
                                            -- ** Formatting
                                            , Tick(..), TickValues, GridLines
                                           , AX.setTicks
                                           , AX.setTickLabelFormat
                                           , AX.withAxisLabel
                                           , AX.withAxisLine
                                           ) where

-----------------------------------------------------------------------------

import Data.Packed.Vector
import Numeric.Vector

import qualified Data.Array.IArray as A

import Control.Monad.State
import Control.Monad.Reader
--import Control.Monad.Supply

import Prelude hiding(min,max)

import Graphics.Rendering.Plot.Types
import Graphics.Rendering.Plot.Defaults
import qualified Graphics.Rendering.Plot.Figure.Text as T
import qualified Graphics.Rendering.Plot.Figure.Plot.Data as D
import qualified Graphics.Rendering.Plot.Figure.Plot.Axis as AX
import qualified Graphics.Rendering.Plot.Figure.Plot.Legend as L

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
setRange :: AxisType -> AxisSide -> Double -> Double -> Plot ()
setRange XAxis sd min max = modify $ \s -> s { _ranges = setXRanges sd min max (_ranges s) }
    where setXRanges Lower min' max' (Ranges (Left _) yr)       = Ranges (Left (Range min' max')) yr
          setXRanges Lower min' max' (Ranges (Right (_,xr)) yr) = Ranges (Right ((Range min' max',xr))) yr
          setXRanges Upper min' max' (Ranges (Left xr) yr)      = Ranges (Right (xr,Range min' max')) yr
          setXRanges Upper min' max' (Ranges (Right (_,xr)) yr) = Ranges (Right (Range min' max',xr)) yr
setRange YAxis sd min max = modify $ \s -> s { _ranges = setYRanges sd min max (_ranges s) }
    where setYRanges Lower min' max' (Ranges xr (Left _))       = Ranges xr (Left (Range min' max'))
          setYRanges Lower min' max' (Ranges xr (Right (_,yr))) = Ranges xr (Right ((Range min' max',yr)))
          setYRanges Upper min' max' (Ranges xr (Left yr))      = Ranges xr (Right (yr,Range min' max'))
          setYRanges Upper min' max' (Ranges xr (Right (_,yr))) = Ranges xr (Right ((Range min' max',yr)))

-- | set the axis ranges to values based on dataset
setRangeFromData :: AxisType -> AxisSide -> Plot ()
setRangeFromData ax sd = do
                         ds <- gets _data
                         let ((xmin,xmax),(ymin,ymax)) = calculateRanges ds
                         case ax of
                                 XAxis -> setRange ax sd xmin xmax
                                 YAxis -> setRange ax sd ymin ymax
                    
-----------------------------------------------------------------------------

-- | clear the axes of a subplot
clearAxes :: Plot ()
clearAxes = modify $ \s -> s { _axes = [] }

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
                    modify $ \s -> s { _axes = map (\a@(Axis at' ap' _ _ _ _ _) 
                                                    -> if at == at' && axp == ap' then execAxis m o a else a) axes' }

-----------------------------------------------------------------------------

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

-- | set the type of the subplot
setPlotType :: PlotType -> Plot ()
setPlotType pt = modify $ \s -> s { _type = pt }

-----------------------------------------------------------------------------

-- | operate on the data
withData :: D.Data () -> Plot ()
withData = dataInPlot

-- | set the data series of the subplot
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
 
> setColour i = withAllSeriesFormats (\i -> do
>                                           setLineColour $ [black,blue,red,green,yellow] !! i
>                                           setLineWidth 1.0
-}
withAllSeriesFormats :: D.PlotFormats m => (Int -> m ()) -> Plot ()
withAllSeriesFormats f = withData $ D.withAllSeriesFormats f

-----------------------------------------------------------------------------


-----------------------------------------------------------------------------

findMinMax :: Abscissae -> Ordinates -> (Double,Double)
findMinMax AbsFunction (OrdFunction _ f _) = let v = mapVector f (linspace 100 (-1,1))
                                           in (minElement v,maxElement v)
findMinMax (AbsPoints x) (OrdFunction _ f _) = let v = mapVector f x
                                             in (minElement v,maxElement v)
                                           -- what if errors go beyond plot?
findMinMax _ (OrdPoints _ y _)    = let o = getOrdData y
                                  in (minElement o,maxElement o)

abscMinMax :: Abscissae -> (Double,Double)
abscMinMax AbsFunction        = (-1,1)
abscMinMax (AbsPoints x)      = (minElement x,maxElement x)


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
                                     xmm = (minimum $ fst xm,maximum $ snd ym) 
                                 in (xmm,ymm)

-----------------------------------------------------------------------------
                          
