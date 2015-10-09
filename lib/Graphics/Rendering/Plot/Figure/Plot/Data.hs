{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot.Figure.Plot.Data
-- Copyright   :  (c) A. V. H. McPhail 2010
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- 'Data' operations
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Plot.Figure.Plot.Data (
                                                 Data
                                                 -- * Series data
                                                , FormattedSeries()
                                                , line, point, linepoint
                                                , impulse, step
                                                , area
                                                , bar
                                                , hist
                                                , candle, whisker
                                                , setDataSeries
                                                -- * Plot type
                                                , setSeriesType
                                                , setAllSeriesTypes
                                                -- * Formatting
                                                , PlotFormats(..)
                                                , withSeriesFormat
                                                , withAllSeriesFormats
                                                -- * Internal
                                                , Abscissa(), Ordinate(), Dataset()
                                                ) where

-----------------------------------------------------------------------------

import Numeric.LinearAlgebra.Data hiding (format,step)
import Numeric.LinearAlgebra.Devel

import Data.Maybe

import qualified Data.Array.IArray as A

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Supply
import Control.Monad.Trans.Maybe

import Graphics.Rendering.Plot.Types
import Graphics.Rendering.Plot.Figure.Line
import Graphics.Rendering.Plot.Figure.Point
import Graphics.Rendering.Plot.Figure.Bar

-----------------------------------------------------------------------------

dataSeriesNum :: DataSeries -> Int
dataSeriesNum (DS_Y a)      = A.rangeSize $ A.bounds $ a
dataSeriesNum (DS_1toN _ a) = A.rangeSize $ A.bounds $ a
dataSeriesNum (DS_1to1 a)   = A.rangeSize $ A.bounds $ a
dataSeriesNum (DS_Surf _)   = 1

-----------------------------------------------------------------------------

class SeriesTypes a where
    setSeriesType'' :: SeriesType -> a -> Data a

instance SeriesTypes Decoration where
    setSeriesType'' Line      d@(DecLine _)    = return d
    setSeriesType'' Line      (DecPoint pt)    = do
                                                 let c = getPointColour pt
                                                 lt <- toLine c
                                                 return $ DecLine lt
    setSeriesType'' Line      (DecLinPt lt _)  = return $ DecLine lt
    setSeriesType'' Line      (DecImpulse lt)  = return $ DecLine lt
    setSeriesType'' Line      (DecStep lt)     = return $ DecLine lt
    setSeriesType'' Line      (DecArea lt)     = return $ DecLine lt
    setSeriesType'' Line      (DecBar bt)      = do
                                                 let c = getBarColour bt
                                                 lt <- toLine c
                                                 return $ DecLine lt
    setSeriesType'' Line      (DecHist bt)     = do
                                                 let c = getBarColour bt
                                                 lt <- toLine c
                                                 return $ DecLine lt
    setSeriesType'' Line      (DecCand bt)     = do
                                                 let c = getBarColour bt
                                                 lt <- toLine c
                                                 return $ DecLine lt
    setSeriesType'' Line      (DecWhisk bt)    = do
                                                 let c = getBarColour bt
                                                 lt <- toLine c
                                                 return $ DecLine lt
    setSeriesType'' Point     (DecLine lt)     = do
                                                 let c = fromJust $ getLineColour lt
                                                 g <- supply
                                                 pt <- toPoint (g :: Glyph,c)
                                                 return $ DecPoint pt
    setSeriesType'' Point     d@(DecPoint _)   = return d
    setSeriesType'' Point     (DecLinPt _ pt)  = return $ DecPoint pt
    setSeriesType'' Point     (DecImpulse lt)  = do
                                                 let c = fromJust $ getLineColour lt
                                                 g <- supply
                                                 pt <- toPoint (g :: Glyph,c)
                                                 return $ DecPoint pt
    setSeriesType'' Point     (DecStep lt)     = do
                                                 let c = fromJust $ getLineColour lt
                                                 g <- supply
                                                 pt <- toPoint (g :: Glyph,c)
                                                 return $ DecPoint pt
    setSeriesType'' Point     (DecArea lt)     = do
                                                 let c = fromJust $ getLineColour lt
                                                 g <- supply
                                                 pt <- toPoint (g :: Glyph,c)
                                                 return $ DecPoint pt
    setSeriesType'' Point     (DecBar bt)      = do
                                                 let c = getBarColour bt
                                                 g <- supply
                                                 pt <- toPoint (g :: Glyph,c)
                                                 return $ DecPoint pt
    setSeriesType'' Point     (DecHist bt)     = do
                                                 let c = getBarColour bt
                                                 g <- supply
                                                 pt <- toPoint (g :: Glyph,c)
                                                 return $ DecPoint pt
    setSeriesType'' Point     (DecCand bt)     = do
                                                 let c = getBarColour bt
                                                 g <- supply
                                                 pt <- toPoint (g :: Glyph,c)
                                                 return $ DecPoint pt
    setSeriesType'' Point     (DecWhisk bt)    = do
                                                 let c = getBarColour bt
                                                 g <- supply
                                                 pt <- toPoint (g :: Glyph,c)
                                                 return $ DecPoint pt
    setSeriesType'' LinePoint (DecLine lt)     = do
                                                 let c = fromJust $ getLineColour lt
                                                 g <- supply
                                                 pt <- toPoint (g :: Glyph,c)
                                                 return $ DecLinPt lt pt
    setSeriesType'' LinePoint (DecPoint pt)    = do
                                                 let c = getPointColour pt
                                                 lt <- toLine (c :: Color)
                                                 return $ DecLinPt lt pt
    setSeriesType'' LinePoint d@(DecLinPt _ _) = return d
    setSeriesType'' LinePoint (DecImpulse lt)  = do
                                                 let c = fromJust $ getLineColour lt
                                                 g <- supply
                                                 pt <- toPoint (g :: Glyph,c)
                                                 return $ DecLinPt lt pt
    setSeriesType'' LinePoint (DecStep lt)     = do
                                                 let c = fromJust $ getLineColour lt
                                                 g <- supply
                                                 pt <- toPoint (g :: Glyph,c)
                                                 return $ DecLinPt lt pt
    setSeriesType'' LinePoint (DecArea lt)     = do
                                                 let c = fromJust $ getLineColour lt
                                                 g <- supply
                                                 pt <- toPoint (g :: Glyph,c)
                                                 return $ DecLinPt lt pt
    setSeriesType'' LinePoint (DecBar bt)      = do
                                                 let c = getBarColour bt
                                                 lt <- toLine c
                                                 g <- supply
                                                 pt <- toPoint (g :: Glyph,c)
                                                 return $ DecLinPt lt pt
    setSeriesType'' LinePoint (DecHist bt)     = do
                                                 let c = getBarColour bt
                                                 lt <- toLine c
                                                 g <- supply
                                                 pt <- toPoint (g :: Glyph,c)
                                                 return $ DecLinPt lt pt
    setSeriesType'' LinePoint (DecCand bt)     = do
                                                 let c = getBarColour bt
                                                 lt <- toLine c
                                                 g <- supply
                                                 pt <- toPoint (g :: Glyph,c)
                                                 return $ DecLinPt lt pt
    setSeriesType'' LinePoint (DecWhisk bt)    = do
                                                 let c = getBarColour bt
                                                 lt <- toLine c
                                                 g <- supply
                                                 pt <- toPoint (g :: Glyph,c)
                                                 return $ DecLinPt lt pt
    setSeriesType'' Impulse   (DecLine lt)     = return $ DecImpulse lt
    setSeriesType'' Impulse   (DecPoint pt)    = do
                                                 let c = getPointColour pt
                                                 lt <- toLine c
                                                 return $ DecImpulse lt
    setSeriesType'' Impulse   (DecLinPt lt _)  = return $ DecImpulse lt
    setSeriesType'' Impulse   d@(DecImpulse _) = return d
    setSeriesType'' Impulse   (DecStep lt)     = return $ DecImpulse lt
    setSeriesType'' Impulse   (DecArea lt)     = return $ DecImpulse lt
    setSeriesType'' Impulse   (DecBar bt)      = do
                                                 let c = getBarColour bt
                                                 lt <- toLine c
                                                 return $ DecImpulse lt
    setSeriesType'' Impulse   (DecHist bt)     = do
                                                 let c = getBarColour bt
                                                 lt <- toLine c
                                                 return $ DecImpulse lt
    setSeriesType'' Impulse   (DecCand bt)     = do
                                                 let c = getBarColour bt
                                                 lt <- toLine c
                                                 return $ DecImpulse lt
    setSeriesType'' Impulse   (DecWhisk bt)    = do
                                                 let c = getBarColour bt
                                                 lt <- toLine c
                                                 return $ DecImpulse lt
    setSeriesType'' Step      (DecLine lt)     = return $ DecStep lt
    setSeriesType'' Step      (DecPoint pt)    = do
                                                 let c = getPointColour pt
                                                 lt <- toLine c
                                                 return $ DecStep lt
    setSeriesType'' Step      (DecLinPt lt _)  = return $ DecStep lt
    setSeriesType'' Step      (DecImpulse lt)  = return $ DecStep lt 
    setSeriesType'' Step      d@(DecStep _)    = return d
    setSeriesType'' Step      (DecArea lt)     = return $ DecStep lt 
    setSeriesType'' Step      (DecBar bt)      = do
                                                 let c = getBarColour bt
                                                 lt <- toLine c
                                                 return $ DecStep lt
    setSeriesType'' Step      (DecHist bt)     = do
                                                 let c = getBarColour bt
                                                 lt <- toLine c
                                                 return $ DecStep lt
    setSeriesType'' Step      (DecCand bt)     = do
                                                 let c = getBarColour bt
                                                 lt <- toLine c
                                                 return $ DecStep lt
    setSeriesType'' Step      (DecWhisk bt)    = do
                                                 let c = getBarColour bt
                                                 lt <- toLine c
                                                 return $ DecStep lt
    setSeriesType'' Area      (DecLine lt)     = return $ DecArea lt
    setSeriesType'' Area      (DecPoint pt)    = do
                                                 let c = getPointColour pt
                                                 lt <- toLine c
                                                 return $ DecArea lt
    setSeriesType'' Area      (DecLinPt lt _)  = return $ DecArea lt
    setSeriesType'' Area      (DecImpulse lt)  = return $ DecArea lt 
    setSeriesType'' Area      (DecStep lt)     = return $ DecArea lt 
    setSeriesType'' Area      d@(DecArea _)    = return d
    setSeriesType'' Area      (DecBar bt)      = do
                                                 let c = getBarColour bt
                                                 lt <- toLine c
                                                 return $ DecArea lt
    setSeriesType'' Area      (DecHist bt)     = do
                                                 let c = getBarColour bt
                                                 lt <- toLine c
                                                 return $ DecArea lt
    setSeriesType'' Area      (DecCand bt)     = do
                                                 let c = getBarColour bt
                                                 lt <- toLine c
                                                 return $ DecArea lt
    setSeriesType'' Area      (DecWhisk bt)    = do
                                                 let c = getBarColour bt
                                                 lt <- toLine c
                                                 return $ DecArea lt
    setSeriesType'' Bar      (DecLine lt)      = do
                                                 let c = fromJust $ getLineColour lt
                                                 bt <- toBar c
                                                 return $ DecBar bt
    setSeriesType'' Bar      (DecPoint pt)     = do
                                                 let c = getPointColour pt
                                                 bt <- toBar c
                                                 return $ DecBar bt
    setSeriesType'' Bar      (DecLinPt lt _)   = do
                                                 let c = fromJust $ getLineColour lt
                                                 bt <- toBar c
                                                 return $ DecBar bt
    setSeriesType'' Bar      (DecImpulse lt)   = do
                                                 let c = fromJust $ getLineColour lt
                                                 bt <- toBar c
                                                 return $ DecBar bt
    setSeriesType'' Bar      (DecStep lt)      = do
                                                 let c = fromJust $ getLineColour lt
                                                 bt <- toBar c
                                                 return $ DecBar bt
    setSeriesType'' Bar      (DecArea lt)      = do
                                                 let c = fromJust $ getLineColour lt
                                                 bt <- toBar c
                                                 return $ DecBar bt
    setSeriesType'' Bar      d@(DecBar _)      = return d
    setSeriesType'' Bar      (DecHist bt)      = return $ DecBar bt
    setSeriesType'' Bar      (DecCand bt)      = return $ DecBar bt
    setSeriesType'' Bar      (DecWhisk bt)     = return $ DecBar bt
    setSeriesType'' Hist     (DecLine lt)      = do
                                                 let c = fromJust $ getLineColour lt
                                                 bt <- toBar c
                                                 return $ DecHist bt
    setSeriesType'' Hist     (DecPoint pt)     = do
                                                 let c = getPointColour pt
                                                 bt <- toBar c
                                                 return $ DecHist bt
    setSeriesType'' Hist     (DecLinPt lt _)   = do
                                                 let c = fromJust $ getLineColour lt
                                                 bt <- toBar c
                                                 return $ DecHist bt
    setSeriesType'' Hist     (DecImpulse lt)   = do
                                                 let c = fromJust $ getLineColour lt
                                                 bt <- toBar c
                                                 return $ DecHist bt
    setSeriesType'' Hist     (DecStep lt)      = do
                                                 let c = fromJust $ getLineColour lt
                                                 bt <- toBar c
                                                 return $ DecHist bt
    setSeriesType'' Hist     (DecArea lt)      = do
                                                 let c = fromJust $ getLineColour lt
                                                 bt <- toBar c
                                                 return $ DecHist bt
    setSeriesType'' Hist     (DecBar bt)       = return $ DecHist bt
    setSeriesType'' Hist     d@(DecHist _)     = return d
    setSeriesType'' Hist     (DecCand bt)      = return $ DecHist bt
    setSeriesType'' Hist     (DecWhisk bt)     = return $ DecHist bt
    setSeriesType'' Candle  (DecLine lt)      = do
                                                 let c = fromJust $ getLineColour lt
                                                 bt <- toBar c
                                                 return $ DecCand bt
    setSeriesType'' Candle  (DecPoint pt)     = do
                                                 let c = getPointColour pt
                                                 bt <- toBar c
                                                 return $ DecCand bt
    setSeriesType'' Candle  (DecLinPt lt _)   = do
                                                 let c = fromJust $ getLineColour lt
                                                 bt <- toBar c
                                                 return $ DecCand bt
    setSeriesType'' Candle  (DecImpulse lt)   = do
                                                 let c = fromJust $ getLineColour lt
                                                 bt <- toBar c
                                                 return $ DecCand bt
    setSeriesType'' Candle  (DecStep lt)      = do
                                                 let c = fromJust $ getLineColour lt
                                                 bt <- toBar c
                                                 return $ DecCand bt
    setSeriesType'' Candle  (DecArea lt)      = do
                                                 let c = fromJust $ getLineColour lt
                                                 bt <- toBar c
                                                 return $ DecCand bt
    setSeriesType'' Candle  (DecBar bt)       = return $ DecCand bt
    setSeriesType'' Candle  (DecHist bt)      = return $ DecCand bt
    setSeriesType'' Candle  d@(DecCand _)     = return d
    setSeriesType'' Candle  (DecWhisk bt)     = return $ DecCand bt
    setSeriesType'' Whisker  (DecLine lt)      = do
                                                 let c = fromJust $ getLineColour lt
                                                 bt <- toBar c
                                                 return $ DecWhisk bt
    setSeriesType'' Whisker  (DecPoint pt)     = do
                                                 let c = getPointColour pt
                                                 bt <- toBar c
                                                 return $ DecWhisk bt
    setSeriesType'' Whisker  (DecLinPt lt _)   = do
                                                 let c = fromJust $ getLineColour lt
                                                 bt <- toBar c
                                                 return $ DecWhisk bt
    setSeriesType'' Whisker  (DecImpulse lt)   = do
                                                 let c = fromJust $ getLineColour lt
                                                 bt <- toBar c
                                                 return $ DecWhisk bt
    setSeriesType'' Whisker  (DecStep lt)      = do
                                                 let c = fromJust $ getLineColour lt
                                                 bt <- toBar c
                                                 return $ DecWhisk bt
    setSeriesType'' Whisker  (DecArea lt)      = do
                                                 let c = fromJust $ getLineColour lt
                                                 bt <- toBar c
                                                 return $ DecWhisk bt
    setSeriesType'' Whisker  (DecBar bt)       = return $ DecWhisk bt
    setSeriesType'' Whisker  (DecHist bt)      = return $ DecWhisk bt
    setSeriesType'' Whisker  (DecCand bt)      = return $ DecWhisk bt
    setSeriesType'' Whisker  d@(DecWhisk _)    = return d

instance SeriesTypes DecoratedSeries where
    setSeriesType'' t (DecSeries o d) = do
                                       d' <- setSeriesType'' t d
                                       return $ DecSeries o d'

setSeriesType' :: Int -> SeriesType -> DataSeries -> Data DataSeries
setSeriesType' i t (DS_Y a)      = do
                                      s' <- setSeriesType'' t $ a A.! i
                                      return $ DS_Y $ a A.// [(i,s')] 
setSeriesType' i t (DS_1toN x a) = do
                                      s' <- setSeriesType'' t $ a A.! i
                                      return $ DS_1toN x $ a A.// [(i,s')] 
setSeriesType' i t (DS_1to1 a)   = do
                                      let (x,s) = a A.! i
                                      s' <- setSeriesType'' t s
                                      return $ DS_1to1 $ a A.// [(i,(x,s'))] 
setSeriesType' _ _ d@(DS_Surf _) = return d
 
-- | set the series type of a given data series
setSeriesType :: SeriesType -> Int -> Data ()
setSeriesType t i = do
                       ds <- get
                       ds' <- setSeriesType' i t ds
                       put ds'
                       
-- | set the series type of all data series
setAllSeriesTypes :: SeriesType -> Data ()
setAllSeriesTypes t = do
                         ds <- get
                         let ln = dataSeriesNum ds
                         mapM_ (setSeriesType t) [1..ln]

-----------------------------------------------------------------------------

class PlotFormats m where
    modifyFormat :: m () -> DecoratedSeries -> Data DecoratedSeries

instance PlotFormats Line where
    modifyFormat l (DecSeries o (DecLine lt))     = do
                                                    lo <- asks _lineoptions
                                                    let lt' = execLine l lo lt
                                                    return $ DecSeries o (DecLine lt')
    modifyFormat _ d@(DecSeries _ (DecPoint _))   = return d
    modifyFormat l (DecSeries o (DecLinPt lt pt)) = do
                                                    lo <- asks _lineoptions
                                                    let lt' = execLine l lo lt
                                                    return $ DecSeries o (DecLinPt lt' pt)
    modifyFormat l (DecSeries o (DecImpulse lt))  = do
                                                    lo <- asks _lineoptions
                                                    let lt' = execLine l lo lt
                                                    return $ DecSeries o (DecImpulse lt')
    modifyFormat l (DecSeries o (DecStep lt))     = do
                                                    lo <- asks _lineoptions
                                                    let lt' = execLine l lo lt
                                                    return $ DecSeries o (DecStep lt')
    modifyFormat l (DecSeries o (DecArea lt))     = do
                                                    lo <- asks _lineoptions
                                                    let lt' = execLine l lo lt
                                                    return $ DecSeries o (DecArea lt')
    modifyFormat _ d@(DecSeries _ (DecBar _))     = return d
    modifyFormat _ d@(DecSeries _ (DecHist _))    = return d
    modifyFormat _ d@(DecSeries _ (DecCand _))    = return d
    modifyFormat _ d@(DecSeries _ (DecWhisk _))   = return d

instance PlotFormats Point where
    modifyFormat _ d@(DecSeries _ (DecLine _))    = return d
    modifyFormat _ d@(DecSeries _ (DecImpulse _)) = return d
    modifyFormat _ d@(DecSeries _ (DecStep _))    = return d
    modifyFormat _ d@(DecSeries _ (DecArea _))    = return d
    modifyFormat p (DecSeries o (DecPoint pt))    = do
                                                    po <- asks _pointoptions
                                                    let pt' = execPoint p po pt
                                                    return $ DecSeries o (DecPoint pt')
    modifyFormat p (DecSeries o (DecLinPt lt pt)) = do
                                                    po <- asks _pointoptions
                                                    let pt' = execPoint p po pt
                                                    return $ DecSeries o (DecLinPt lt pt')
    modifyFormat _ d@(DecSeries _ (DecBar _))     = return d
    modifyFormat _ d@(DecSeries _ (DecHist _))    = return d
    modifyFormat _ d@(DecSeries _ (DecCand _))    = return d
    modifyFormat _ d@(DecSeries _ (DecWhisk _))   = return d

instance PlotFormats Bar where
    modifyFormat _ d@(DecSeries _ (DecLine _))    = return d
    modifyFormat _ d@(DecSeries _ (DecImpulse _)) = return d
    modifyFormat _ d@(DecSeries _ (DecStep _))    = return d
    modifyFormat _ d@(DecSeries _ (DecArea _))    = return d
    modifyFormat _ d@(DecSeries _ (DecPoint _))   = return d
    modifyFormat _ d@(DecSeries _ (DecLinPt _ _)) = return d
    modifyFormat b   (DecSeries o (DecBar bt))    = do  
                                                    bo <- asks _baroptions
                                                    let bt' = execBar b bo bt
                                                    return $ DecSeries o (DecBar bt')
    modifyFormat b   (DecSeries o (DecHist bt))   = do  
                                                    bo <- asks _baroptions
                                                    let bt' = execBar b bo bt
                                                    return $ DecSeries o (DecHist bt')
    modifyFormat b   (DecSeries o (DecCand bt))   = do  
                                                    bo <- asks _baroptions
                                                    let bt' = execBar b bo bt
                                                    return $ DecSeries o (DecCand bt')
    modifyFormat b   (DecSeries o (DecWhisk bt))  = do  
                                                    bo <- asks _baroptions
                                                    let bt' = execBar b bo bt
                                                    return $ DecSeries o (DecWhisk bt')

-- | format the plot elements of a given series
withSeriesFormat :: PlotFormats m => Int -> m () -> Data ()
withSeriesFormat i f = do
                       ds <- get
                       ds' <- case ds of
                                      (DS_Y a)      -> do
                                                       let d = a A.! i
                                                       d' <- modifyFormat f d
                                                       return $ DS_Y $ a A.// [(i,d')]
                                      (DS_1toN x a) -> do
                                                       let d = a A.! i
                                                       d' <- modifyFormat f d
                                                       return $ DS_1toN x $ a A.// [(i,d')]
                                      (DS_1to1 a)   -> do
                                                       let (x,d) = a A.! i
                                                       d' <- modifyFormat f d
                                                       return $ DS_1to1 $ a A.// [(i,(x,d'))]
                                      d@(DS_Surf _) -> return d
                       put ds'

-- | format the plot elements of all series
-- |     the operation to modify the formats is passed the series index
-- |     this allows, for example, colours to be selected from a list
-- |     that gets indexed by the argument
-- | @setColour i = setLineColour $ [black,blue,red,green,yellow] !! i@
withAllSeriesFormats :: PlotFormats m => (Int -> m ()) -> Data ()
withAllSeriesFormats f = do
                         ds <- get
                         let ln = dataSeriesNum ds
                         mapM_ (\i -> withSeriesFormat i (f i)) [1..ln]
 
-----------------------------------------------------------------------------

class Abscissa a where
    toAbscissa :: a -> Abscissae

toAbscissae :: Abscissa a => [a] -> [Abscissae]
toAbscissae = map toAbscissa

instance Abscissa Series                   where toAbscissa s         = AbsPoints (isMonotoneIncreasing s) s

class Ordinate a where
    toOrdinate :: a -> Ordinates

toOrdinates :: Ordinate a => [a] -> [Ordinates]
toOrdinates = map toOrdinate

instance Ordinate Function                           where toOrdinate f         = OrdFunction Lower f Nothing
instance Ordinate Series                             where toOrdinate s         = OrdPoints Lower (Plain s) Nothing
instance Ordinate (Series,ErrorSeries)               where toOrdinate (s,e)     = OrdPoints Lower (Error s (Left e)) Nothing
instance Ordinate (Series,(ErrorSeries,ErrorSeries)) where toOrdinate (s,(l,u)) = OrdPoints Lower (Error s (Right (l,u))) Nothing
instance Ordinate (MinMaxSeries,(ErrorSeries,ErrorSeries)) where toOrdinate (s,(l,u)) = OrdPoints Lower (MinMax s (Just (l,u))) Nothing
instance Ordinate (Function,AxisSide)                         where toOrdinate (f,ax)       = OrdFunction ax f Nothing
instance Ordinate (Series,AxisSide)                           where toOrdinate (s,ax)       = OrdPoints ax (Plain s) Nothing
instance Ordinate (Series,ErrorSeries,AxisSide)               where toOrdinate (s,e,ax)     = OrdPoints ax (Error s (Left e)) Nothing
instance Ordinate (Series,(ErrorSeries,ErrorSeries),AxisSide) where toOrdinate (s,(l,u),ax) = OrdPoints ax (Error s (Right (l,u))) Nothing
instance Ordinate (MinMaxSeries,(ErrorSeries,ErrorSeries),AxisSide) where toOrdinate (s,(l,u),ax) = OrdPoints ax (MinMax s (Just (l,u))) Nothing
instance Ordinate (Function,SeriesLabel)                         where toOrdinate (f,la)       = OrdFunction Lower f (Just la)
instance Ordinate (Series,SeriesLabel)                           where toOrdinate (s,la)       = OrdPoints Lower (Plain s) (Just la)
instance Ordinate (Series,ErrorSeries,SeriesLabel)               where toOrdinate (s,e,la)     = OrdPoints Lower (Error s (Left e)) (Just la)
instance Ordinate (Series,(ErrorSeries,ErrorSeries),SeriesLabel) where toOrdinate (s,(l,u),la) = OrdPoints Lower (Error s (Right (l,u))) (Just la)

instance Ordinate (Function,AxisSide,SeriesLabel)                          where toOrdinate (f,ax,la)       = OrdFunction ax f (Just la)
instance Ordinate (Series,AxisSide,SeriesLabel)                            where toOrdinate (s,ax,la)       = OrdPoints ax (Plain s) (Just la)
instance Ordinate (Series,ErrorSeries,AxisSide,SeriesLabel)                where toOrdinate (s,e,ax,la)     = OrdPoints ax (Error s (Left e)) (Just la)
instance Ordinate (Series,(ErrorSeries,ErrorSeries),AxisSide,SeriesLabel)  where toOrdinate (s,(l,u),ax,la) = OrdPoints ax (Error s (Right (l,u))) (Just la)
instance Ordinate (MinMaxSeries,(ErrorSeries,ErrorSeries),AxisSide,SeriesLabel) where toOrdinate (s,(l,u),ax,la) = OrdPoints ax (MinMax s (Just (l,u))) (Just la)

class Decorations a where
    toDecoration :: a -> Decoration

toDecorations :: Decorations a => [a] -> [Decoration]
toDecorations = map toDecoration

instance Decorations LineType             where toDecoration l     = DecLine l
instance Decorations PointType            where toDecoration p     = DecPoint p
instance Decorations (LineType,PointType) where toDecoration (l,p) = DecLinPt l p
instance Decorations (PointType,LineType) where toDecoration (p,l) = DecLinPt l p
instance Decorations BarType              where toDecoration b     = DecBar b
instance Decorations Decoration           where toDecoration       = id

format :: (Ordinate a, Decorations b) => a -> b -> DecoratedSeries
format o f = DecSeries (toOrdinate o) (toDecoration f)

line :: (Ordinate a, LineFormat b) => a -> b -> FormattedSeries
line o f = do
           f' <- toLine f
           return $ format o f'

point :: (Ordinate a, PointFormat b) => a -> b -> FormattedSeries
point o f = do
            f' <- toPoint f
            return $ format o f'

linepoint :: (Ordinate a, LineFormat b, PointFormat c) => a -> b -> c -> FormattedSeries
linepoint o l p = do
                  l' <- toLine l
                  p' <- toPoint p
                  return $ format o (l',p')

impulse :: (Ordinate a, LineFormat b) => a -> b -> FormattedSeries
impulse o f = do
              f' <- toLine f
              setSeriesType'' Impulse (format o f')

step :: (Ordinate a, LineFormat b) => a -> b -> FormattedSeries
step o f = do
           f' <- toLine f
           setSeriesType'' Step (format o f')
                 
area :: (Ordinate a, LineFormat b) => a -> b -> FormattedSeries
area o f = do
           f' <- toLine f
           setSeriesType'' Area (format o f')

bar :: (Ordinate a, BarFormat b) => a -> b -> FormattedSeries
bar o f = do
          f' <- toBar f
          return $ format o f'

hist :: (Ordinate a, BarFormat b) => a -> b -> FormattedSeries
hist o f = do
          f' <- toBar f
          setSeriesType'' Hist (format o f')

candle :: (Ordinate a, BarFormat b) => a -> b -> FormattedSeries
candle o f = do
          f' <- toBar f
          setSeriesType'' Candle (format o f')

whisker :: (Ordinate a, BarFormat b) => a -> b -> FormattedSeries
whisker o f = do
          f' <- toBar f
          setSeriesType'' Whisker (format o f')

-----------------------------------------------------------------------------

getType :: SeriesType -> Data Decoration
getType Line = do
               c <- supply
               lt <- toLine (c :: Color)
               return $ toDecoration lt
getType Point = do
                g <- supply
                pt <- toPoint (g :: Glyph) 
                return $ toDecoration pt
getType LinePoint = do
                    c <- supply
                    g <- supply
                    lt <- toLine (c :: Color)
                    pt <- toPoint (g :: Glyph)
                    return $ toDecoration (lt,pt)
getType Impulse = do
                  c <- supply
                  lt <- toLine (c :: Color)
                  setSeriesType'' Impulse $ toDecoration lt
getType Step = do
               c <- supply
               lt <- toLine (c :: Color)
               setSeriesType'' Impulse $ toDecoration lt
getType Area = do
               c <- supply
               lt <- toLine (c :: Color)
               setSeriesType'' Area $ toDecoration lt
getType Bar  = do
               c <- supply
               bt <- toBar (c :: Color)
               return $ toDecoration bt
getType Hist = do
               c <- supply
               bt <- toBar (c :: Color)
               setSeriesType'' Hist $ toDecoration bt
getType Candle = do
               c <- supply
               bt <- toBar (c :: Color)
               setSeriesType'' Candle $ toDecoration bt
getType Whisker = do
               c <- supply
               bt <- toBar (c :: Color)
               setSeriesType'' Whisker $ toDecoration bt

getNTypes :: Int -> SeriesType -> Data [Decoration]
getNTypes n st = mapM getType (replicate n st)

-----------------------------------------------------------------------------

class Dataset a where
    toDataSeries :: a -> Data DataSeries

instance Dataset Surface where
    toDataSeries m = return $ DS_Surf m
                              

instance (Ordinate a) => Dataset (SeriesType,[a]) where 
    toDataSeries (Line,os) = do
                             let ln = length os
                             cs <- supplyN ln
                             ls <- mapM toLine (cs :: [Color])
                             return $ DS_Y $ A.listArray (1,ln) $ zipWith format os ls
    toDataSeries (Point,os) = do
                              let ln = length os
                              cs <- supplyN ln
                              gs <- supplyN ln
                              ps <- mapM toPoint (zip (gs :: [Glyph]) (cs :: [Color]))
                              return $ DS_Y $ A.listArray (1,ln) $ zipWith format os ps
    toDataSeries (LinePoint,os) = do
                              let ln = length os
                              cs <- supplyN ln
                              gs <- supplyN ln
                              ls <- mapM toLine cs
                              ps <- mapM toPoint (zip (gs :: [Glyph]) (cs :: [Color]))
                              let ds = toDecorations (zip ls ps)
                              return $ DS_Y $ A.listArray (1,ln) $ zipWith format os ds
    toDataSeries (Impulse,os) = do
                              let ln = length os
                              cs <- supplyN ln
                              ls <- mapM toLine (cs :: [Color])
                              ds <- mapM (setSeriesType'' Impulse) $ toDecorations ls
                              return $ DS_Y $ A.listArray (1,ln) $ zipWith format os ds
    toDataSeries (Step,os) = do
                             let ln = length os
                             cs <- supplyN ln
                             ls <- mapM toLine (cs :: [Color])
                             ds <- mapM (setSeriesType'' Step) $ toDecorations ls
                             return $ DS_Y $ A.listArray (1,ln) $ zipWith format os ds
    toDataSeries (Area,os) = do
                             let ln = length os
                             cs <- supplyN ln
                             ls <- mapM toLine (cs :: [Color])
                             ds <- mapM (setSeriesType'' Area) $ toDecorations ls
                             return $ DS_Y $ A.listArray (1,ln) $ zipWith format os ds
    toDataSeries (Bar,os) = do
                            let ln = length os
                            cs <- supplyN ln
                            bs <- mapM toBar (cs :: [Color])
                            return $ DS_Y $ A.listArray (1,ln) $ zipWith format os bs
    toDataSeries (Hist,os) = do
                            let ln = length os
                            cs <- supplyN ln
                            bs <- mapM toBar (cs :: [Color])
                            ds <- mapM (setSeriesType'' Hist) $ toDecorations bs
                            return $ DS_Y $ A.listArray (1,ln) $ zipWith format os ds
    toDataSeries (Candle,os) = do
                            let ln = length os
                            cs <- supplyN ln
                            bs <- mapM toBar (cs :: [Color])
                            ds <- mapM (setSeriesType'' Candle) $ toDecorations bs
                            return $ DS_Y $ A.listArray (1,ln) $ zipWith format os ds
    toDataSeries (Whisker,os) = do
                            let ln = length os
                            cs <- supplyN ln
                            bs <- mapM toBar (cs :: [Color])
                            ds <- mapM (setSeriesType'' Whisker) $ toDecorations bs
                            return $ DS_Y $ A.listArray (1,ln) $ zipWith format os ds

instance (Abscissa a, Ordinate b) => Dataset (SeriesType,a,[b]) where
    toDataSeries (Line,t,os) = do
                               let ln = length os
                               cs <- supplyN ln
                               ls <- mapM toLine (cs :: [Color])
                               return $ DS_1toN (toAbscissa t) $ A.listArray (1,ln) 
                                        $ zipWith format os ls
    toDataSeries (Point,t,os) = do
                                let ln = length os
                                cs <- supplyN ln
                                gs <- supplyN ln
                                ps <- mapM toPoint (zip (gs :: [Glyph]) (cs :: [Color]))
                                return $ DS_1toN (toAbscissa t) $ A.listArray (1,ln) 
                                        $ zipWith format os ps
    toDataSeries (LinePoint,t,os) = do
                                let ln = length os
                                cs <- supplyN ln
                                gs <- supplyN ln
                                ls <- mapM toLine cs
                                ps <- mapM toPoint (zip (gs :: [Glyph]) (cs :: [Color]))
                                let ds = toDecorations (zip ls ps)
                                return $ DS_1toN (toAbscissa t) $ A.listArray (1,ln) 
                                        $ zipWith format os ds
    toDataSeries (Impulse,t,os) = do
                              let ln = length os
                              cs <- supplyN ln
                              ls <- mapM toLine (cs :: [Color])
                              ds <- mapM (setSeriesType'' Impulse) $ toDecorations ls
                              return $ DS_1toN (toAbscissa t) $ A.listArray (1,ln) 
                                         $ zipWith format os ds
    toDataSeries (Step,t,os) = do
                              let ln = length os
                              cs <- supplyN ln
                              ls <- mapM toLine (cs :: [Color])
                              ds <- mapM (setSeriesType'' Step) $ toDecorations ls
                              return $ DS_1toN (toAbscissa t) $ A.listArray (1,ln) 
                                         $ zipWith format os ds
    toDataSeries (Area,t,os) = do
                              let ln = length os
                              cs <- supplyN ln
                              ls <- mapM toLine (cs :: [Color])
                              ds <- mapM (setSeriesType'' Area) $ toDecorations ls
                              return $ DS_1toN (toAbscissa t) $ A.listArray (1,ln) 
                                         $ zipWith format os ds
    toDataSeries (Bar,t,os) = do
                              let ln = length os
                              cs <- supplyN ln
                              bs <- mapM toBar (cs :: [Color])
                              return $ DS_1toN (toAbscissa t) $ A.listArray (1,ln) 
                                         $ zipWith format os bs
    toDataSeries (Hist,t,os) = do
                              let ln = length os
                              cs <- supplyN ln
                              bs <- mapM toBar (cs :: [Color])
                              ds <- mapM (setSeriesType'' Hist) $ toDecorations bs
                              return $ DS_1toN (toAbscissa t) $ A.listArray (1,ln) 
                                         $ zipWith format os ds
    toDataSeries (Candle,t,os) = do
                              let ln = length os
                              cs <- supplyN ln
                              bs <- mapM toBar (cs :: [Color])
                              ds <- mapM (setSeriesType'' Candle) $ toDecorations bs
                              return $ DS_1toN (toAbscissa t) $ A.listArray (1,ln) 
                                         $ zipWith format os ds
    toDataSeries (Whisker,t,os) = do
                              let ln = length os
                              cs <- supplyN ln
                              bs <- mapM toBar (cs :: [Color])
                              ds <- mapM (setSeriesType'' Whisker) $ toDecorations bs
                              return $ DS_1toN (toAbscissa t) $ A.listArray (1,ln) 
                                         $ zipWith format os ds

instance (Abscissa a, Ordinate b) => Dataset [(SeriesType,a,b)] where
    toDataSeries prs = do
                       let ln = length prs
                           (ss,xs,ys) = unzip3 prs
                       ds <- mapM toDataSeries' $ zip ss ys
                       return $ DS_1to1 $ A.listArray (1,ln) $ zip (toAbscissae xs) ds


toDataSeries' :: Ordinate b => (SeriesType,b) -> Data DecoratedSeries
toDataSeries' (Line,o) = do
                         c <- supply 
                         l <- toLine (c :: Color)
                         return $ format o l
toDataSeries' (Point,o) = do
                          c <- supply 
                          g <- supply
                          p <- toPoint ((g :: Glyph),(c :: Color))
                          return $ format o p
toDataSeries' (LinePoint,o) = do
                          c <- supply 
                          g <- supply
                          l <- toLine (c :: Color)
                          p <- toPoint ((g :: Glyph),(c :: Color))
                          let d = toDecoration (l,p)
                          return $ format o d
toDataSeries' (Impulse,o) = do
                         c <- supply 
                         l <- toLine (c :: Color)
                         d <- setSeriesType'' Impulse $ toDecoration l
                         return $ format o d
toDataSeries' (Step,o) = do
                         c <- supply 
                         l <- toLine (c :: Color)
                         d <- setSeriesType'' Step $ toDecoration l
                         return $ format o d
toDataSeries' (Area,o) = do
                         c <- supply 
                         l <- toLine (c :: Color)
                         d <- setSeriesType'' Area $ toDecoration l
                         return $ format o d
toDataSeries' (Bar,o)  = do
                         c <- supply 
                         b <- toBar (c :: Color)
                         return $ format o b
toDataSeries' (Hist,o)  = do
                         c <- supply 
                         b <- toBar (c :: Color)
                         d <- setSeriesType'' Hist $ toDecoration b
                         return $ format o d
toDataSeries' (Candle,o)  = do
                         c <- supply 
                         b <- toBar (c :: Color)
                         d <- setSeriesType'' Candle $ toDecoration b
                         return $ format o d
toDataSeries' (Whisker,o)  = do
                         c <- supply 
                         b <- toBar (c :: Color)
                         d <- setSeriesType'' Whisker $ toDecoration b
                         return $ format o d

instance Dataset [FormattedSeries] where 
    toDataSeries ds = do
                      let ln = length ds
                      ds' <- sequence ds
                      return $ DS_Y $ A.listArray (1,ln) ds'

instance (Abscissa a) => Dataset (a,[FormattedSeries]) where
    toDataSeries (t,prs) = do
                           let ln = length prs
                           prs' <- sequence prs
                           return $ DS_1toN (toAbscissa t) $ A.listArray (1,ln) prs'

instance (Abscissa a) => Dataset [(a,FormattedSeries)] where
    toDataSeries prs = do
                       let ln = length prs
                           (xs,ys) = unzip prs
                       ys' <- sequence ys
                       return $ DS_1to1 $ A.listArray (1,ln) (zip (toAbscissae xs) ys')


-- | set the data set
setDataSeries :: Dataset a => a -> Data ()
setDataSeries d = do
                  ds <- toDataSeries d
                  put ds

-----------------------------------------------------------------------------

monoStep :: Double -> MaybeT (State Double) ()
monoStep d = do
             dp <- get
             when (d < dp) (fail "negative difference")
             put d
{-# INLINE monoStep #-}

isMonotoneIncreasing :: Vector Double -> Bool
isMonotoneIncreasing v = maybe False (\_ -> True) $ evalState (runMaybeT $ (mapVectorM_ monoStep (subVector 1 (size v -1) v))) (v `atIndex` 0)

-----------------------------------------------------------------------------

