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
                                                , setDataSeries
                                                -- * Plot type
                                                , setSeriesType
                                                , setAllSeriesTypes
                                                -- * Formatting
                                                , PlotType(..), PlotFormats(..)
                                                , withSeriesFormat
                                                , withAllSeriesFormats
                                                -- * Internal
                                                , Abscissa(), Ordinate(), Dataset()
                                                ) where

-----------------------------------------------------------------------------

--import Data.Packed.Vector

import Data.Maybe

import qualified Data.Array.IArray as A

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Supply

import Graphics.Rendering.Plot.Types
import Graphics.Rendering.Plot.Figure.Line
import Graphics.Rendering.Plot.Figure.Point

-----------------------------------------------------------------------------

dataSeriesNum :: DataSeries -> Int
dataSeriesNum (DS_Y a)      = A.rangeSize $ A.bounds $ a
dataSeriesNum (DS_1toN _ a) = A.rangeSize $ A.bounds $ a
dataSeriesNum (DS_1to1 a)   = A.rangeSize $ A.bounds $ a

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
    setSeriesType'' Point     (DecLine lt)     = do
                                                 let c = fromJust $ getLineColour lt
                                                 g <- supply
                                                 pt <- toPoint (g :: Glyph,c)
                                                 return $ DecPoint pt
    setSeriesType'' Point     d@(DecPoint _)   = return d
    setSeriesType'' Point     (DecLinPt _ pt)  = return $ DecPoint pt
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

instance PlotFormats Point where
    modifyFormat _ d@(DecSeries _ (DecLine _))    = return d
    modifyFormat p (DecSeries o (DecPoint pt))    = do
                                                    po <- asks _pointoptions
                                                    let pt' = execPoint p po pt
                                                    return $ DecSeries o (DecPoint pt')
    modifyFormat p (DecSeries o (DecLinPt lt pt)) = do
                                                    po <- asks _pointoptions
                                                    let pt' = execPoint p po pt
                                                    return $ DecSeries o (DecLinPt lt pt')

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

instance Abscissa Series                   where toAbscissa s         = AbsPoints s

class Ordinate a where
    toOrdinate :: a -> Ordinates

toOrdinates :: Ordinate a => [a] -> [Ordinates]
toOrdinates = map toOrdinate

instance Ordinate Function                           where toOrdinate f         = OrdFunction Lower f
instance Ordinate Series                             where toOrdinate s         = OrdPoints Lower (Plain s)
instance Ordinate (Series,ErrorSeries)               where toOrdinate (s,e)     = OrdPoints Lower (Error s (e,e))
instance Ordinate (Series,(ErrorSeries,ErrorSeries)) where toOrdinate (s,(l,u)) = OrdPoints Lower (Error s (l,u))

instance Ordinate (Function,AxisSide)                         where toOrdinate (f,ax)       = OrdFunction ax f
instance Ordinate (Series,AxisSide)                           where toOrdinate (s,ax)       = OrdPoints ax (Plain s)
instance Ordinate (Series,ErrorSeries,AxisSide)               where toOrdinate (s,e,ax)     = OrdPoints ax (Error s (e,e))
instance Ordinate (Series,(ErrorSeries,ErrorSeries),AxisSide) where toOrdinate (s,(l,u),ax) = OrdPoints ax (Error s (l,u))

class Decorations a where
    toDecoration :: a -> Decoration

toDecorations :: Decorations a => [a] -> [Decoration]
toDecorations = map toDecoration

instance Decorations LineType             where toDecoration l     = DecLine l
instance Decorations PointType            where toDecoration p     = DecPoint p
instance Decorations (LineType,PointType) where toDecoration (l,p) = DecLinPt l p
instance Decorations (PointType,LineType) where toDecoration (p,l) = DecLinPt l p
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

getNTypes :: Int -> SeriesType -> Data [Decoration]
getNTypes n st = mapM getType (replicate n st)

-----------------------------------------------------------------------------

class Dataset a where
    toDataSeries :: a -> Data DataSeries

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

instance (Abscissa a, Ordinate b) => Dataset (SeriesType,[(a,b)]) where
    toDataSeries (Line,prs) = do
                            let ln = length prs
                            cs <- supplyN ln
                            ls <- mapM toLine (cs :: [Color])
                            let (xs,ys') = unzip prs
                                ys = zipWith format ys' ls
                            return $ DS_1to1 $ A.listArray (1,ln) $ zip (toAbscissae xs) ys
    toDataSeries (Point,prs) = do
                            let ln = length prs
                            cs <- supplyN ln
                            gs <- supplyN ln
                            ps <- mapM toPoint (zip (gs :: [Glyph]) (cs :: [Color]))
                            let (xs,ys') = unzip prs
                                ys = zipWith format ys' ps
                            return $ DS_1to1 $ A.listArray (1,ln) $ zip (toAbscissae xs) ys
    toDataSeries (LinePoint,prs) = do
                            let ln = length prs
                            cs <- supplyN ln
                            gs <- supplyN ln
                            ps <- mapM toPoint (zip (gs :: [Glyph]) (cs :: [Color]))
                            ls <- mapM toLine (cs :: [Color])
                            let ds = toDecorations (zip ls ps)
                            let (xs,ys') = unzip prs
                                ys = zipWith format ys' ds
                            return $ DS_1to1 $ A.listArray (1,ln) $ zip (toAbscissae xs) ys

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

