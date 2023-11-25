{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Supply
-- Copyright   :  (c) A. V. H. McPhail 2010
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- a monad that supplies the next value
--
-----------------------------------------------------------------------------

module Control.Monad.Supply (
                             Supply(..)
                             , MonadSupply(..)
                             , supplyN
                             , SupplyT(..), evalSupplyT, execSupplyT
                             , mapSupplyT
                            ) where

-----------------------------------------------------------------------------
#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
#endif
import Control.Monad.Writer hiding ( fail )
import Control.Monad.Reader hiding ( fail )
import Control.Monad.State hiding ( fail )
import Control.Monad.Trans()
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail, fail )
import Prelude hiding ( fail )
#endif
#if MIN_VERSION_mtl(2,3,0)
import Control.Monad
#endif
-----------------------------------------------------------------------------

class Supply a b where
    nextSupply :: a -> (b,a)

{-
instance Supply [a]       a where nextSupply (x:xs)      = (x,xs)
instance Supply ([a],[b]) a where nextSupply ((x:xs),ys) = (x,(xs,ys))
instance Supply ([a],[b]) b where nextSupply (xs,(y:ys)) = (y,(xs,ys))
-}
-----------------------------------------------------------------------------

replicateM :: Monad m => Int -> m a -> m [a]
replicateM 0 m = return []
replicateM 1 m = do
               a < m
               return [a]
replicateM n m = do
                a <- m
                as <- replicateM (n-1) m
                return (a:as)

class Monad m => MonadSupply s m | m -> s where
    supply :: Supply s a => m a

supplyN :: (MonadSupply s m, Supply s a) => Int -> m [a]
supplyN n = replicateM n supply

-----------------------------------------------------------------------------

newtype SupplyT s m a = SupplyT { runSupplyT :: s -> m (a, s) }

evalSupplyT :: Monad m => SupplyT s m a -> s -> m a
evalSupplyT st s = do
                   ~(a,_) <- runSupplyT st s
                   return a

execSupplyT :: Monad m => SupplyT s m a -> s -> m s
execSupplyT st s = do
                   ~(_,s') <- runSupplyT st s
                   return s'

mapSupplyT :: (m (a,s) -> n (b,s)) -> SupplyT s m a -> SupplyT s n b
mapSupplyT f st = SupplyT $ f . runSupplyT st

-----------------------------------------------------------------------------

instance Monad m => Functor (SupplyT s m) where
    fmap f m = SupplyT $ \s -> do
                               ~(x, s') <- runSupplyT m s
                               return (f x,s')
instance Monad m => Applicative (SupplyT s m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (SupplyT s m) where
    return a  = SupplyT $ \s -> return (a, s)
    m >>= f   = SupplyT $ \s -> do
                                ~(a,s') <- runSupplyT m s
                                runSupplyT (f a) s'

instance (MonadFail m, Monad m) => MonadFail (SupplyT s m) where
    fail str  = SupplyT $ \_ -> fail str

instance MonadTrans (SupplyT s) where
    lift m = SupplyT $ \s -> do
                             a <- m
                             return (a,s)

instance Monad m => MonadSupply s (SupplyT s m) where
    supply = SupplyT $ \s -> return $ nextSupply s

-----------------------------------------------------------------------------
{-
instance (Monad (t m), MonadSupply s m, MonadTrans t) => MonadSupply s (t m) where
    supply = lift supply
-}
-----------------------------------------------------------------------------

instance MonadState s m => MonadState s (SupplyT s' m) where
    get = lift get
    put = lift . put

instance MonadReader r m => MonadReader r (SupplyT s m) where
    ask       = lift ask
    local f m = SupplyT $ \s -> local f (runSupplyT m s)

instance MonadWriter w m => MonadWriter w (SupplyT s m) where
    tell      = lift . tell
    listen m  = SupplyT $ \s -> do
                                ~((a,s'),w) <- listen (runSupplyT m s)
                                return ((a,w),s')
    pass m    = SupplyT $ \s -> pass $ do
                                       ~((a,f),s') <- runSupplyT m s
                                       return ((a,s'),f)

-----------------------------------------------------------------------------
