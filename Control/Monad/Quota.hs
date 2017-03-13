{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances #-}
module Control.Monad.Quota where

-- NOTE: QuotaT basically re-implements the state monad internally. I
-- (zenhack) have found that the code actually ends up being shorter
-- and more straightforward this way than trying to reuse StateT.

import Control.Monad.Catch (throwM, MonadThrow, Exception)
import Control.Monad (when)
import Control.Monad.Trans.Class (MonadTrans(..))

import Control.Monad.State (MonadState(..))

data QuotaError
    = QuotaError
    deriving(Show, Eq)

instance Exception QuotaError

newtype Quota = Quota Int
              deriving(Show, Eq)

class MonadQuota m where
    invoice :: Int -> m ()

newtype QuotaT m a = QuotaT { runQuotaT :: Quota -> m (a, Quota) }

instance (Monad m) => Monad (QuotaT m) where
    return x = QuotaT $ \q -> return (x, q)
    QuotaT r >>= f = QuotaT $ \q -> do
        (ret, q') <- r q
        runQuotaT (f ret) q'

instance (Monad m, MonadThrow m) => MonadQuota (QuotaT m) where
    invoice n = QuotaT $ \(Quota q) -> do
        when (n > q) $ throwM QuotaError
        return ((), Quota (q - n))

instance (MonadThrow m) => MonadThrow (QuotaT m) where
    throwM = lift . throwM

instance MonadTrans QuotaT where
    lift m = QuotaT $ \q -> do
        ret <- m
        return (ret, q)

-- These are just the standard ways of derving Applicative & Functor
-- from Monad; nothing quota-specific going on here:
instance (Monad m) => Applicative (QuotaT m) where
    pure = return
    f <*> x = do
        x' <- x
        f' <- f
        return (f' x')
instance (Monad m, Applicative m) => Functor (QuotaT m) where
    fmap f q = pure f <*> q


-- Misc. mtl type class instances:

instance MonadState s m => MonadState s (QuotaT m) where
    get = lift get
    put = lift . put
    state = lift . state
