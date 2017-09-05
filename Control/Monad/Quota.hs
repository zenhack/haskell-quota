{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances, GeneralizedNewtypeDeriving #-}
module Control.Monad.Quota where

import Control.Monad.Catch (throwM, MonadThrow, Exception)
import Control.Monad (when)
import Control.Monad.Trans.Class (MonadTrans(..))

import Control.Monad.State (StateT, runStateT, evalStateT, MonadState(..))

data QuotaError
    = QuotaError
    deriving(Show, Eq)

instance Exception QuotaError

newtype Quota = Quota Int
              deriving(Show, Eq)

class MonadQuota m where
    invoice :: Int -> m ()

newtype QuotaT m a = QuotaT (StateT Quota m a)
    deriving(Functor, Applicative, Monad)

runQuotaT :: QuotaT m a -> Quota -> m (a, Quota)
runQuotaT (QuotaT st) = runStateT st

instance (Monad m, MonadThrow m) => MonadQuota (QuotaT m) where
    invoice n = QuotaT $ do
        Quota q <- get
        when (n > q) $ throwM QuotaError
        put $ Quota (q - n)

instance (MonadThrow m) => MonadThrow (QuotaT m) where
    throwM = QuotaT . lift . throwM

instance MonadTrans QuotaT where
    lift = QuotaT . lift

instance MonadState s m => MonadState s (QuotaT m) where
    get = lift get
    put = lift . put

evalQuotaT :: (Monad m, Applicative m) => QuotaT m a -> Quota -> m a
evalQuotaT (QuotaT st) q = evalStateT st q
