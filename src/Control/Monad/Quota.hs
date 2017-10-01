{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances, GeneralizedNewtypeDeriving #-}
-- | Description: Monad for tracking quota limits.
module Control.Monad.Quota
    ( Quota(..)
    , QuotaError(..)
    , QuotaT
    , MonadQuota(..)
    , runQuotaT
    , evalQuotaT
    )
  where

import Control.Monad.Catch (throwM, MonadThrow, Exception)
import Control.Monad (when)
import Control.Monad.Trans.Class (MonadTrans(..))

import Control.Monad.State (StateT, runStateT, evalStateT, MonadState(..))

-- | An exception indicating that the quota was exceeded.
data QuotaError
    = QuotaError
    deriving(Show, Eq)

instance Exception QuotaError

-- | A quota.
newtype Quota = Quota Int
              deriving(Show, Eq, Num)

-- | mtl-style type class for tracking quotas.
--
-- laws:
--
-- * @invoice 0 >> m = m@
class MonadQuota m where
    -- | @invoice n@ deducts @n@ from the quota.
    invoice :: Int -> m ()

-- | Monad transformer implementing MonadQuota on top of a 'MonadThrow'.
-- When the quota hits 0, it invokes @throwM QuotaError@.
newtype QuotaT m a = QuotaT (StateT Quota m a)
    deriving(Functor, Applicative, Monad)

-- | Run a 'QuotaT' with the given 'Quota'. Returns the monadic result
-- of the computation and the remaining quota.
runQuotaT :: QuotaT m a -> Quota -> m (a, Quota)
runQuotaT (QuotaT st) = runStateT st

instance MonadThrow m => MonadQuota (QuotaT m) where
    invoice n = QuotaT $ do
        Quota q <- get
        when (n > q) $ throwM QuotaError
        put $ Quota (q - n)

instance (MonadThrow m) => MonadThrow (QuotaT m) where
    throwM = lift . throwM

instance MonadTrans QuotaT where
    lift = QuotaT . lift

instance MonadState s m => MonadState s (QuotaT m) where
    get = lift get
    put = lift . put

-- | Like 'runQuotaT', but only returns the monadic result (not the remaining
-- quota).
evalQuotaT :: (Monad m, Applicative m) => QuotaT m a -> Quota -> m a
evalQuotaT (QuotaT st) q = evalStateT st q
