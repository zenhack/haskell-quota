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
import Control.Monad.Reader (MonadReader(..), ReaderT)
import Control.Monad.Writer (MonadWriter(..), WriterT)
import Control.Monad.RWS (MonadRWS(..), RWST)

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
class Monad m => MonadQuota m where
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

-- | Like 'runQuotaT', but only returns the monadic result (not the remaining
-- quota).
evalQuotaT :: (Monad m, Applicative m) => QuotaT m a -> Quota -> m a
evalQuotaT (QuotaT st) q = evalStateT st q

------ # Instances of standard mtl type classes, with QuotaT on the outside:

instance MonadState s m => MonadState s (QuotaT m) where
    get = lift get
    put = lift . put

instance MonadWriter w m => MonadWriter w (QuotaT m) where
    tell = lift . tell
    -- TODO: add 'listen' and 'pass'

instance MonadReader r m => MonadReader r (QuotaT m) where
    ask = lift ask
    reader = lift . reader
    -- TODO: add 'local'

instance MonadRWS r w s m => MonadRWS r w s (QuotaT m)

------ # Instances of MonadQuota for standard monad transformers:

instance MonadQuota m => MonadQuota (StateT s m) where
    invoice = lift . invoice

instance (Monoid w, MonadQuota m) => MonadQuota (WriterT w m) where
    invoice = lift . invoice

instance MonadQuota m => MonadQuota (ReaderT r m) where
    invoice = lift . invoice

instance (Monoid w, MonadQuota m) => MonadQuota (RWST r w s m) where
    invoice = lift . invoice
