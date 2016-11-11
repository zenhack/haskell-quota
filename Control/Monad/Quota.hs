{-|
Module : Control.Monad.Quota
Description : Track and control quota usage
-}
module Control.Monad.Quota
    ( MonadQuota(..)
    , Quota(..)
    , QuotaError(..)
    , QuotaLimitT(..)
    ) where

import Control.Monad (when)
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.State (MonadState, get, put, StateT, runStateT)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Typeable (Typeable)

class (Ord n, Num n, Monad m) => MonadQuota n m | m -> n where
    recurse :: m a -> m a
    invoice :: n -> m ()

-- | A quota to be imposed on a computation
data Quota n = Quota
    { recurseLimit :: n -- Maximum recursion depth
    , traverseLimit :: n -- Total traversal limit
    } deriving(Show)

-- | Thrown when a quota limit is exceeded.
data QuotaError
    = RecurseError -- ^ Recursion limit exceeded
    | TraverseError -- ^ Total traversal limit exeeded
    deriving(Show, Typeable)

instance Exception QuotaError

newtype QuotaLimitT m a =
    QuotaLimitT { runQuotaLimitT :: m a }
    deriving(Monad,Functor,Applicative)

instance MonadTrans QuotaLimitT where
    lift = QuotaLimitT

instance MonadState (Quota n) m => MonadState (Quota n) (QuotaLimitT m) where
    put = lift . put
    get = lift get

instance MonadThrow m => MonadThrow (QuotaLimitT m) where
    throwM = lift . throwM

instance (Ord n, Num n, MonadState (Quota n) m, MonadThrow m) => MonadQuota n (QuotaLimitT m) where
    recurse cursor = do
        Quota r t <- getQuota
        setQuota (r - 1) (t - 1)
        ret <- cursor
        Quota r' t' <- getQuota
        setQuota r t'
        return ret
    invoice n = do
        Quota r t <- getQuota
        when (n > t) $ throwM TraverseError
        setQuota r (t - n)

-- Helpers for MonadQuota (QuotaLimitT m):

getQuota :: (Ord n, Num n, MonadState (Quota n) m, MonadThrow m) => m (Quota n)
getQuota = do
    q@(Quota r t) <- get
    checkQuota r t
    return q

setQuota :: (Ord n, Num n, MonadState (Quota n) m, MonadThrow m) => n -> n -> m ()
setQuota r t = do
    checkQuota r t
    put $ Quota r t

checkQuota :: (Ord n, Num n, MonadState (Quota n) m, MonadThrow m) => n -> n -> m ()
checkQuota r t = do
    when (r < 0) $ throwM RecurseError
    when (t < 0) $ throwM TraverseError
    return ()
