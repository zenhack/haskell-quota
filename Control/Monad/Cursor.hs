module Control.Monad.Cursor
    ( MonadQuota
    , Quota(..)
    , QuotaError(..)
    , QuotaCursorT(..)
    ) where

import Control.Monad (when)
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.State (MonadState, get, put, StateT, runStateT)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Typeable (Typeable)

data Quota = Quota
    { recurseLimit :: Int
    , traverseLimit :: Int
    } deriving(Show)

data QuotaError = RecurseError | TraverseError
                deriving(Show, Typeable)
instance Exception QuotaError

getQuota :: (MonadState Quota m, MonadThrow m) => m Quota
getQuota = do
    q@(Quota r t) <- get
    checkQuota r t
    return q

setQuota :: (MonadState Quota m, MonadThrow m) => Int -> Int -> m ()
setQuota r t = do
    checkQuota r t
    put $ Quota r t

checkQuota :: (MonadState Quota m, MonadThrow m) => Int -> Int -> m ()
checkQuota r t = do
    when (r <= 0) $ throwM RecurseError
    when (t <= 0) $ throwM TraverseError
    return ()

class (Monad m) => MonadQuota m where
    recurse :: m a -> m a
    invoice :: Int -> m ()

newtype QuotaCursorT m a =
    QuotaCursorT { runQuotaCursorT :: m a }
    deriving(Monad,Functor,Applicative)

instance MonadTrans QuotaCursorT where
    lift = QuotaCursorT

instance MonadState Quota m => MonadState Quota (QuotaCursorT m) where
    put = lift . put
    get = lift get

instance MonadThrow m => MonadThrow (QuotaCursorT m) where
    throwM = lift . throwM

instance (MonadState Quota m, MonadThrow m) => MonadQuota (QuotaCursorT m) where
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
