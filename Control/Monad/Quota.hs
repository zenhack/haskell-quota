module Control.Monad.Quota where

-- NOTE: QuotaT basically re-implements the state monad internally. I
-- (zenhack) have found that the code actually ends up being shorter
-- and more straightforward this way than trying to reuse StateT.

import Control.Monad.Catch (throwM, MonadThrow, Exception)
import Control.Monad (when)

data QuotaError
    = TraversalQuotaError
    | RecursionQuotaError
    deriving(Show, Eq)

instance Exception QuotaError

data Quota = Quota
    { recursionLimit :: Int
    , traversalLimit :: Int
    } deriving(Show, Eq)

class MonadQuota m where
    invoice :: Int -> m ()
    recurse :: m a -> m a

newtype QuotaT m a = QuotaT { runQuotaT :: Quota -> m (a, Quota) }


instance (Monad m) => Monad (QuotaT m) where
    return x = QuotaT $ \q -> return (x, q)
    QuotaT r >>= f = QuotaT $ \q -> do
        (ret, q') <- r q
        runQuotaT (f ret) q'

instance (Monad m, MonadThrow m) => MonadQuota (QuotaT m) where
    invoice n = QuotaT $ \q -> do
        let tlim = traversalLimit q
        when (n > tlim) $ throwM TraversalQuotaError
        return ((), q { traversalLimit = tlim - n })
    recurse (QuotaT m) = do
        invoice 1
        QuotaT $ \q -> do
            let rlim = recursionLimit q
            when (rlim < 1) $ throwM RecursionQuotaError
            (ret, q') <- m $ q { recursionLimit = rlim - 1 }
            return (ret, q' { recursionLimit = rlim })


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
