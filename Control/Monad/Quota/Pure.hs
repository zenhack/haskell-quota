module Control.Monad.Quota.Pure where

import Control.Monad.Catch.Pure (CatchT(..), SomeException)
import Control.Monad.Quota (Quota, MonadQuota(..))
import Control.Monad.State (State, runState)

-- | Datatype representation of the operations supported by MonadQuota.
data Op
    = Return
    | Recurse Op
    | Invoice Int Op
    deriving(Show)

runOp :: (MonadQuota m) => Op -> m ()
runOp Return = return ()
runOp (Recurse q') = recurse (runOp q')
runOp (Invoice n fm) = invoice n >> runOp fm

-- A pure implementation of (MonadState Quota) and MonadThrow.
type PureBase a = CatchT (State Quota) a
pureBase :: a -> PureBase a
pureBase x = CatchT $ return (Right x)

runPure :: PureBase a -> Quota -> (Either SomeException a, Quota)
runPure mq = runState (runCatchT $ mq)
