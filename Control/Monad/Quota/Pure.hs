{-|
Module : Control.Monad.Quota.Pure
Description : Pure concrete implementation of MonadQuota.

This module provides convienence wrappers for concrete monad stacks
involving instances of 'MonadQuota'. It is intended to reduce the
boilerplate required to use the facilities of this library.

-}
module Control.Monad.Quota.Pure where

import Data.Functor.Identity (Identity, runIdentity)
import Control.Monad.Catch.Pure (CatchT(..), SomeException)
import Control.Monad.Quota
import Control.Monad.State (State, runState, StateT, runStateT)

-- | Datatype representation of the operations supported by MonadQuota.
-- Useful for testing.
data Op
    = Recurse [Op]
    | Invoice Int
    deriving(Show)

interpOps :: (MonadQuota m) => [Op] -> m ()
interpOps [] = return ()
interpOps (Recurse ops:ops') = recurse (interpOps ops) >> interpOps ops'
interpOps (Invoice n:ops) = invoice n >> interpOps ops

type CatchStateT m = CatchT (StateT Quota m)
type CatchState = CatchStateT Identity
type QuotaLimit = QuotaLimitT CatchState

runCatchStateT = runStateT  . runCatchT
runCatchState = runState . runCatchT
runQuotaLimit = runCatchState . runQuotaLimitT
