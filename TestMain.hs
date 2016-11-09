module Main where

import Data.Functor.Identity (Identity, runIdentity)
import Control.Monad.Cursor (Quota, QuotaError)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.Except (ExceptT, runExceptT)
import Test.Framework (defaultMain)

main :: IO ()
main = defaultMain
    [
    ]

-- TODO: these should go in the library somewhere, not here in the test suite.
type QuotaM a = ExceptT QuotaError (StateT Quota Identity) a

runPure :: QuotaM a -> Quota -> (Either QuotaError a, Quota)
runPure qm q = runIdentity $ runStateT (runExceptT qm) q
