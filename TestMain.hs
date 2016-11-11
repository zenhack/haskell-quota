module Main where

import Data.Maybe (fromJust)
import Control.Monad.Quota
    ( Quota(..)
    , QuotaError(..)
    , QuotaLimitT(..)
    , MonadQuota(..)
    )
import Control.Monad.State (State, runState, StateT(..), runStateT, put, get)
import Control.Monad.Catch (SomeException, fromException)
import Control.Monad.Catch.Pure (CatchT(..))
import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneof)


data QuotaAction
    = Return
    | Recurse QuotaAction
    | Invoice Int QuotaAction
    deriving(Show)

instance Arbitrary QuotaAction where
    arbitrary = oneof [ pure Return
                      , Recurse <$> arbitrary
                      , Invoice <$> (abs <$> arbitrary) <*> arbitrary
                      ]

instance Arbitrary Quota where
    arbitrary = Quota <$> arbitrary <*> arbitrary

doQuotaAction :: (MonadQuota m) => QuotaAction -> m ()
doQuotaAction Return = return ()
doQuotaAction (Recurse q') = recurse (doQuotaAction q')
doQuotaAction (Invoice n fm) = do
    invoice n
    doQuotaAction fm

isConsistent :: (Either QuotaError a, Quota) -> Bool
isConsistent (Left RecurseError, q) | recurseLimit q < 0 = True
isConsistent (Left TraverseError, q) | traverseLimit q < 0 = True
isConsistent (Right _, q) | recurseLimit q > 0 && traverseLimit q >= 0 = True
isConsistent _ = False

propQuotaIsConsistent :: Quota -> QuotaAction -> Bool
propQuotaIsConsistent q qa = isConsistent $ fixExn $
    runPure ((doQuotaAction qa :: QuotaM ()) >>= liftQuotaM :: QuotaM ()) q
  where
    fixExn :: (Either SomeException a, s) -> (Either QuotaError a, s)
    fixExn (Left err, s) = (Left $ fromJust $ fromException err, s)
    fixExn (Right x, s) = (Right x, s)

main :: IO ()
main = defaultMain
    [ testProperty "Any sequence of quota operations is consistent."
      propQuotaIsConsistent
    ]

-- TODO: these should go in the library somewhere, not here in the test suite.
type QuotaM a = QuotaLimitT (CatchT (State Quota)) a

liftQuotaM ::a -> QuotaM a
liftQuotaM x = QuotaLimitT (CatchT $ return (Right x))

runPure :: QuotaM a -> Quota -> (Either SomeException a, Quota)
runPure qm q = runState (runCatchT $ runQuotaLimitT qm) q
