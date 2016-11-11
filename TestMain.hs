module Main where

import Data.Maybe (fromJust)
import Control.Monad.Quota
    ( Quota(..)
    , QuotaError(..)
    , QuotaLimitT(..)
    , MonadQuota(..)
    )
import Control.Monad.Quota.Pure
    ( Op(..)
    , runOp
    , PureBase
    , pureBase
    , runPure
    )
import Control.Monad.State (State, runState, StateT(..), runStateT, put, get)
import Control.Monad.Catch (SomeException, fromException)
import Control.Monad.Catch.Pure (CatchT(..))
import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneof)

arbitraryNat = abs <$> arbitrary

instance Arbitrary Op where
    arbitrary = oneof [ pure Return
                      , Recurse <$> arbitrary
                      , Invoice <$> arbitraryNat <*> arbitrary
                      ]

instance Arbitrary Quota where
    arbitrary = Quota <$> arbitraryNat <*> arbitraryNat

-- | predicate which denotes whether the parts of the result are consistent.
-- An error should be flagged if and only if the quota has been used up.
isConsistent :: (Either QuotaError a, Quota) -> Bool
isConsistent (Left RecurseError, q) | recurseLimit q < 0 = True
isConsistent (Left TraverseError, q) | traverseLimit q < 0 = True
isConsistent (Right _, q) | recurseLimit q > 0 && traverseLimit q > 0 = True
isConsistent _ = False


runQuotaOp :: Quota -> Op -> (Either QuotaError (), Quota)
runQuotaOp q op = fixExn $
    runPure (runQuotaLimitT (runOp op) >>= pureBase) q
  where
    fixExn :: (Either SomeException a, s) -> (Either QuotaError a, s)
    fixExn (Left err, s) = (Left $ fromJust $ fromException err, s)
    fixExn (Right x, s) = (Right x, s)

propIsConsistent q op = isConsistent $ runQuotaOp q op

main :: IO ()
main = defaultMain
    [ testProperty "Any sequence of quota operations is consistent."
      propIsConsistent
    ]
