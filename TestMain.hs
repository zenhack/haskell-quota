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


instance Arbitrary Op where
    arbitrary = oneof [ pure Return
                      , Recurse <$> arbitrary
                      , Invoice <$> (abs <$> arbitrary) <*> arbitrary
                      ]

instance Arbitrary Quota where
    arbitrary = Quota <$> arbitrary <*> arbitrary

-- | predicate which denotes whether the parts of the result are consistent.
-- An error should be flagged if and only if the quota has been used up.
isConsistent :: (Either QuotaError a, Quota) -> Bool
isConsistent (Left RecurseError, q) | recurseLimit q < 0 = True
isConsistent (Left TraverseError, q) | traverseLimit q < 0 = True
isConsistent (Right _, q) | recurseLimit q > 0 && traverseLimit q >= 0 = True
isConsistent _ = False

propQuotaIsConsistent :: Quota -> Op -> Bool
propQuotaIsConsistent q qa = isConsistent $ fixExn $
    runPure (runQuotaLimitT (runOp qa) >>= pureBase) q
  where
    fixExn :: (Either SomeException a, s) -> (Either QuotaError a, s)
    fixExn (Left err, s) = (Left $ fromJust $ fromException err, s)
    fixExn (Right x, s) = (Right x, s)

main :: IO ()
main = defaultMain
    [ testProperty "Any sequence of quota operations is consistent."
      propQuotaIsConsistent
    ]
