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

type Result = (Either QuotaError (), Quota)

runQuotaOp :: Quota -> Op -> Result
runQuotaOp q op = fixExn $
    runPure (runQuotaLimitT (runOp op) >>= pureBase) q
  where
    fixExn :: (Either SomeException a, s) -> (Either QuotaError a, s)
    fixExn (Left err, s) = (Left $ fromJust $ fromException err, s)
    fixExn (Right x, s) = (Right x, s)

resultProp :: (Result -> Bool) -> Quota -> Op -> Bool
resultProp p q op = p (runQuotaOp q op)

main :: IO ()
main = defaultMain
    [ testProperty "No sequence of operations reduces the quotas below zero."
      (resultProp (\(_, Quota r t) -> r >= 0 && t >= 0))
    ]
