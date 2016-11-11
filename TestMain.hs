module Main where

import Data.Maybe (fromJust)
import Control.Monad.Quota
import Control.Monad.Quota.Pure
import Control.Monad.State (State, runState, StateT(..), runStateT, put, get)
import Control.Monad.Catch (SomeException, fromException)
import Control.Monad.Catch.Pure (CatchT(..))
import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneof)

arbitraryNat = abs <$> arbitrary

instance Arbitrary Op where
    arbitrary = oneof [ Recurse <$> arbitrary
                      , Invoice <$> arbitraryNat
                      ]

instance Arbitrary Quota where
    arbitrary = Quota <$> arbitraryNat <*> arbitraryNat

type Result = (Either QuotaError (), Quota)

runQuotaOps :: Quota -> [Op] -> Result
runQuotaOps q ops = fixExn $ runQuotaLimit (interpOps ops) q
  where
    fixExn :: (Either SomeException (), Quota) -> Result
    fixExn (Left err, s) = (Left $ fromJust $ fromException err, s)
    fixExn (Right x, s) = (Right x, s)

resultProp :: (Result -> Bool) -> Quota -> [Op] -> Bool
resultProp p q ops = p (runQuotaOps q ops)

main :: IO ()
main = defaultMain
    [ testProperty "No sequence of operations reduces the quotas below zero."
      (resultProp (\(_, Quota r t) -> r >= 0 && t >= 0))
    ]
