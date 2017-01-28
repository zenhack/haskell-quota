module Main where

import Control.Monad.Quota
import Control.Monad.Catch (SomeException)
import Control.Monad.Catch.Pure (CatchT(..))
import Data.Functor.Identity (Identity(..))
import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneof, Gen)

arbitraryNat = abs <$> arbitrary

data QuotaOp = Recurse QuotaOp
             | Invoice Int
             | Then QuotaOp QuotaOp
             deriving(Show, Eq)

instance Arbitrary QuotaOp where
    arbitrary = oneof [ Recurse <$> arbitrary
                      , Invoice <$> arbitraryNat
                      , Then <$> arbitrary <*> arbitrary
                      ]

instance Arbitrary Quota where
    arbitrary = Quota <$> arbitraryNat <*> arbitraryNat

interpQuotaOp :: QuotaOp -> QuotaT (CatchT Identity) ()
interpQuotaOp (Recurse qo) = recurse (interpQuotaOp qo)
interpQuotaOp (Invoice n)  = invoice n
interpQuotaOp (Then x y)   = interpQuotaOp x >> interpQuotaOp y

runPureQuota :: QuotaT (CatchT Identity) a -> Quota -> Either SomeException (a, Quota)
runPureQuota qt q = runIdentity $ runCatchT $ runQuotaT qt q

noNegativeQuotas :: QuotaOp -> Quota -> Bool
noNegativeQuotas qo q = case runPureQuota (interpQuotaOp qo) q of
    Left _ -> True
    Right ((), q')
        | recursionLimit q' < 0 -> False
        | traversalLimit q' < 0 -> False
        | otherwise -> True

main :: IO ()
main = defaultMain
    [ testProperty "No sequence of operations reduces the quotas below zero."
      noNegativeQuotas
    ]
