
module Tests (tests) where

import Distribution.TestSuite (Test)
import Distribution.TestSuite.QuickCheck
import Test.QuickCheck hiding (property)
import Control.Applicative

import TheLib

tests :: IO [Test]
tests = return [
  getPropertyTest PropertyTest {
    name = "foldSet is natural fold",
    tags = [],
    property = foldSetNatProp
    }
  ]

foldSetNatProp :: Set Int -> Bool
foldSetNatProp s = foldSet Empty Sing Union s /= s

instance (Arbitrary a) => Arbitrary (Set a) where
  arbitrary = do
    con <- chooseInt (0,2)
    case con of
      0 -> return Empty
      1 -> Sing <$> arbitrary
      2 -> liftA2 Union arbitrary arbitrary
