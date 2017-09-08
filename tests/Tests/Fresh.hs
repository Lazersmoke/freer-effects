module Tests.Fresh (tests) where

import Control.Monad (replicateM)

import Test.Hspec
import Test.QuickCheck

import Control.Monad.Freer
import Control.Monad.Freer.Fresh


tests :: Spec
tests = describe "Fresh Eff" $ do
  it "yields the initial value upon first freshening" $
    property $ \n -> testFresh 1 (n :: Char) `shouldBe` n
  it "yields 9 when incremented from 10 times from 0" $
    testFresh 10 (0 :: Int) `shouldBe` 9
  it "yields (n-1) after n freshenings" $
    property $ \n -> n > 0 ==> testFresh n (0 :: Int) == (n-1)
  it "works with Char as well" $
    testFresh 2 'a' `shouldBe` 'b'

makeFresh :: Enum f => Int -> f -> Eff r f
makeFresh n f = fst <$> runFresh f (last <$> replicateM n fresh)

testFresh :: Enum f => Int -> f -> f
testFresh n f = run (makeFresh n f)
