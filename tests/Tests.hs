module Main where

import Test.Hspec
import Test.QuickCheck

import Control.Monad.Freer

import qualified Tests.Coroutine (tests)
import qualified Tests.Exception (tests)
import qualified Tests.Fresh (tests)
import qualified Tests.NonDet (tests)
import qualified Tests.Reader (tests)
import qualified Tests.State (tests)
import qualified Tests.Loop (tests)

addInEff :: Int -> Int -> Int
addInEff x y = run $ (+) <$> pure x <*> pure y

pureTests :: Spec
pureTests = describe "Pure Eff (Eff '[] a)" $
  it "adds numbers in a pure Eff the same as adding them normally" $
    property $ \x y -> addInEff x y == x + y

--------------------------------------------------------------------------------
                             -- Runner --
--------------------------------------------------------------------------------
main :: IO ()
main = hspec . parallel . describe "Tests" $ do
  pureTests
  Tests.Coroutine.tests
  Tests.Exception.tests
  Tests.Fresh.tests
  Tests.NonDet.tests
  Tests.Reader.tests
  Tests.State.tests
  Tests.Loop.tests
  
