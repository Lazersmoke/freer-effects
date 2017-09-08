{-# LANGUAGE FlexibleContexts #-}
module Tests.State (tests) where

import Test.Hspec
import Test.QuickCheck

import Control.Monad.Freer
import Control.Monad.Freer.State (evalState, execState, get, put, runState)


tests :: Spec
tests = describe "State Eff" $ do
  it "follows the law: get (put n) ==> (n,n)" $
    property $ \n -> testPutGet n 0 == (n, n)
  it "stores only the final put" $
    property $ \p1 p2 start -> testPutGetPutGetPlus p1 p2 start == (p1 + p2, p2)
  it "preserves the start state over multiple gets" $ 
    property $ \start -> testGetStart start == (start, start)
{-
    , testProperty "testPutGet: State == StateRW"
        $ \n -> testPutGet n 0 == testPutGetRW n 0
    , testProperty "testPutGetPutGetPlus: State == StateRW"
        $ \p1 p2 start ->
            testPutGetPutGetPlus p1 p2 start
                == testPutGetPutGetPlusRW p1 p2 start
    , testProperty "testGetStart: State == StateRW"
        $ \n -> testGetStart n == testGetStartRW n
-}
  describe "evalState" $
    it "discards final state" $
      property $ \n -> testEvalState n == n
  describe "evalState" $
    it "returns final state" $
      property $ \n -> testExecState n == n

testPutGet :: Int -> Int -> (Int, Int)
testPutGet n start = run $ runState start $ put n >> get
{-
testPutGetRW :: Int -> Int -> (Int, Int)
testPutGetRW n start = run $ runStateR go start
  where
    go = tell n >> ask
-}

testPutGetPutGetPlus :: Int -> Int -> Int -> (Int, Int)
testPutGetPutGetPlus p1 p2 start = run $ runState start $ do
  put p1
  x <- get
  put p2
  y <- get
  pure (x + y)
{-
testPutGetPutGetPlusRW :: Int -> Int -> Int -> (Int, Int)
testPutGetPutGetPlusRW p1 p2 start = run $ runStateR go start
  where
    go = do
        tell p1
        x <- ask
        tell p2
        y <- ask
        pure (x+y)
-}
testGetStart :: Int -> (Int, Int)
testGetStart i = run $ runState i get

{-
testGetStartRW :: Int -> (Int, Int)
testGetStartRW = run . runStateR ask
-}

testEvalState :: Int -> Int
testEvalState i = run $ evalState i $ do
  x <- get
  -- Destroy the previous state.
  put (0 :: Int)
  pure x

testExecState :: Int -> Int
testExecState n = run $ execState 0 (put n)
