{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Tests.Coroutine (tests) where

import Control.Monad (unless)

import Test.Hspec
import Test.QuickCheck

import Control.Monad.Freer
import Control.Monad.Freer.Coroutine
import Control.Monad.Freer.State (State, modify, runState)


tests :: Spec
tests = describe "Coroutine Eff" $
  it "counts consecutive pairs of odds" $
    property $ \list -> runTestCoroutine list == countOddDuoPrefix list

-- | Counts number of consecutive pairs of odd elements at beginning of a list.
countOddDuoPrefix :: [Int] -> Int
countOddDuoPrefix list = count list 0
  where
    count (i1:i2:is) n = if even i1 && even i2 then n else count is (n + 1)
    count _          n = n

runTestCoroutine :: [Int] -> Int
runTestCoroutine list = snd . run $ runState 0 effTestCoroutine
  where
    testCoroutine :: Members '[Yield () Int, State Int] r => Eff r ()
    testCoroutine = do
        -- Yield for two elements and hope they're both odd.
        b <- (&&)
            <$> yield () (even :: Int -> Bool)
            <*> yield () (even :: Int -> Bool)
        unless b $ modify (+ (1 :: Int)) >> testCoroutine

    effTestCoroutine = runC testCoroutine >>= handleStatus list
      where
        handleStatus _      (Done ())       = pure ()
        handleStatus (i:is) (Continue () k) = k i >>= handleStatus is
        handleStatus []     _               = pure ()
