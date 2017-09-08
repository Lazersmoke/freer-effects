{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Tests.Reader (tests) where

import Test.Hspec
import Test.QuickCheck

import Control.Monad.Freer
import Control.Monad.Freer.Reader (ask, local, runReader)

tests :: Spec
tests = describe "Reader Eff" $ do
  it "passes along environment: n + x" $
    property $ \n x -> testReader n x == n + x
  it "works when stacked" $
    property $ \i n -> testMultiReader i n == (i + 2) + fromIntegral (n + 1)
  it "allows local to inject into env" $
    property $ \env inc -> testLocal env inc == 2 * (env + 1) + inc

--------------------------------------------------------------------------------
                            -- Examples --
--------------------------------------------------------------------------------
testReader :: Int -> Int -> Int
testReader n x = run . runReader n $ (+) <$> ask <*> pure x

{-
t1rr' = run t1
    No instance for (Member (Reader Int) Void)
      arising from a use of `t1'
-}

testMultiReader :: Integer -> Int -> Integer
testMultiReader i1 i2 = run . runReader i1 $ runReader i2 t2
  where
    t2 = do
        v1 <- ask
        v2 <- ask
        pure $ fromIntegral (v1 + (1 :: Int)) + (v2 + (2 :: Integer))

-- The opposite order of layers
{- If we mess up, we get an error
t2rrr1' = run $ runReader (runReader t2 (20 :: Float)) (10 :: Float)
    No instance for (Member (Reader Int) [])
      arising from a use of `t2'
-}

testLocal :: Int -> Int -> Int
testLocal env inc = run $ runReader env t3
  where
    t3 = (+) <$> t1 <*> local (+ inc) t1
    t1 = (+) <$> ask <*> pure (1 :: Int)
