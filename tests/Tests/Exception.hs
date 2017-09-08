{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Tests.Exception (tests) where

import Test.Hspec
import Test.QuickCheck

import Control.Monad.Freer
import Control.Monad.Freer.Exception
import Control.Monad.Freer.Reader (ask, runReader)
import Control.Monad.Freer.State (State, get, put, runState)


tests :: Spec
tests = describe "Exception Eff" $ do
  it "takes precedence over other compuatations" $
    property $ \x y -> testExceptionTakesPriority x y == Left y
  it "doesn't catch when it's on the inside" $
    ter1 `shouldBe` (Left "exc", 2)
  it "doesn't catch when it's on the outside" $
    ter2 `shouldBe` Left "exc"
  it "catches when we catch it when it's on the inside" $
    ter3 `shouldBe` (Right "exc", 2)
  it "catches when we catch it when it's on the outside" $
    ter4 `shouldBe` Right ("exc", 2)
  it "finishes successfully if the error condition is not triggered" $
    ex2rr `shouldBe` Right 5
  it "fails when on the inside and uncaught" $
    ex2rr1 `shouldBe` Left (TooBig 7)
  it "fails when on the outside and uncaught" $
    ex2rr2 `shouldBe` Left (TooBig 7)

testExceptionTakesPriority :: Int -> Int -> Either Int Int
testExceptionTakesPriority x y = run $ runError (go x y)
  where
    go a b = (+) <$> pure a <*> throwError b

-- The following won't type: unhandled exception!
-- ex2rw = run et2
{-
    No instance for (Member (Exc Int) Void)
      arising from a use of `et2'
-}

-- Exceptions and state.
incr :: Member (State Int) r => Eff r ()
incr = get >>= put . (+ (1 :: Int))

tes1 :: (Members '[State Int, Exc String] r) => Eff r b
tes1 = incr >> throwError "exc"

ter1 :: (Either String Int, Int)
ter1 = run $ runState (1 :: Int) (runError tes1)

ter2 :: Either String (String, Int)
ter2 = run $ runError (runState (1 :: Int) tes1)

teCatch :: Member (Exc String) r => Eff r a -> Eff r String
teCatch m = (m >> pure "done") `catchError` \e -> pure (e :: String)

ter3 :: (Either String String, Int)
ter3 = run $ runState (1 :: Int) (runError (teCatch tes1))

ter4 :: Either String (String, Int)
ter4 = run $ runError (runState (1 :: Int) (teCatch tes1))

-- | The example from the paper.
newtype TooBig = TooBig Int
  deriving (Eq, Show)

ex2 :: Member (Exc TooBig) r => Eff r Int -> Eff r Int
ex2 m = do
    v <- m
    if v > 5
        then throwError (TooBig v)
        else pure v

-- | Specialization to tell the type of the exception.
runErrBig :: Eff (Exc TooBig ': r) a -> Eff r (Either TooBig a)
runErrBig = runError

ex2rr :: Either TooBig Int
ex2rr = run $ runReader (5 :: Int) (runErrBig (ex2 ask))

ex2rr1 :: Either TooBig Int
ex2rr1 = run $ runReader (7 :: Int) (runErrBig (ex2 ask))

-- | Different order of handlers (layers).
ex2rr2 :: Either TooBig Int
ex2rr2 = run $ runErrBig (runReader (7 :: Int) (ex2 ask))
