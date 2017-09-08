{-# LANGUAGE FlexibleContexts #-}
module Tests.NonDet (tests)
  where

import Control.Applicative ((<|>), pure)
import Control.Monad (guard, msum, mzero)
import Data.List ((\\))
import Data.Maybe (maybe)

import Test.Hspec
import Test.QuickCheck

import Control.Monad.Freer
import Control.Monad.Freer.NonDet


tests :: Spec
tests = describe "NonDet Eff" $
  it "Works to generate primes 2..n" $
    property $ \n' -> let n = abs n' in testIfte [2 .. n] == primesTo n

-- https://wiki.haskell.org/Prime_numbers
primesTo :: Int -> [Int]
primesTo m = sieve [2 .. m]
  where
    -- Function (\\) is set-difference for unordered lists.
    sieve (x:xs) = x : sieve (xs \\ [x, (x + x) .. m])
    sieve []     = []

ifte :: Member NonDet r => Eff r a -> (a -> Eff r b) -> Eff r b -> Eff r b
ifte t th el = msplit t >>= maybe el (\(a,m) -> unNonDetEff $ NonDetEff (th a) <|> (NonDetEff (m >>= th)))

generatePrimes :: Member NonDet r => [Int] -> Eff r Int
generatePrimes xs = do
    n <- gen
    ifte
        (gen >>= \d -> unNonDetEff . guard $ d < n && n `mod` d == 0)
        (const $ unNonDetEff mzero)
        (pure n)
  where
    gen = (unNonDetEff . msum . map NonDetEff) (pure <$> xs)

testIfte :: [Int] -> [Int]
testIfte = run . makeChoiceA . generatePrimes
