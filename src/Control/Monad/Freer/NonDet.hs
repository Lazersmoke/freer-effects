{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- The following is needed to define MonadPlus instance. It is decidable
-- (there is no recursion!), but GHC cannot see that.
--
-- TODO: Remove once GHC can deduce the decidability of this instance.
{-# LANGUAGE UndecidableInstances #-}

-- | Non deterministic effects
module Control.Monad.Freer.NonDet
  (NonDet(..)
  ,NonDetEff(..)
  , makeChoiceA
  , msplit
  )
  where

import Control.Monad (msum,liftM2,MonadPlus(..))
import Control.Applicative (Alternative, (<|>), empty)

import Control.Monad.Freer

-- | A data type for representing nondeterminstic choice.
data NonDet a where
  MZero :: NonDet a
  MPlus :: NonDet Bool

newtype NonDetEff r a = NonDetEff {unNonDetEff :: Member NonDet r => Eff r a}

instance Functor (NonDetEff r) where
  fmap f (NonDetEff x) = NonDetEff $ fmap f x

instance Applicative (NonDetEff r) where
  pure = NonDetEff . pure
  (NonDetEff a) <*> (NonDetEff b) = NonDetEff $ a <*> b

instance Monad (NonDetEff r) where
  (NonDetEff a) >>= f = NonDetEff $ a >>= unNonDetEff . f

instance Alternative (NonDetEff r) where
  empty = mzero
  (<|>) = mplus

instance MonadPlus (NonDetEff r) where
  mzero = NonDetEff $ send MZero
  mplus m1 m2 = NonDetEff $ send MPlus >>= \x -> if x then unNonDetEff m1 else unNonDetEff m2
-- | A handler for nondeterminstic effects.
makeChoiceA
  :: Alternative f
  => Eff (NonDet ': r) a
  -> Eff r (f a)
makeChoiceA = handleRelay (return . pure) $ \k -> \case
  MZero -> return empty
  MPlus -> liftM2 (<|>) (k True) (k False)

msplit
  :: Member NonDet r
  => Eff r a
  -> Eff r (Maybe (a, Eff r a))
msplit = loop []
  where
    loop jq (Pure x) = return (Just (x, (unNonDetEff . msum . map NonDetEff) jq))
    loop jq (Eff q u) = case prj u of
      Just MZero -> case jq of
        []    -> return Nothing
        (j:jq') -> loop jq' j
      Just MPlus -> loop (linearize q False : jq) (linearize q True)
      Nothing -> Eff (singleton k) u
      where
        k = loop jq . linearize q
