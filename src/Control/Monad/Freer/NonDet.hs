{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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
  --, makeChoiceA
  --, msplit
  )
  where

--import Control.Monad (MonadPlus(..))
--import Control.Applicative (Alternative, (<|>), empty)

--import Control.Monad.Freer

-- | A data type for representing nondeterminstic choice.
data NonDet a where
  MZero :: NonDet a
  MPlus :: NonDet Bool

--instance Member NonDet effs => Alternative (Eff effs) where
  --empty = mzero
  --(<|>) = mplus

--instance Member NonDet effs => MonadPlus (Eff effs) where
  --mzero   = send MZero
  --mplus m1 m2 = send MPlus >>= \x -> if x then m1 else m2
{-
-- | A handler for nondeterminstic effects.
makeChoiceA
  :: Alternative f
  => Eff (NonDet ': effs) a
  -> Eff effs (f a)
makeChoiceA = handleRelay (return . pure) $ \m k ->
  case m of
    MZero -> return empty
    MPlus -> liftM2 (<|>) (k True) (k False)

msplit
  :: Member NonDet effs
  => Eff effs a
  -> Eff effs (Maybe (a, Eff effs a))
msplit = loop []
  where
  loop jq (Pure x) = return (Just (x, msum jq))
  loop jq (Eff q u) = case prj u of
    Just MZero -> case jq of
      []    -> return Nothing
      (j:jq') -> loop jq' j
    Just MPlus -> loop (qApp q False : jq) (qApp q True)
    Nothing  -> E u (tsingleton k)
    where
    k = qComp q (loop jq)
-}
