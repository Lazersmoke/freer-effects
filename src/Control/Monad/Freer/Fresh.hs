{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Generation of fresh integers as an effect.
--
-- Composable handler for 'Fresh' effects. This is likely to be of use when
-- implementing De Bruijn naming/scopes.
module Control.Monad.Freer.Fresh
  (Fresh(..)
  ,fresh
  ,runFresh
  ,evalFresh
  )
  where

import Control.Monad.Freer
import Control.Monad.Freer.State

-- | The @'Fresh' f@ effect keeps a global counter of @f@ that you
-- can pull fresh values from as an effect.
data Fresh f a where
  Fresh :: Fresh f f

-- | Get a fresh @f@ from the global counter
fresh :: Member (Fresh f) r => Eff r f
fresh = send Fresh

-- | Handles a @'Fresh' f@ effect in the expected way. Also returns
-- the final value of the global counter (the next to be dispensed) when it is done.
runFresh :: forall f r a. Enum f => f -> Eff (Fresh f ': r) a -> Eff r (a, f)
runFresh f0 = runState f0 . runFreshAsState

-- | Just like @'runFresh'@, but it doesn't return the final value
-- of the global counter at the end.
evalFresh :: forall f r a. Enum f => f -> Eff (Fresh f ': r) a -> Eff r a
evalFresh f0 = evalState f0 . runFreshAsState

-- | Implement a @'Fresh' f@ effect in terms of @'State' f@
runFreshAsState :: forall f r a. Enum f => Eff (Fresh f ': r) a -> Eff (State f ': r) a
runFreshAsState = reinterpret @'[State f] $ \case 
  Fresh-> get >>= \s -> put (succ s) *> pure s
