{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
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

-- | The @'Fresh' f@ effect keeps a global counter of @f@ that you
-- can pull fresh values from as an effect.
data Fresh f a where
  Fresh :: Fresh f f

-- | Get a fresh @f@ from the global counter
fresh :: Member (Fresh f) r => Eff r f
fresh = send Fresh

-- | Handles a @'Fresh' f@ effect in the expected way. Also returns
-- the final value of the global counter (the next to be dispensed) when it is done.
runFresh :: Enum f => f -> Eff (Fresh f ': r) a -> Eff r (a, f)
runFresh = handleRelayS
  -- Put the next value in the output
  (\s -> pure . (,s)) 
  -- Put the next value in the output
  (\s k Fresh -> (k $! succ s) s)

-- | Just like @'runFresh'@, but it doesn't return the final value
-- of the global counter at the end.
evalFresh :: Enum f => f -> Eff (Fresh f ': r) a -> Eff r a
evalFresh = runNatS $ \ !s -> \case
  -- Put the next global state back in the counter,
  -- and give the count we just got to the user.
  Fresh -> pure (succ s,s)
