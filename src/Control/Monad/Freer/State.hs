{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
-- | State effects, for state-carrying computations.
--
-- Some computations may not require the full power of @'State'@ effect:
--
-- * For a read-only state, see "Control.Monad.Freer.Reader".
-- * To accumulate a value without using it on the way, see
--   "Control.Monad.Freer.Writer".
module Control.Monad.Freer.State
    (
    -- * State Effect
      State(..)
    -- * State Operations
    , get
    , put
    , modify
    -- * State Handlers
    , runState
    , evalState
    , execState
    )
  where

import Control.Monad.Freer

-- | Strict @'State'@ effect: one can either @'Get'@ the current state or @'Put'@ a new one.
data State s a where
    Get :: State s s
    Put :: !s -> State s ()

-- | Get the current state
get :: Member (State s) r => Eff r s
get = send Get

-- | Set the state
put :: Member (State s) r => s -> Eff r ()
put = send . Put

-- | Modify the state using the function
modify :: Member (State s) r => (s -> s) -> Eff r ()
modify f = fmap f get >>= put

-- | Handler for @'State'@ effects. Given an initial state,
-- it provides the pair of the result and the final state.
runState :: s -> Eff (State s ': r) a -> Eff r (a, s)
runState = handleRelayS (\s -> pure . (,s)) $ \s k -> \case
  Get -> k s s
  Put s' -> k s' ()

-- | Run a @'State'@ effect, returning only the final state.
execState :: s -> Eff (State s ': r) a -> Eff r s
execState s0 es = snd <$> runState s0 es

-- | Run a @'State'@ effect, discarding the final state.
evalState :: s -> Eff (State s ': r) a -> Eff r a
evalState s0 es = fst <$> runState s0 es

-- TODO: reimplement transactionState
