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
  ,get
  ,put
  ,modify
  -- * State Handlers
  ,runState
  ,evalState
  ,execState
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
--
-- Note that this function is particularly low-level, and
-- highly unrepresentative of actual user code using this package
runState :: s -> Eff (State s ': r) a -> Eff r (a, s)
runState s = cataEff
  (pure . (,s)) 
  (\k -> \case
    Get -> runState s $ k s
    Put s' -> runState s' $ k ()
  )
  (\q -> Eff (singleton $ runState s . q))

-- | Run a @'State'@ effect, returning only the final state.
execState :: s -> Eff (State s ': r) a -> Eff r s
execState s = cataEff
  (pure . const s)
  (\k -> \case
    Get -> execState s $ k s
    Put s' -> execState s' $ k ()
  )
  (\q -> Eff (singleton $ execState s . q))

-- | Run a @'State'@ effect, discarding the final state.
evalState :: s -> Eff (State s ': r) a -> Eff r a
evalState s = cataEff
  pure
  (\k -> \case
    Get -> evalState s $ k s
    Put s' -> evalState s' $ k ()
  )
  (\q -> Eff (singleton $ evalState s . q))

-- TODO: reimplement transactionState
