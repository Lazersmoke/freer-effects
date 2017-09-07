{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
-- | Composable coroutine effects layer.
--
-- An effect to compose functions with the ability to yield.
module Control.Monad.Freer.Coroutine
  (
  -- * Yield Control
   Yield(..)
  ,yield

  -- * Handle Yield Effect
  ,Status(..)
  ,runC
  ,interposeC
  ,replyC
  )
  where

import Control.Monad.Freer


-- | Sending a @'Yield' a b c @ effect means this coroutine has produced
-- an @a@, and needs a @b@ to go on to produce a @c@.
data Yield a b c = Yield a (b -> c) deriving Functor

-- | Build a @'Yield'@ out of a value that we are producing, and a pure
-- function as a continuation.
yield :: Member (Yield a b) r => a -> (b -> c) -> Eff r c
yield x f = send (Yield x f)

-- | Represents status of a coroutine.
data Status r a b f
  = Done f
  -- ^ Coroutine is done and has returned an @f@.
  | Continue a (b -> Eff r (Status r a b f))
  -- ^ This means the coroutine has @'Yield'@ed an @a@ value, and now
  -- it needs a @b@ value to go on to produce its next status.

-- | Convert a @'Yield' a b c@ into an effect that returns a @'Continue'@.
-- This tells the user of the effect that we have provided an @a@, and
-- expect a @b@ in order to execute the continuation.
replyC
  :: (c -> Eff r (Status r a b f))
  -> Yield a b c
  -> Eff r (Status r a b f)
replyC arr (Yield a k) = pure $ Continue a (arr . k)

-- | Run a coroutine
runC :: Eff (Yield a b ': r) f -> Eff r (Status r a b f)
runC = handleRelay (pure . Done) replyC

-- | Launch a coroutine and report its status, without handling (removing)
-- @'Yield'@ from the effects. This is useful for reducing nested coroutines.
interposeC
  :: Member (Yield a b) r
  => Eff r f
  -> Eff r (Status r a b f)
interposeC = interpose (pure . Done) replyC
