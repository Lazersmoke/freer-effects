{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- | Reader effects
--
-- A @'Reader' e@ effect is a computation that has access to some
-- shared environment of type @e@. @'Reader' e a@ is isomorphic to
-- @e -> a@.
module Control.Monad.Freer.Reader
  (
  -- * Reader Effect
  Reader(..)
  -- * Reader Operations
  ,ask
  ,asks
  ,local
  -- * Reader Handlers
  ,runReader
  ) where

import Control.Monad.Freer

-- | A @'Reader' e@ effect means an effectful computation has access to some
-- shared environment of type @e@. This environment can /not/ be changed,
-- unlike @'Control.Monad.Freer.State'@. 
--
-- @Eff '['Reader' e] a@ is isomorphic to @e -> a@.
data Reader e a where
  Ask :: Reader e e

-- | Ask for the value of the environment
ask :: Member (Reader e) r => Eff r e
ask = send Ask

-- | Just like @'ask'@, but apply a function to the environment after getting it.
-- Typically you will do this if the environment is a record. For example, if you have:
--
-- @
--   data SomeRecord = MkSomeRecord {someRecordData :: String, someRecordId :: Int}
-- @
--
-- then you can do this:
--
-- @
--   asks someRecordId :: Member (Reader SomeRecord) r => Eff r Int
-- @
--
-- Note that:
--
-- @
--   asks f = f <$> ask
-- @
asks :: Member (Reader e) r => (e -> a) -> Eff r a
asks = (<$> ask)

-- | Handler for 'Reader' effects.
runReader :: e -> Eff (Reader e ': r) a -> Eff r a
runReader e = handleRelay pure (\k Ask -> k e)

-- | Locally rebind the value in the dynamic environment.
-- The environment in the contained compuatation will be
-- modified by the provided function. For example:
-- 
-- @
--   main :: IO ()
--   main = run . runM . runReader "Hi" $ do
--     send . putStrLn =<< ask
--     local (++ "!!!") $ send . putStrLn =<< ask
-- @
--
-- Will print:
--
-- @
--   Hi
--   Hi!!!
-- @
local
  :: forall e a r. Member (Reader e) r
  => (e -> e)
  -> Eff r a
  -> Eff r a
local f m = do
  e <- asks f
  let 
    h :: (v -> Eff r a) -> Reader e v -> Eff r a
    h k Ask = k e
  interpose pure h m
