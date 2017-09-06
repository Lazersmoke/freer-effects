{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
-- | An Exception effect and handler.
--
-- Composable handler for Exception effects. Communicates success\/failure
-- via an 'Either' type.
module Control.Monad.Freer.Exception
    ( Exc(..)
    , throwError
    , runError
    , catchError
    )
  where

import Control.Monad.Freer

-- | Throwing an exception never actually returns a value.
newtype Exc e a = Exc e

-- | Throws an exception of type @e@.
throwError :: Member (Exc e) r => e -> Eff r a
throwError = send . Exc

-- | Handler for exception effects. If there are no exceptions thrown, returns
-- @'Right'@. If exceptions are thrown and not handled, returns @'Left'@, while
-- interrupting the execution of any other effect handlers.
runError :: Eff (Exc e ': r) a -> Eff r (Either e a)
runError = handleRelay 
  (pure . Right) 
  -- Throw out the continuation because we are erroring out.
  (\_k (Exc e) -> pure (Left e))

-- | A catcher for Exceptions. Handlers are allowed to rethrow exceptions.
catchError
    :: Member (Exc e) r
    => Eff r a
    -> (e -> Eff r a)
    -> Eff r a
catchError m handle = interpose pure (\_k (Exc e) -> handle e) m
