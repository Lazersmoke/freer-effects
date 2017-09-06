{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
-- | State effects in terms of Reader and Writer.
--
-- Composable handler for @'State'@ effects in terms of @'Reader'@ and @'Writer'@
-- effects. This module is more a tutorial on how to compose handlers. It is
-- slightly slower than a dedicated @'State'@ handler.
module Control.Monad.Freer.StateRW
  (combineWriteRead
  ) where

import Control.Monad.Freer.Reader
import Control.Monad.Freer.Writer
import Control.Monad.Freer.State
import Control.Monad.Freer


-- | Combine a @'Writer' s@ and a @'Reader' s@ into a @'State' s@, inefficiently.
-- The semantics for this is that @'ask'@ will get you the most recent @'tell'@'d value.
combineWriteRead :: Member (State s) r => Eff (Writer s ': Reader s ': r) a -> Eff r a
combineWriteRead = runNat (\Ask -> get) . runNat (\(Tell s) -> put s)
