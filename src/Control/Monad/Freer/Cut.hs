{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
-- | An implementation of logical Cut.
--
-- Composable handler for logical Cut effects. Implemented in terms of 'Exc'
-- effect.
module Control.Monad.Freer.Cut
  (CutFalse(..)
  ,cutFalse
  ) where

import Control.Monad.Freer.Exception (Exc, throwError)
import Control.Monad.Freer


data CutFalse = CutFalse

-- | Implementation of logical Cut using Exc effects.
cutFalse :: Member (Exc CutFalse) r => Eff r a
cutFalse = throwError CutFalse
