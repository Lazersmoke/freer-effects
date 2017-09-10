{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
-- | An implementation of logical Cut.
--
-- A logical cut is the same an uncatchable exception; it causes the program to die.
module Control.Monad.Freer.Cut
  (CutFalse(..)
  ,cutFalse
  ) where

import Control.Monad.Freer.Exception (Exc, throwError)
import Control.Monad.Freer

-- | The exception to be thrown when we cut.
data CutFalse = CutFalse

-- | Implementation of logical Cut using Exc effects.
cutFalse :: Member (Exc CutFalse) r => Eff r a
cutFalse = throwError CutFalse
