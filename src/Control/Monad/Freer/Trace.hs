{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
-- | Composable Trace effects.
--
-- Composable handler for 'Trace' effects. Trace allows one to debug the
-- operation of sequences of effects by outputing to the console.
module Control.Monad.Freer.Trace
  (Trace
  ,trace
  ,runTrace
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Writer

-- | A @'Trace'@ effect is a @'Writer' 'String'@. 
-- It can be interpreted by @'runTrace'@ as a logging effect.
type Trace = Writer String

-- | @'Trace'@ the given @'String'@ for later printing by @'runTrace'@.
trace :: Member Trace r => String -> Eff r ()
trace = tell

-- | Log all the @'trace'@'d @'String'@s to stdout.
runTrace :: Member IO r => Eff (Trace ': r) a -> Eff r a
runTrace = runNat $ \(Tell s) -> send (putStrLn s)
