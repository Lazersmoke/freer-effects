{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:       Control.Monad.Freer.Trace
-- Description:  Composable Trace effects.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Composable handler for 'Trace' effects. Trace allows one to debug the
-- operation of sequences of effects by outputing to the console.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.Trace
    ( Trace(..)
    , trace
    , runTrace
    )
  where

import Control.Monad.Freer


-- | A Trace effect; takes a 'String' and performs output.
data Trace a where
    Trace :: String -> Trace ()

-- | Printing a string in a trace.
trace :: Member Trace r => String -> Eff r ()
trace = send . Trace

-- | An 'IO' handler for 'Trace' effects.
runTrace :: Member IO r => Eff (Trace ': r) a -> Eff r a
runTrace = runNat $ \(Trace s) -> send (putStrLn s)
