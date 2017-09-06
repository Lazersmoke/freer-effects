{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- | Composable Writer effects.
--
-- @'Writer'@ effects, for writing\/appending values (line count, list of
-- messages, etc.) to an output. Current value of the @'Writer'@ effect output is not
-- accessible to the computation.
module Control.Monad.Freer.Writer
  (Writer(..)
  ,tell
  ,runWriter
  ) where

import Data.Monoid ((<>))

import Control.Monad.Freer


-- | A @'Writer' w@ effect can write messages to be handled later.
-- Common use cases are logging, pretty-printing, and builing up data structures.
data Writer w a where
    Tell :: w -> Writer w ()

-- | @'Tell'@ a message to the effect.
tell :: Member (Writer w) r => w -> Eff r ()
tell = send . Tell

-- | Handle the messages by gluing them together with the provide @'Monoid'@ instance.
runWriter :: Monoid w => Eff (Writer w ': r) a -> Eff r (a, w)
runWriter = handleRelay
  (pure . (,mempty)) 
  (\k (Tell w) -> (\(a,b) -> (a,w <> b)) <$> k ())
