{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Trace where

import Data.Monoid ((<>))

import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Trace


-- Higher-order effectful function
-- The inferred type shows that the Trace affect is added to the effects
-- of r
mapMdebug:: (Show a, Member Trace r) => (a -> Eff r b) -> [a] -> Eff r [b]
mapMdebug _ [] = pure []
mapMdebug f (h:t) = do
  trace $ "mapMdebug: " <> show h
  h' <- f h
  t' <- mapMdebug f t
  pure (h':t')

tMd :: IO [Int]
tMd = runM . runTrace . runReader (10::Int) $ mapMdebug f [1..5]
  where f x = (+) <$> ask <*> pure x
{-
mapMdebug: 1
mapMdebug: 2
mapMdebug: 3
mapMdebug: 4
mapMdebug: 5
[11,12,13,14,15]
-}

-- duplicate layers
tdup :: IO ()
tdup = runM . runTrace . runReader (10::Int) $ do
  runReader (20::Int) talk
  talk

talk :: Members '[Trace,Reader Int] r => Eff r ()
talk = do
  v <- ask
  trace $ "AskedL: " <> show (v :: Int)
{-
Asked: 20
Asked: 10
-}
