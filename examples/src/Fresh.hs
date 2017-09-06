module Fresh where

import Data.Monoid ((<>))

import Control.Monad.Freer.Fresh (evalFresh, fresh)
import Control.Monad.Freer.Trace (runTrace, trace)
import Control.Monad.Freer

-- | Generate two fresh values.
--
-- >>> traceFresh
-- Fresh 0
-- Fresh 1
traceFresh :: IO ()
traceFresh = runM . runTrace . evalFresh (0 :: Int) $ do
    n <- fresh
    trace $ "Fresh " <> show (n :: Int)
    n' <- fresh
    trace $ "Fresh " <> show (n' :: Int)
