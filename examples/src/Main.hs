{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Monad (forever, when)
import Data.List (intercalate, lookup, map, null)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import System.Environment (getArgs)

import Control.Monad.Freer

import Capitalize
import Console
--import Coroutine
--import Cut
--import Fresh
--import Trace

capitalizingService :: (Member Console r, Member Capitalize r) => Eff r ()
capitalizingService = forever $ do
  putStrLn' "Send something to capitalize..."
  l <- getLine'
  when (null l) exitSuccess'
  capitalize l >>= putStrLn'

mainPure :: IO ()
mainPure = runM
  . runConsole
  -- . runConsolePure ["cat", "fish", "dog", "bird", ""]
  $ runCapitalize capitalizingService

mainConsoleA :: IO ()
mainConsoleA = runM (runConsole (runCapitalize capitalizingService))
--             |     |             |              |
--      IO () -'     |             |              |
--     Eff '[IO] () -'             |              |
--          Eff '[Console, IO] () -'              |
--             Eff '[Capitalize, Console, IO] () -'

mainConsoleB :: IO ()
mainConsoleB = runM (runCapitalize (runConsole capitalizingService))
--             |     |             |              |
--      IO () -'     |             |              |
--     Eff '[IO] () -'             |              |
--       Eff '[Capitalize, IO] () -'              |
--             Eff '[Console, Capitalize, IO] () -'

examples :: [(String, IO ())]
examples =
    [ ("pure", mainPure)
    , ("consoleA", mainConsoleA)
    , ("consoleB", mainConsoleB)
    ]

main :: IO ()
main = getArgs >>= \case
    [x] -> fromMaybe e $ lookup x examples
    _ -> e
  where
    e = putStrLn msg
    msg = "Usage: prog [" <> intercalate "|" (map fst examples) <> "]"
