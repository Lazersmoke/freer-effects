{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
module Console
    ( Console
    , exitSuccess'
    , getLine'
    , putStrLn'
    , runConsole
    --, runConsolePure
    )
  where

import System.Exit (exitSuccess)

import Control.Monad.Freer

-- | The primitive actions for a @'Console'@ are:
--
-- * Print a @'String'@
-- * Get a @'String'@ (a line)
-- * Exit the program
data Console s where
    PutStrLn    :: String -> Console ()
    GetLine     :: Console String
    ExitSuccess :: Console ()

-- | Print the string
putStrLn' :: Member Console r => String -> Eff r ()
putStrLn' = send . PutStrLn

-- | Get a line from the user
getLine'  :: Member Console r => Eff r String
getLine' = send GetLine

-- | Exit successfully
exitSuccess' :: Member Console r => Eff r ()
exitSuccess' = send ExitSuccess

-- | Interpret the effect by:
--
-- * Printing to stdout
-- * Getting from stdin
-- * Exiting by doing nothing
runConsole :: Member IO r => Eff (Console ': r) a -> Eff r a
runConsole = interpret $ \case
  PutStrLn msg -> send (putStrLn msg)
  GetLine -> send getLine
  ExitSuccess -> send exitSuccess

-- | Interpret the effect by:
--
-- * Adding a line to the output list
-- * Getting a line from the input list (throws error if not enough input)
-- * Exiting by throwing out the rest of the input and halting
--runConsolePure :: [String] -> Eff (Console ': r) a -> Eff r (a,Maybe ([String],[String]))
--runConsolePure inputs = handleRelay (\a -> (a,(inputs,[]))) $ \q -> \case
  --PutStrLn msg -> 
    
