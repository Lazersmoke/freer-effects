module Tests.Loop (tests) where

{-
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.QSemN (newQSemN, signalQSemN, waitQSemN)
import Control.Monad (forever)
import Data.Function (fix)

import Test.QuickCheck
import Control.Monad.Freer
-}
import Test.Hspec



tests :: Spec
tests = describe "Looping tests" $ it "Doesn't loop forever" $ pendingWith "NYI; need to figure out looping options for HSpec"

{- localOption timeout $ testGroup "Loop tests"
    [ testCase "fix loop" $ testLoop fixLoop
    , testCase "tail loop" $ testLoop tailLoop
    , testCase "forever loop" $ testLoop foreverLoop
    ]
  where
    timeout = mkTimeout 1000000

testLoop :: (IO () -> Eff '[IO] ()) -> IO ()
testLoop loop = do
    s <- newQSemN 0
    t <- forkIO . runM . loop $ signalQSemN s 1
    waitQSemN s 5
    killThread t

fixLoop :: Member IO r => IO () -> Eff r ()
fixLoop action = fix $ \fxLoop -> do
    send action
    fxLoop

tailLoop :: Member IO r => IO () -> Eff r ()
tailLoop action = let loop = send action >> loop in loop

foreverLoop :: Member IO r => IO () -> Eff r ()
foreverLoop action = forever $ send action
-}
