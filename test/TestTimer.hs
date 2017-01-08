module TestTimer (
    testTimer
) where

import Control.Monad (forever)
import System.Log.Logger (Priority(INFO))
import Test.Framework (Test, testGroup, buildTest)
import Test.Framework.Providers.HUnit (testCase)
import Utils (assertConstantMemory, testLogsOfMatch, runAppWithoutLogging)

import Control.Concurrent.Longrun

testTimer :: Test
testTimer = testGroup "test timer"
    [ testTim1
    , testTim2
    , testTimLoop
    , testTimExpire
    , testTimException
    , testTimRestart
    ]


-- | Basic timer
tim1 :: Process ()
tim1 = do
    t <- newTimer "timer" 0.02 $ do
        logM INFO "done"
    _ <- restartTimer t
    sleep 0.03
    logM INFO "bye"

testTim1 :: Test
testTim1 = testLogsOfMatch "test tim1" INFO tim1
    [ (INFO ,"done")
    , (INFO ,"bye")
    ]


-- | Stop/restart
tim2 :: Process ()
tim2 = do
    t <- newTimer "timer" 0.1 $ do
        logM INFO "done"
    _ <- restartTimer t
    sleep 0.05
    _ <- stopTimer t
    _ <- restartTimer t
    return ()


testTim2 :: Test
testTim2 = testLogsOfMatch "test tim2" INFO tim2
    []


-- | Check memory.
testTimLoop :: Test
testTimLoop = buildTest $ runAppWithoutLogging $ do
    t <- newTimer "timer" 0.01 $ do
        _sub <- spawnProcess "task" nop $ forever $ do
            logM INFO "task"
            sleep 0.01
        sleep 0.03
        logM INFO "done"

    assertion <- assertConstantMemory 100 1.2 $ do
        _ <- restartTimer t
        nop

    return $ testCase "tim loop memory leak" assertion


-- | Expire timer.
timExpire :: Process ()
timExpire = do
    t <- newTimer "timer" 0.02 $ do
        logM INFO "tick"
    logM INFO "start"
    _ <- expireTimer t
    _ <- restartTimer t
    sleep 0.01
    _ <- expireTimer t
    logM INFO "stop"

testTimExpire :: Test
testTimExpire = testLogsOfMatch "test timExpire" INFO timExpire
    [ (INFO,  "start")
    , (INFO,  "stop")
    , (INFO,  "tick")
    ]



-- | Terminate parent.
timException :: Process ()
timException = do
    logM INFO "hello"
    t <- newTimer "timer" 0.01 $ do
        logM INFO "expired"
        fail "terminate"
    sleep 0.001
    _ <- restartTimer t
    sleep 0.02
    return ()

testTimException :: Test
testTimException = testLogsOfMatch "test timException" INFO timException
    [ (INFO, "hello")
    , (INFO, "expired")
    ]


testTimRestart :: Test
testTimRestart = buildTest $ runAppWithoutLogging $ do
    t <- newTimer "timer" 0.001 $ do
        logM INFO "tick"

    assertion <- assertConstantMemory 100 1.2 $ do
        _ <- restartTimer t
        sleep 0.002

    return $ testCase "tim restart memory leak" assertion
