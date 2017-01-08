module TestSubprocess (
    testSubprocess
) where

import Control.Concurrent.Longrun
import Control.Concurrent.STM (atomically, readTVar)
import Control.Monad (when, replicateM_, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import Data.Int (Int64)
import Data.Set as Set
import Test.Framework (Test, buildTest, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Utils (testLogsOfMatch, assertConstantMemory, runAppWithoutLogging)


testSubprocess :: Test
testSubprocess = testGroup "test subprocess"
    [ testTask1
    , testTask2
    , testTaskRepeat
    , testProc
    , testProcProc
    , testAutoStop1
    , testAutoStop2
    , testTerm
    , testInLoop
    ]


-- | Regular task with failure or without
task :: Bool -> Process ()
task failFlag = do
    logM INFO $ "startup: " ++ show failFlag
    t <- spawnTask "test" $ do
        logM INFO "task start"
        sleep 0.1
        when failFlag $ fail "fail request"
        return "OK"
    rv <- waitCatch t
    logM INFO $ "result: " ++ show rv

testTask1 :: Test
testTask1 = testLogsOfMatch "task1" INFO (task False)
    [ (INFO, "startup: False")
    , (INFO, "task start")
    , (INFO, "result: Right \"OK\"")
    ]

testTask2 :: Test
testTask2 = testLogsOfMatch "task2" INFO (task True)
    [ (INFO, "startup: True")
    , (INFO, "task start")
    , (INFO, "result: Left user error (fail request)")
    ]

-- | Repeat running task, observe memory
testTaskRepeat :: Test
testTaskRepeat = buildTest $ runAppWithoutLogging $ do
    counter <- newVar "counter" (0 :: Int64)
    assertion <- assertConstantMemory 100 1.2 $ do
        cnt <- getVar' counter
        setVar' counter $ cnt + 1
        t <- spawnTask ("iteration " ++ show cnt) $ do
            return cnt
        rv <- waitCatch t
        logM INFO $ show rv
    return $ testCase "repeat running a task" assertion


testProc :: Test
testProc = testGroup "regular long running process"
    [ testProcLogs
    , testProcMem
    ]

proc1 :: Process ()
proc1 = do
    logM INFO "startup"
    _ticker <- spawnProcess "tickerProcess" nop $ forever $ do
        logM INFO "tick"
        sleep 0.01
    sleep 0.095
    logM INFO "done"

testProcLogs :: Test
testProcLogs = testLogsOfMatch "regular long running process" INFO proc1 $
    [(INFO, "startup")] ++ replicate 10 (INFO, "tick") ++ [(INFO, "done")]


testProcMem :: Test
testProcMem = buildTest $ runAppWithoutLogging $ do
    _ticker <- spawnProcess "tickerProcess" nop $ forever $ do
        logM INFO "tick"
        sleep 0.00001
    assertion <- assertConstantMemory 100 1.2 $ do
        sleep 0.005
    return $ testCase "simple ticker" assertion


-- | Process in process.
procproc :: Double -> Process ()
procproc factor = do
    logM INFO "startup"
    let f delta = do
            sleep $ delta * factor
            logM INFO $ show delta

    p1 <- spawnProcess "p1" nop $ do
        _p1a <- spawnProcess "a" nop $ forever (f 0.11)
        _p1b <- spawnProcess "b" nop $ forever (f 0.12)
        rest
    p2 <- spawnProcess "p2" nop (forever (f 0.13))

    sleep $ 0.3 * factor
    logM INFO "stop p1"
    stop_ p1
    sleep $ 0.2 * factor
    logM INFO "stop p2"
    stop_ p2


testProcProc :: Test
testProcProc = testGroup "process in process"
    [ testProcProcLogs
    , testProcProcMem
    ]

testProcProcLogs :: Test
testProcProcLogs = testLogsOfMatch "process in process" INFO (procproc 0.1)
    [ (INFO, "startup")
    , (INFO, "0.11")
    , (INFO, "0.12")
    , (INFO, "0.13")
    , (INFO, "0.11")
    , (INFO, "0.12")
    , (INFO, "0.13")
    , (INFO, "stop p1")
    , (INFO, "0.13")
    , (INFO, "stop p2")
    ]

testProcProcMem :: Test
testProcProcMem = buildTest $ runAppWithoutLogging $
    fmap (testCase "spawning suprocesses in constant memory") $
        assertConstantMemory 100 1.2 $
            procproc 0.001

-- | Implicit children stop (ver1)
autoStop1 :: Process ()
autoStop1 = do
    logM INFO "start"
    t <- spawnTask "task" $ do
        _ <- spawnTask "subtask" $ do
            sleep 0.002
            logM INFO "this should not be displayed"
        sleep 0.002
        logM INFO "this should also not be displayed"
    c1 <- asks procChilds >>= liftIO . atomically . readTVar
    logM INFO $ "c1: " ++ show (Set.size c1)
    sleep 0.001
    stop_ t
    c2 <- asks procChilds >>= liftIO . atomically . readTVar
    logM INFO $ "c2: " ++ show (Set.size c2)
    sleep 0.005
    logM INFO "done"

testAutoStop1 :: Test
testAutoStop1 = testLogsOfMatch "implicit children stop" INFO autoStop1
    [ (INFO, "start")
    , (INFO, "c1: 1")
    , (INFO, "c2: 0")
    , (INFO, "done")
    ]

-- | Implicit children stop (ver2)
autoStop2 :: Process ()
autoStop2 = do
    logM INFO "start"
    t <- spawnTask "task" $ do
        _ <- spawnTask "subtask" $ do
            sleep 0.005
            logM INFO "this should not be displayed"
        sleep 0.001
        _ <- fail "oops"
        rest
    sleep 0.01
    c1 <- asks procChilds >>= liftIO . atomically . readTVar
    logM INFO $ "c1: " ++ show (Set.size c1)

    _ <- waitCatch t
    c2 <- asks procChilds >>= liftIO . atomically . readTVar
    logM INFO $ "c2: " ++ show (Set.size c2)

testAutoStop2 :: Test
testAutoStop2 = testLogsOfMatch "implicit children stop v2" INFO autoStop2
    [ (INFO, "start")
    , (INFO, "c1: 1")
    , (INFO, "c2: 0")
    ]

-- | Termination
term :: Process ()
term = do
    _ <- spawnProcess "proc" (logM ERROR "termination") $ do
        logM INFO "will stop now"
    rest

testTerm :: Test
testTerm = testLogsOfMatch "stop on sub-proc termination" INFO term
    [ (INFO, "will stop now")
    , (ERROR, "termination")
    ]


inLoop1 :: Process ()
inLoop1 = do
    t <- spawnTask "task" $ do
        sleep 0.01
        logM INFO "task done"

    replicateM_  100 $ do
        sleep 0.001
        stop_ t


testInLoop :: Test
testInLoop = testGroup "test stop in loop"
    [ testInLoopLogs
    , testInLoopMem
    ]


testInLoopLogs :: Test
testInLoopLogs = testLogsOfMatch "logs" INFO inLoop1
    []

testInLoopMem :: Test
testInLoopMem = buildTest $ fmap (testCase "constant memory" ) $
    runAppWithoutLogging $ assertConstantMemory 100 1.2 $ do
        t <- spawnTask "task" $ do
            sleep 0.001
            nop
        sleep 0.00001
        stop_ t
