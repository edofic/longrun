module TestWait (
    testWait
) where

import Test.Framework (Test, buildTest, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Utils (assertConstantMemory, testLogsOfMatch)

import qualified Control.Concurrent.Longrun as Longrun

testWait :: Test
testWait = testGroup "Test task sleep"
    [ testTimeout
    , testMem
    ]


procTimeout :: Longrun.Process ()
procTimeout = do
    Longrun.logM Longrun.INFO "start"
    q <- Longrun.newQueue1 "test"
    _ <- Longrun.spawnTask "send" $ do
        Longrun.sleep 0.5
        Longrun.writeQueue' q "hello"
    msg <- Longrun.readQueueTimeout (Longrun.ReadEnd q) 0.6
    Longrun.logM Longrun.INFO $ show msg

testTimeout :: Test
testTimeout = testLogsOfMatch "timeout" Longrun.DEBUG procTimeout
    [ (Longrun.INFO,"start")
    , (Longrun.DEBUG, "newQueue (bounded 1)")
    , (Longrun.DEBUG, "spawnTask")
    , (Longrun.DEBUG, "addChild")
    , (Longrun.DEBUG, "sleep 0.5 seconds")
    , (Longrun.DEBUG, "writeQueue, value: \"hello\"")
    , (Longrun.INFO, "Just \"hello\"")
    ]

procMem :: Longrun.Process ()
procMem = do

    -- case false
    do
        q <- Longrun.newQueue1 "q"
        t <- Longrun.spawnTask "send" $ do
            Longrun.sleep 0.004
            Longrun.writeQueue' q "hello"
        msg <- Longrun.readQueueTimeout (Longrun.ReadEnd q) 0.003
        _ <- Longrun.stop t
        Longrun.logM Longrun.INFO $ show msg

    -- case true
    do
        q <- Longrun.newQueue1 "q"
        t <- Longrun.spawnTask "send" $ do
            Longrun.sleep 0.002
            Longrun.writeQueue' q "hello"
        msg <- Longrun.readQueueTimeout (Longrun.ReadEnd q) 0.003
        _ <- Longrun.stop t
        Longrun.logM Longrun.INFO $ show msg

testMem :: Test
testMem = buildTest $
    fmap (testCase "queue timeout memory leak") $
        Longrun.runApp $ assertConstantMemory 100 1.2 procMem