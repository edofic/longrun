module TestLongrun (
    testLongrun
) where

import Control.Monad (forM_, forever)
import Control.Monad.IO.Class (liftIO)
import System.Random (newStdGen, randoms)
import Test.Framework (Test, buildTest, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Utils (assertConstantMemory, testLogsOfMatch, runAppWithoutLogging)

import Control.Concurrent.Longrun

testLongrun :: Test
testLongrun = testGroup "test Longrun"
    [ testChvar
    , testLong
    ]


-- | Variable change
chvar :: Process ()
chvar = do
    var <- newVar "var" (1::Int)
    _ <- onChangeVar "observer" 1 (GetEnd var) id $ \oldVal newVal -> do
        logM INFO $ "Variable changed " ++ show oldVal ++ " -> " ++ show newVal

    sleep 0.01
    setVar' var 2
    sleep 0.01
    setVar' var 3
    sleep 0.01

testChvar :: Test
testChvar = testLogsOfMatch "variable change" INFO chvar
    [ (INFO, "Variable changed 1 -> 2")
    , (INFO, "Variable changed 2 -> 3")
    ]


testLong :: Test
testLong = buildTest $ fmap (testCase "long queue pipe") $
    runAppWithoutLogging $ do
        stdGen <- liftIO newStdGen
        q <- newQueue1 "q"

        _src <- spawnProcess "source" nop $
            forM_ (randoms stdGen :: [Int]) $ \x -> do
                writeQueue' q x
                sleep 0.00001

        _sink <- spawnProcess "sink" nop $ forever $ do
            _ <- readQueue' q
            return ()

        assertion <- assertConstantMemory 500 3 $ do
            sleep 0.001

        stop_ _sink
        stop_ _src
        return assertion
