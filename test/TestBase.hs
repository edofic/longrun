module TestBase (
    testBase
) where

import Control.Monad (forever)
import Control.Concurrent.Longrun
import Test.Framework (Test, buildTest, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Utils (assertConstantMemory, runAppWithoutLogging)


testBase :: Test
testBase = testGroup "test subprocess"
    [ testForeverProcess
    ]

testForeverProcess :: Test
testForeverProcess = buildTest $ runAppWithoutLogging $ do
    _ <- spawnProcess "p1" nop $ forever $ do
      logM INFO "tick"
      sleep 0.000001
    assertion <- assertConstantMemory 100 1.2 $
      sleep 0.001
    return $ testCase "Process using `forever` in constant memory"
                      assertion
