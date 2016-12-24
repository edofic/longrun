module Utils where

import Control.Concurrent.Longrun (Process, runApp)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import InMemoryLogger (inMemoryLogger, evacuateLogger)
import System.Environment (lookupEnv)
import System.Log (LogRecord)
import Test.Framework (Test)
import Test.Framework (buildTest)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?), Assertion, assertString)
import qualified GHC.Stats as Stats
import qualified System.Log.Logger as Logger
import qualified System.Mem as Mem

resetLogger :: IO ()
resetLogger = do
    Logger.updateGlobalLogger Logger.rootLoggerName
        (Logger.setLevel Logger.DEBUG . Logger.removeHandler)

captureLogs :: IO a -> IO ([LogRecord], a)
captureLogs computation = do
    resetLogger
    handler <- inMemoryLogger
    Logger.updateGlobalLogger Logger.rootLoggerName (Logger.addHandler handler)
    a <- computation
    logs <- evacuateLogger handler
    resetLogger
    return (logs, a)

assertLogsWithoutTimeEqual :: [LogRecord] -> [LogRecord] -> Assertion
assertLogsWithoutTimeEqual value expected =
    expected @=? fmap f value where
       f (priority, msg) = (priority, msg') where
           msg' = dropWhileEnd isThrowaway $ dropWhileEnd (/= '@') msg
           isThrowaway c = isSpace c || (c == '@')

testLogsOfMatch :: String -> Process () -> [LogRecord] -> Test
testLogsOfMatch name proc expected = buildTest $ do
    (logs, _) <- captureLogs $ runApp proc
    return $ testCase name $
        assertLogsWithoutTimeEqual logs expected

getUsedMemory :: IO Int64
getUsedMemory = do
  Mem.performGC  -- stats are only refresed after a GC cycle
  Stats.currentBytesUsed <$> Stats.getGCStats

assertConstantMemory :: MonadIO m => Int64 -> Double -> m a -> m Assertion
assertConstantMemory baseIterations maxRatio block = do
    multiplier <- liftIO $ lookupEnv "ITERATION_MULTIPLIER"
    _ <- block
    afterOne <- liftIO $ getUsedMemory
    let iterations = baseIterations * (read $ fromMaybe "1" multiplier)
        upperBound = round $ fromIntegral afterOne * maxRatio
    go iterations upperBound
    where
    go i upperBound | i <= 0 = return $ assertString ""
                    | otherwise       = do
        _ <- block
        after <- liftIO $ getUsedMemory
        if after > upperBound
        then return $ assertString $ "Memory leak found: " ++ show (after, upperBound)
        else go (i-1) upperBound
