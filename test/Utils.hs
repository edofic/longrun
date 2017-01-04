module Utils where

import Control.Exception (SomeException, catch)
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

resetLogger :: Logger.Priority -> IO ()
resetLogger priority = do
    Logger.updateGlobalLogger Logger.rootLoggerName
        (Logger.setLevel priority . Logger.removeHandler)

captureLogs :: Logger.Priority -> IO a -> IO ([LogRecord], Either SomeException a)
captureLogs priority computation = do
    resetLogger priority
    handler <- inMemoryLogger
    Logger.updateGlobalLogger Logger.rootLoggerName (Logger.addHandler handler)
    a <- catch (Right <$> computation) (return . Left)
    logs <- evacuateLogger handler
    resetLogger Logger.DEBUG
    return (logs, a)

assertLogsWithoutTimeEqual :: [LogRecord] -> [LogRecord] -> Assertion
assertLogsWithoutTimeEqual value expected =
    expected @=? fmap f value where
       f (priority, msg) = (priority, msg') where
           msg' = dropWhileEnd isThrowaway $ dropWhileEnd (/= '@') msg
           isThrowaway c = isSpace c || (c == '@')

testLogsOfMatch :: String -> Logger.Priority -> Process () -> [LogRecord] -> Test
testLogsOfMatch name priority proc expected = buildTest $ do
    (logs, _) <- captureLogs priority $ runApp proc
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
        then return $ assertString $
          "Memory leak found in iteration  " ++ show i ++ " " ++ show (after, upperBound)
        else go (i-1) upperBound
