-----------------------------------------------------------
-- |
-- Module       : Control.Concurrent.Longrun
-- Copyright    : (c) Zoran Bošnjak 2016
-- License      : GLPv3
--
-- Functions for long running process handling.
--
-- Maintainer   : Zoran Bošnjak <zoran.bosnjak@via.si>
--
-- This file is part of Longrun.
--
-- Longrun is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Longrun is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Longrun. If not, see <http://www.gnu.org/licenses/>.
--
-----------------------------------------------------------

module Control.Concurrent.Longrun
    ( onChangeVar
    , readQueueTimeout
    , readQueueTimeout'
    , module Control.Concurrent.Longrun.Base
    , module Control.Concurrent.Longrun.Queue
    , module Control.Concurrent.Longrun.Subprocess
    , module Control.Concurrent.Longrun.Timer
    , module Control.Concurrent.Longrun.Variable
    ) where

import Control.Concurrent.Async (async, cancel)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.STM as STM

import Control.Concurrent.Longrun.Base
import Control.Concurrent.Longrun.Subprocess
import Control.Concurrent.Longrun.Variable
import Control.Concurrent.Longrun.Queue
import Control.Concurrent.Longrun.Timer

-- | Run single action on each variable change.
onChangeVar :: (Eq b) =>
    String -> b -> GetEnd a -> (a->b) -> (b -> b -> Process ()) -> Process Child
onChangeVar procname initial (GetEnd (Var varname var)) f act = group procname $
    do
        trace $ "onChangeVar " ++ show varname

        -- We want to block indefinitely if variable is never changed
        -- need another dummy thread to reference 'var',
        -- just to prevent deadlock detection.
        -- TODO: replace this ugly solution to keep var reference alive
        _ <- spawnProcess "dummy" nop $ forever $ do
            sleep $ case (var==var) of
                True -> 1
                False -> 2
        p <- ungroup $ spawnProcess procname nop $ loop initial
        return $ Child p
      where
        loop x = do
            y <- liftIO $ STM.atomically $ do
                y <- STM.readTVar var >>= return . f
                case y == x of
                    True -> STM.retry
                    False -> return y
            trace $ "variable " ++ show varname ++ " changed, triggering action"
            act x y
            loop y

-- | Return (Just msg) or Nothing on timeout.
readQueueTimeout :: ReadEnd a -> Double -> Process (Maybe a)
readQueueTimeout (ReadEnd q) timeout = liftIO $ do
    expired <- STM.newTVarIO False
    task <- async $ do
        threadDelaySec timeout
        STM.atomically $ STM.writeTVar expired True
    ready <- STM.atomically $ do
        val <- qTryPeek q
        end <- STM.readTVar expired
        case (end, val) of
            (True, _) -> return False
            (False, Just _) -> return True
            _ -> STM.retry
    cancel task
    case ready of
        False -> return Nothing
        True -> do
            msg <- STM.atomically $ qRead q
            return $ Just msg

readQueueTimeout' :: Queue a -> Double -> Process (Maybe a)
readQueueTimeout' q = readQueueTimeout (ReadEnd q)

