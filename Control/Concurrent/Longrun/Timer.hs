-----------------------------------------------------------
-- |
-- Module       : Control.Concurrent.Longrun.Timer
-- Copyright    : (c) Zoran Bošnjak 2016
-- License      : GLPv3
--
-- Functions for long running process handling (Timer).
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

module Control.Concurrent.Longrun.Timer where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.STM as STM

import Control.Concurrent.Longrun.Base
import Control.Concurrent.Longrun.Subprocess

data Timer = Timer
    { tParent   :: ThreadId
    , tName     :: ProcName
    , tTimeout  :: Double
    , tAction   :: Process ()
    , tRunning  :: STM.TVar (Maybe (Subprocess ()))
    , tExpired  :: STM.TVar Bool
    }

-- | Create new timer.
newTimer :: ProcName -> Double -> Process () -> Process Timer
newTimer name seconds action = group name $ do
    trace $ "newTimer, seconds: " ++ show seconds
    parent <- liftIO $ Control.Concurrent.myThreadId
    running <- liftIO $ STM.newTVarIO Nothing
    expired <- liftIO $ STM.newTVarIO False
    return $ Timer
        { tParent = parent
        , tName = name
        , tTimeout = seconds
        , tAction = action
        , tRunning = running
        , tExpired = expired
        }

_manipulateTimer :: Timer -> Process a -> Process a
_manipulateTimer t manipulator = group (tName t) $ do
    -- timer manipulation is only possible from the same parent
    _ <- liftIO $ Control.Concurrent.myThreadId
    -- TODO: for some reason, this assertion leaks memory
    --assert (parent == tParent t) "wrong caller"
    manipulator

_stopTimer :: Timer -> Process Bool
_stopTimer t = do
    (running, expired) <- liftIO $ STM.atomically $ do
        running <- STM.readTVar $ tRunning t
        expired <- STM.readTVar $ tExpired t
        STM.writeTVar (tRunning t) Nothing
        STM.writeTVar (tExpired t) False
        return (running, expired)
    case running of
        Nothing -> return False
        Just a -> do
            stop_ a
            return (not expired)

-- | (Re)start timer.
restartTimer :: Timer -> Process Bool
restartTimer t = _manipulateTimer t $ do
    parent <- liftIO $ Control.Concurrent.myThreadId
    wasRunning <- _stopTimer t

    -- start delayed task
    d <- spawnTask "delayed" $ ungroup $ do
        sleep $ tTimeout t
        trace "timer expired"
        liftIO $ STM.atomically $ STM.writeTVar (tExpired t) True
        mask_ $ tAction t `onFailureSignal` parent

    liftIO $ STM.atomically $ STM.writeTVar (tRunning t) $ Just d
    trace $ "restartTimer, was running: " ++ show wasRunning
    return wasRunning

-- | Stop timer.
stopTimer :: Timer -> Process Bool
stopTimer t = _manipulateTimer t $ do
    wasRunning <- _stopTimer t
    trace $ "stopTimer, was running: " ++ show wasRunning
    return wasRunning

-- | Expedite timer expire if running.
expireTimer :: Timer -> Process Bool
expireTimer t = _manipulateTimer t $ do
    parent <- liftIO $ Control.Concurrent.myThreadId
    wasRunning <- _stopTimer t
    when wasRunning $ do
        _ <- spawnTask "action" $ ungroup $ do
            mask_ $ tAction t `onFailureSignal` parent
        return ()
    trace $ "expireTimer, was running: " ++ show wasRunning
    return wasRunning

