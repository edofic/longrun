-----------------------------------------------------------
-- |
-- Module       : Control.Concurrent.Longrun.Base
-- Copyright    : (c) Zoran Bošnjak 2016
-- License      : GLPv3
--
-- Functions for long running process handling (Subprocess).
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

module Control.Concurrent.Longrun.Subprocess where

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import qualified Control.Concurrent.Async as A
import Control.Monad.Reader.Class (asks, ask)

import Control.Concurrent.Longrun.Base

-- | All subprocesses will automatically terminate,
-- if the parent process terminates.
newtype Subprocess a = Subprocess (A.Async a)
instance Terminator (Subprocess a) where
    getTid (Subprocess a) = A.asyncThreadId a
    terminate (Subprocess a) = A.cancel a

-- | Spawn a child process that eventually returns something.
-- Use waitCatch to get the returned value or error.
spawnTask :: ProcName -> Process a -> Process (Subprocess a)
spawnTask name action = group name $ do
    trace "spawnTask"
    pName <- asks procName
    cfg <- mkChildConfig pName
    lock <- liftIO $ newEmptyMVar
    a <- liftIO $ A.async $ do
        -- Need to wait for parent to finish updating its state
        _ <- takeMVar lock
        runProcess cfg action
    addChild $ Child (Subprocess a)
    liftIO $ putMVar lock ()
    return $ Subprocess a

-- | Wait for a process to teminate.
waitCatch :: Subprocess a -> Process (Either SomeException a)
waitCatch (Subprocess a) = do
    trace "waitCatch"
    rv <- liftIO $ A.waitCatch a
    removeChild $ Child (Subprocess a)
    return rv

-- | Spawn a subprocess that shall not terminate by itself.
spawnProcess :: ProcName -> Process b -> Process a -> Process (Subprocess ())
spawnProcess name onExit action = do
    group name $ trace "spawnProcess"
    parent <- liftIO $ Control.Concurrent.myThreadId
    a <- spawnTask name $ do
        cfg <- ask
        b <- liftIO $ A.async $ runProcess cfg action
        addChild $ Child (Subprocess b)
        _ <- liftIO $ A.waitCatch b
        trace $ "process terminated"
        _ <- onExit
        removeChild $ Child (Subprocess b)
        liftIO $ Control.Concurrent.killThread parent
    return a

-- | Stop a subprocess, remove it from the list of childs.
-- Return running status, just before call to stop.
stop :: Subprocess a -> Process Bool
stop (Subprocess a) = do
    trace "stop"
    removeChild $ Child (Subprocess a)
    liftIO $ A.cancel a
    rv <- liftIO $ A.waitCatch a
    case rv of
        Left _ -> return True
        Right _ -> return False

-- | Stop a subprocess, don't care about running state
stop_ :: Subprocess a -> Process ()
stop_ p = stop p >>= \_ -> return ()

