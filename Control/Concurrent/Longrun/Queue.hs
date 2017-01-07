-----------------------------------------------------------
-- |
-- Module       : Control.Concurrent.Longrun.Queue
-- Copyright    : (c) Zoran Bošnjak 2016
-- License      : GLPv3
--
-- Functions for long running process handling (Queue).
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

module Control.Concurrent.Longrun.Queue where

import Control.Concurrent.STM
import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (liftIO)

import Control.Concurrent.Longrun.Base

data Queue a = Queue
    { qName     :: ProcName
    , qRead     :: STM a
    , qWrite    :: a -> STM ()
    , qTryPeek  :: STM (Maybe a)
    }

newtype ReadEnd a = ReadEnd (Queue a)
newtype WriteEnd a = WriteEnd (Queue a)

-- | Create new queue.
newQueue :: Maybe Int -> ProcName -> Process (Queue a)
newQueue mBound name = group name $ do
    (readFunc, writeFunc, tryPeekFunc) <- case mBound of
        Nothing -> do
            trace $ "newQueue (unbounded)"
            q <- liftIO $ newTQueueIO
            return $ (readTQueue q, writeTQueue q, tryPeekTQueue q)
        Just bound -> do
            trace $ "newQueue (bounded " ++ show bound ++ ")"
            q <- liftIO $ newTBQueueIO bound
            return $ (readTBQueue q, writeTBQueue q, tryPeekTBQueue q)
    return $ Queue name readFunc writeFunc tryPeekFunc

-- | Create one element bounded queue.
newQueue1 :: ProcName -> Process (Queue a)
newQueue1 = newQueue (Just 1)

-- | Read data from the queue.
readQueue :: (Show a) => ReadEnd a -> Process a
readQueue (ReadEnd q) = group (qName q) $ do
    val <- liftIO $ atomically $ qRead q
    trace $ "readQueue, value: " ++ show val
    return val

-- | Read data from the queue (operate on Queue instead of ReadEnd).
readQueue' :: (Show a) => Queue a -> Process a
readQueue' = readQueue . ReadEnd

-- | Write data to the queue.
writeQueue :: (Show a, NFData a) => WriteEnd a -> a -> Process ()
writeQueue (WriteEnd q) val = group (qName q) $ do
    val' <- force val
    liftIO $ atomically $ (qWrite q) val'
    trace $ "writeQueue, value: " ++ show val'

-- | Write data to the queue (operate on Queue instead of WriteEnd)
writeQueue' :: (Show a, NFData a) => Queue a -> a -> Process ()
writeQueue' = writeQueue . WriteEnd

