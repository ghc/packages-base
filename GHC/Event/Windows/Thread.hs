{-# LANGUAGE NoImplicitPrelude #-}
module GHC.Event.Windows.Thread (
    ensureIOManagerIsRunning,
    threadDelay,
    registerDelay,
) where

import Control.Monad
import Data.Maybe
import GHC.Conc.Sync
import GHC.Event.Windows
import GHC.Base
import GHC.IO
import GHC.MVar
import GHC.Real

ensureIOManagerIsRunning :: IO ()
ensureIOManagerIsRunning = void getSystemManager

threadDelay :: Int -> IO ()
threadDelay usecs = mask_ $ do
    m <- newEmptyMVar
    Just mgr <- getSystemManager
    reg <- registerTimeout mgr secs $ putMVar m ()
    takeMVar m `onException` unregisterTimeout mgr reg
  where
    secs = microsecondsToSeconds usecs

registerDelay :: Int -> IO (TVar Bool)
registerDelay usecs = do
    t <- newTVarIO False
    Just mgr <- getSystemManager
    _ <- registerTimeout mgr secs $ atomically $ writeTVar t True
    return t
  where
    secs = microsecondsToSeconds usecs

microsecondsToSeconds :: Int -> Seconds
microsecondsToSeconds us = fromIntegral us / 1000000.0
