{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module GHC.Event.Windows.Worker (
    Worker,
    new,
    enqueue,

    -- * Helpers
    forkOSUnmasked,
) where

import Control.Monad        (forever, void)
import Data.IORef
import GHC.Base
import GHC.Conc.Sync
import GHC.IO
import GHC.MVar

data Worker = Worker
    { workerJobs :: !(IORef JobList)
    , workerWake :: !(MVar ())
    }

instance Eq Worker where
    Worker a _ == Worker b _ = a == b

-- | Fork an OS thread, and return a handle for sending jobs to it.
new :: IO Worker
new = do
    workerJobs <- newIORef id
    workerWake <- newEmptyMVar
    _ <- forkOSUnmasked $ forever $ do
        takeMVar workerWake
        jobs <- atomicModifyIORef workerJobs $ \jobs -> (id, jobs)
        runJobList jobs
    return Worker{..}

-- | Add a job to the work queue.  Jobs are executed in the order they are
-- queued, and every job is run in the same OS thread.
--
-- A job should not block for long or throw an exception, as this will prevent
-- future jobs from running.
--
-- Exception safety:  atomic, non-interruptible
enqueue :: Worker -> IO () -> IO ()
enqueue Worker{..} io =
    mask_ $ do
        !() <- atomicModifyIORef workerJobs $ \jobs -> (snocJobList jobs io, ())
        void $ tryPutMVar workerWake ()

------------------------------------------------------------------------
-- Helpers

forkOSUnmasked :: IO () -> IO ThreadId
forkOSUnmasked = forkOSMasked . unsafeUnmask

-- A difference list, but with (x >>) instead of (x :)
type JobList = IO () -> IO ()

-- | Append an action to the job list, so it will
-- run /after/ the existing actions.
snocJobList :: JobList -> IO () -> JobList
snocJobList dl io = dl . (io >>)

runJobList :: JobList -> IO ()
runJobList dl = dl (return ())
