{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
module GHC.Event.Windows (
    -- * Manager
    Manager,
    new,
    getSystemManager,

    -- * Overlapped I/O
    associateHandle,
    withOverlapped,
    StartCallback,
    CompletionCallback,
    Overlapped(..),

    -- * Timeouts
    TimeoutCallback,
    TimeoutKey,
    Seconds,
    registerTimeout,
    updateTimeout,
    unregisterTimeout,
) where

import GHC.Event.Windows.Clock   (Clock, Seconds, getClock, getTime)
import GHC.Event.Windows.FFI     (Overlapped(..))
import GHC.Event.Windows.Worker  (Worker, forkOSUnmasked)
import qualified GHC.Event.Windows.FFI    as FFI
import qualified GHC.Event.Windows.Worker as Worker
import qualified GHC.Event.PSQ            as Q

import Control.Exception as E
import Control.Monad
import Data.Either
import Data.IORef
import Data.Maybe
import Data.Tuple
import Data.Word
import Foreign.C.String
import Foreign.Ptr
import GHC.Base
import GHC.Conc.Sync
import GHC.Event.Unique
import GHC.MVar
import GHC.Num
import GHC.Real
import GHC.Show
import GHC.Windows
import System.IO.Unsafe     (unsafeInterleaveIO, unsafePerformIO)

import qualified GHC.Event.IntMap as IM

------------------------------------------------------------------------
-- Manager

data Manager = Manager
    { mgrIOCP         :: !(FFI.IOCP ManagerCallback)
    , mgrClock        :: !Clock
    , mgrUniqueSource :: !UniqueSource
    , mgrWorkers      :: WorkerList
    , mgrWorkerMap    :: !(IORef WorkerMap)
    }

type ManagerCallback = ErrCode -> DWORD -> Mgr ()

new :: IO Manager
new = do
    mgrIOCP         <- FFI.newIOCP
    mgrClock        <- getClock
    mgrUniqueSource <- newSource
    mgrWorkers      <- newWorkerList
    mgrWorkerMap    <- newIORef IM.empty
    let mgr = Manager{..}
    _tid <- forkOSUnmasked $ loop mgr
    return mgr

getSystemManager :: IO (Maybe Manager)
getSystemManager = readIORef managerRef

managerRef :: IORef (Maybe Manager)
managerRef = unsafePerformIO $
    if rtsSupportsBoundThreads
        then new >>= newIORef . Just
        else newIORef Nothing
{-# NOINLINE managerRef #-}

newOverlapped :: Word64 -> ManagerCallback -> IO Overlapped
newOverlapped = FFI.newOverlapped

-- | Queue an action to be performed by the I/O manager thread.
postIO :: Manager -> IO () -> IO ()
postIO mgr = postMgr mgr . liftIO

-- | Variant of 'postIO' that allows the callback to modify the
-- timeout queue.
postMgr :: Manager -> Mgr () -> IO ()
postMgr mgr cb = newOverlapped 0 (\_errCode _numBytes -> cb)
             >>= FFI.postCompletion (mgrIOCP mgr) 0

------------------------------------------------------------------------
-- Overlapped I/O

-- | Callback that starts the overlapped I/O operation.
-- It must return successfully if and only if an I/O completion has been
-- queued.  Otherwise, it must throw an exception, which 'withOverlapped'
-- will rethrow.
type StartCallback = Overlapped -> IO ()

-- | Called when the completion is delivered.
type CompletionCallback a = ErrCode   -- ^ 0 indicates success
                         -> DWORD     -- ^ Number of bytes transferred
                         -> IO a

-- | Associate a 'HANDLE' with the I/O manager's completion port.  This must be
-- done before using the handle with 'withOverlapped'.
associateHandle :: Manager -> HANDLE -> IO ()
associateHandle Manager{..} h =
    FFI.associateHandleWithIOCP mgrIOCP h

-- | Start an overlapped I/O operation, and wait for its completion.  If
-- 'withOverlapped' is interrupted by an asynchronous exception, the operation
-- will be canceled using @CancelIo@.
--
-- 'withOverlapped' waits for a completion to arrive before returning or
-- throwing an exception.  This means you can use functions like
-- 'Foreign.Marshal.Alloc.alloca' to allocate buffers for the operation.
withOverlapped :: Manager
               -> HANDLE
               -> Word64 -- ^ Value to use for the @OVERLAPPED@
                         --   structure's Offset/OffsetHigh members.
               -> StartCallback
               -> CompletionCallback a
               -> IO a
withOverlapped mgr h offset startCB completionCB = do
    signal <- newEmptyMVar
    let signalReturn a = void $ tryPutMVar signal $ return a
        signalThrow ex = void $ tryPutMVar signal $ throwIO (ex :: SomeException)

    mask_ $ withWorker mgr h $ \enqueue -> do
        enqueue $ do
            let completionCB' e b = liftIO $
                    (completionCB e b >>= signalReturn) `E.catch` signalThrow

            e <- try $ newOverlapped offset completionCB'
            case e of
                Left ex -> signalThrow ex
                Right ol ->
                    startCB ol `E.catch` \ex -> do
                        FFI.discardOverlapped ol
                        signalThrow ex

        let cancel = uninterruptibleMask_ $ do
                cancelDone <- newEmptyMVar
                enqueue $ do
                    FFI.cancelIo h `E.catch` \ex -> do
                        traceIO $ "CancelIo failed: " ++ show (ex :: SomeException)
                        signalThrow ex
                    putMVar cancelDone ()
                _ <- takeMVar signal
                takeMVar cancelDone

        join (takeMVar signal `onException` cancel)

-- Use traceIO because it does not involve the IO manager.  We don't want our
-- error messages to interfere with the IO manager's operation, or worse,
-- produce an infinite loop.
--
-- TODO: Define traceIO in a module that doesn't import Prelude,
-- to avoid duplication.
traceIO :: String -> IO ()
traceIO msg =
    withCString "%s\n" $ \cfmt ->
    withCString msg  $ \cmsg ->
    debugBelch2 cfmt cmsg

foreign import ccall unsafe "HsBase.h debugBelch2"
    debugBelch2 :: CString -> CString -> IO ()

------------------------------------------------------------------------
-- Timeouts

type TimeoutQueue = Q.PSQ TimeoutCallback

-- |
-- Warning: since the 'TimeoutCallback' is called from the I/O manager, it must
-- not throw an exception or block for a long period of time.  In particular,
-- be wary of 'Control.Exception.throwTo' and 'Control.Concurrent.killThread':
-- if the target thread is making a foreign call, these functions will block
-- until the call completes.
type TimeoutCallback = IO ()

newtype TimeoutKey = TK Unique
    deriving (Eq, Ord)

-- | Register an action to be performed in the given number of seconds.  The
-- returned 'TimeoutKey' can be used to later unregister or update the timeout.
-- The timeout is automatically unregistered when it fires.
--
-- The 'TimeoutCallback' will not be called more than once.
registerTimeout :: Manager -> Seconds -> TimeoutCallback -> IO TimeoutKey
registerTimeout mgr@Manager{..} relTime cb = do
    key <- newUnique mgrUniqueSource
    now <- getTime mgrClock
    let !expTime = now + relTime
    postMgr mgr $ modifyTQ $ Q.insert key expTime cb
    return $ TK key

-- | Update an active timeout to fire in the given number of seconds (from the
-- time 'updateTimeout' is called), instead of when it was going to fire.
-- This has no effect if the timeout has already fired.
updateTimeout :: Manager -> TimeoutKey -> Seconds -> IO ()
updateTimeout mgr (TK key) relTime = do
    now <- getTime (mgrClock mgr)
    let !expTime = now + relTime
    postMgr mgr $ modifyTQ $ Q.adjust (const expTime) key

-- | Unregister an active timeout.  This is a harmless no-op if the timeout is
-- already unregistered or has already fired.
--
-- Warning: the timeout callback may fire even after
-- 'unregisterTimeout' completes.
unregisterTimeout :: Manager -> TimeoutKey -> IO ()
unregisterTimeout mgr (TK key) =
    postMgr mgr $ modifyTQ $ Q.delete key

------------------------------------------------------------------------
-- The Mgr state monad

newtype Mgr a = Mgr { runMgr :: TimeoutQueue -> IO (a, TimeoutQueue) }

instance Monad Mgr where
    return a = Mgr $ \s -> return (a, s)
    m >>= k = Mgr $ \s -> do
        (a, s') <- runMgr m s
        runMgr (k a) s'

liftIO :: IO a -> Mgr a
liftIO io = Mgr $ \s -> do
    a <- io
    return (a, s)

getsTQ :: (TimeoutQueue -> a) -> Mgr a
getsTQ f = Mgr $ \s -> return (f s, s)

modifyTQ :: (TimeoutQueue -> TimeoutQueue) -> Mgr ()
modifyTQ f = Mgr $ \s -> do
    let !s' = f s
    return ((), s')

stateTQ :: (TimeoutQueue -> (a, TimeoutQueue)) -> Mgr a
stateTQ f = Mgr $ \s -> do
    let (a, !s') = f s
    return (a, s')

------------------------------------------------------------------------
-- I/O manager loop

-- | Call all expired timeouts, and return how much time until the next expiration.
runExpiredTimeouts :: Manager -> Mgr (Maybe Seconds)
runExpiredTimeouts Manager{..} = do
    -- Avoid calling getTime when there are no pending expirations.
    empty <- getsTQ Q.null
    if empty then
        return Nothing
    else do
        now <- liftIO $ getTime mgrClock

        -- Remove timeouts with expiration <= now, and execute their callbacks.
        expired <- stateTQ $ Q.atMost now
        mapM_ (liftIO . Q.value) expired

        -- See how soon the next timeout expires.
        next <- getsTQ $ fmap Q.prio . Q.findMin
        case next of
            Nothing ->
                return Nothing
            Just t -> do
                -- This value will always be positive since the call
                -- to 'atMost' above removed any timeouts <= 'now'
                let !t' = t - now
                return $ Just t'

-- | Return the delay argument to pass to GetQueuedCompletionStatus.
fromTimeout :: Maybe Seconds -> Word32
fromTimeout Nothing                 = 120000
fromTimeout (Just sec) | sec > 120  = 120000
                       | sec > 0    = ceiling (sec * 1000)
                       | otherwise  = 0

step :: Manager -> Mgr ()
step mgr@Manager{..} = do
    delay <- runExpiredTimeouts mgr
    m <- liftIO $ FFI.getNextCompletion mgrIOCP (fromTimeout delay)
    case m of
        Nothing                      -> return ()
        Just (cb, numBytes, errCode) -> cb errCode numBytes

loop :: Manager -> IO loop
loop mgr = go Q.empty
  where go s = runMgr (step mgr) s >>= go . snd

------------------------------------------------------------------------
-- Worker allocation

type WorkerMap = IM.IntMap Pool

-- | Used to allocate worker threads for I/O requests.  The rule is: a handle
-- may not use the same worker for two simultaneous operations (however,
-- multiple handles may share the same worker).  This is because CancelIo
-- cancels all pending I/O for a given handle in the current thread.
-- CancelIoEx would let us specify an individual operation to cancel,
-- but it was introduced in Windows Vista.
--
-- Whenever we can, we queue jobs to the completion handler using
-- 'postIO'.  This is about 30% faster than using a separate
-- worker thread, as it avoids a context switch.
data Pool = Pool
    { pCompletionPort :: !Bool
    , pWorkers        :: WorkerList
    , pRefCount       :: !Int
        -- ^ Number of in-progress 'withWorker' calls.  When this drops to
        --   zero, we can remove this entry from the 'WorkerMap'.
    }

data WorkerList = WL !Worker WorkerList

-- | Nifty trick to allow each 'Pool' to allocate workers per concurrent
-- operation, while allowing 'Pool's to share workers.
newWorkerList :: IO WorkerList
newWorkerList = unsafeInterleaveIO $ do
    w  <- Worker.new
    ws <- newWorkerList
    return (WL w ws)
{-# NOINLINE newWorkerList #-}

type Enqueue = IO () -> IO ()

type ReleaseF = WorkerMap -> (WorkerMap, ())

withWorker :: Manager -> HANDLE -> (Enqueue -> IO a) -> IO a
withWorker mgr@Manager{..} h cb =
    mask $ \restore -> do
        (enqueue, releaseF) <- atomicModifyIORef mgrWorkerMap grabF
        let release = atomicModifyIORef mgrWorkerMap releaseF
                  >>= evaluate
        a <- restore (cb enqueue) `onException` release
        release
        return a
  where
    key = (fromIntegral . ptrToIntPtr) h :: Int

    grabF :: WorkerMap -> (WorkerMap, (Enqueue, ReleaseF))
    grabF m =
        case IM.lookup key m of
            Nothing ->
                let !pool = Pool False mgrWorkers 1
                    !m'   = snd $ IM.insert key pool m
                 in (m', (postIO mgr, releaseCP))
            Just Pool{..}
              | pCompletionPort ->
                let !pool = Pool False pWorkers (pRefCount + 1)
                    !m'   = snd $ IM.insert key pool m
                 in (m', (postIO mgr, releaseCP))
              | WL w ws <- pWorkers ->
                let !pool = Pool pCompletionPort ws (pRefCount + 1)
                    !m'   = snd $ IM.insert key pool m
                 in (m', (Worker.enqueue w, releaseWorker w))

    releaseCP :: ReleaseF
    releaseCP = releaseWith $ \Pool{..} ->
        Pool True pWorkers (pRefCount - 1)

    releaseWorker :: Worker -> ReleaseF
    releaseWorker w = releaseWith $ \Pool{..} ->
        Pool pCompletionPort (WL w pWorkers) (pRefCount - 1)

    releaseWith :: (Pool -> Pool) -> ReleaseF
    releaseWith f m =
        case IM.lookup key m of
            Nothing -> (m, ())  -- should never happen
            Just pool
              | pRefCount pool <= 1 ->
                let !m' = snd $ IM.delete key m
                 in (m', ())
              | otherwise ->
                let !pool' = f pool
                    !m'    = snd $ IM.insert key pool' m
                 in (m', ())
