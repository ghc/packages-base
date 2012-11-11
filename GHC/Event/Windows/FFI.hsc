{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
module GHC.Event.Windows.FFI (
    -- * IOCP
    IOCP(..),
    newIOCP,
    associateHandleWithIOCP,
    getNextCompletion,
    postCompletion,

    -- * Overlapped
    Overlapped(..),
    newOverlapped,
    discardOverlapped,

    -- * Cancel pending I/O
    cancelIo,

    -- * Monotonic time

    -- ** GetTickCount
    getTickCount,
    loadGetTickCount64,

    -- ** QueryPerformanceCounter
    queryPerformanceCounter,
    queryPerformanceFrequency,
) where

#include <windows.h>

##ifdef mingw32_HOST_OS
## if defined(i386_HOST_ARCH)
##  define WINDOWS_CCONV stdcall
## elif defined(x86_64_HOST_ARCH)
##  define WINDOWS_CCONV ccall
## else
##  error Unknown mingw32 arch
## endif
##endif

import Control.Exception hiding (handle)
import Data.Maybe
import Data.Word
import Foreign
import GHC.Base
import GHC.Show
import GHC.Windows

------------------------------------------------------------------------
-- IOCP

-- |
--
-- The type variable @a@ represents additional data each completion carries
-- with it.  After associating a 'HANDLE' with a completion port, you must not
-- initiate IO on it with @OVERLAPPED@ structures other than those created by
-- 'newOverlapped' with the same type.
newtype IOCP a = IOCP HANDLE
    deriving (Eq, Ord, Show)

foreign import WINDOWS_CCONV unsafe "windows.h CreateIoCompletionPort"
    c_CreateIoCompletionPort :: HANDLE -> IOCP a -> ULONG_PTR -> DWORD -> IO (IOCP a)

newIOCP :: IO (IOCP a)
newIOCP =
    failIf (== IOCP nullPtr) "newIOCP" $
        c_CreateIoCompletionPort iNVALID_HANDLE_VALUE (IOCP nullPtr) 0 1

associateHandleWithIOCP :: IOCP a -> HANDLE -> IO ()
associateHandleWithIOCP iocp handle =
    failIf_ (/= iocp) "associateHandleWithIOCP" $
        c_CreateIoCompletionPort handle iocp 0 0

foreign import ccall safe
    c_iocp_get_next_completion
        :: IOCP a -> DWORD
        -> Ptr DWORD -> Ptr DWORD -> Ptr (StablePtr a) -> IO BOOL

getNextCompletion :: IOCP a
                  -> DWORD  -- ^ Timeout in milliseconds (or 'GHC.Windows.iNFINITE')
                  -> IO (Maybe (a, DWORD, ErrCode))
getNextCompletion iocp timeout =
    alloca $ \num_bytes_ptr ->
    alloca $ \err_ptr ->
    alloca $ \userdata_ptr -> do
        ok <- c_iocp_get_next_completion
                  iocp timeout
                  num_bytes_ptr err_ptr userdata_ptr
        err <- peek err_ptr
        if ok then do
            num_bytes <- peek num_bytes_ptr
            a         <- peek userdata_ptr >>= takeStablePtr
            return $ Just (a, num_bytes, err)
        else if err == #{const WAIT_TIMEOUT} then
            return Nothing
        else
            failWith "GetQueuedCompletionStatus" err

foreign import WINDOWS_CCONV unsafe "windows.h PostQueuedCompletionStatus"
    c_PostQueuedCompletionStatus :: IOCP a -> DWORD -> ULONG_PTR -> Overlapped -> IO BOOL

postCompletion :: IOCP a -> DWORD -> Overlapped -> IO ()
postCompletion iocp numBytes ol =
    failIfFalse_ "PostQueuedCompletionStatus" $
    c_PostQueuedCompletionStatus iocp numBytes 0 ol

------------------------------------------------------------------------
-- Overlapped

-- | Identifies an I/O operation.  Used as the @LPOVERLAPPED@ parameter
-- for overlapped I/O functions (e.g. @ReadFile@, @WSASend@).
newtype Overlapped = Overlapped (Ptr ())
    deriving (Eq, Ord, Show)

foreign import ccall unsafe
    c_iocp_new_overlapped :: Word64 -> StablePtr a -> IO Overlapped

-- | Allocate a new
-- <http://msdn.microsoft.com/en-us/library/windows/desktop/ms684342%28v=vs.85%29.aspx OVERLAPPED>
-- structure.  The resulting pointer may be passed to a system call that takes
-- an @LPOVERLAPPED@, provided the @HANDLE@ was associated with an 'IOCP' with
-- the same type @a@.
newOverlapped :: Word64 -- ^ Offset/OffsetHigh
              -> a      -- ^ Application context (stored alongside the @OVERLAPPED@ structure)
              -> IO Overlapped
newOverlapped offset ctx =
    bracketOnError (newStablePtr ctx) freeStablePtr $ \ptr ->
        failIf (== Overlapped nullPtr) "newOverlapped" $
            c_iocp_new_overlapped offset ptr

foreign import ccall unsafe
    c_iocp_finish_overlapped :: Overlapped -> IO (StablePtr a)

-- | Discard an 'Overlapped' object.  This should be called if and only if
-- no pending I/O was produced after all.
discardOverlapped :: Overlapped -> IO ()
discardOverlapped o = c_iocp_finish_overlapped o >>= freeStablePtr

------------------------------------------------------------------------
-- Cancel pending I/O

-- | CancelIo shouldn't block, but cancellation happens infrequently,
-- so we might as well be on the safe side.
foreign import WINDOWS_CCONV safe "windows.h CancelIo"
    c_CancelIo :: HANDLE -> IO BOOL

-- | Cancel all pending overlapped I/O for the given file that was initiated by
-- the current OS thread.
cancelIo :: HANDLE -> IO ()
cancelIo = failIfFalse_ "CancelIo" . c_CancelIo

------------------------------------------------------------------------
-- Monotonic time

foreign import WINDOWS_CCONV "windows.h GetTickCount"
    c_GetTickCount :: IO #{type DWORD}

-- | Call the @GetTickCount@ function, which returns a monotonic time in
-- milliseconds.
--
-- Problems:
--
--  * Low resolution (10 to 16 milliseconds).
--
--  * Wraps around when the system runs continuously for 49.7 days.
--
--  * Not available for Windows Store apps.
--
-- <http://msdn.microsoft.com/en-us/library/windows/desktop/ms724408%28v=vs.85%29.aspx>
getTickCount :: IO Word32
getTickCount = c_GetTickCount

type C_GetTickCount64 = IO #{type ULONGLONG}

-- Defined in cbits/dll.c
foreign import ccall
    iocp_load_GetTickCount64 :: IO (FunPtr C_GetTickCount64)

foreign import WINDOWS_CCONV "dynamic"
    mkGetTickCount64 :: FunPtr C_GetTickCount64 -> C_GetTickCount64

-- | Load the @GetTickCount64@ function, or return 'Nothing' if it is
-- not available.
--
-- Problems:
--
--  * Low resolution (10 to 16 milliseconds).
--
--  * Introduced in Windows Vista, so not available under Windows XP.
--
-- <http://msdn.microsoft.com/en-us/library/windows/desktop/ms724411%28v=vs.85%29.aspx>
loadGetTickCount64 :: IO (Maybe (IO Word64))
loadGetTickCount64 = do
    fun <- iocp_load_GetTickCount64
    if fun == nullFunPtr then
        return Nothing
    else
        return $ Just $ mkGetTickCount64 fun

type QPFunc = Ptr Int64 -> IO BOOL

foreign import WINDOWS_CCONV "Windows.h QueryPerformanceCounter"
    c_QueryPerformanceCounter :: QPFunc

foreign import WINDOWS_CCONV "Windows.h QueryPerformanceFrequency"
    c_QueryPerformanceFrequency :: QPFunc

callQP :: QPFunc -> IO (Maybe Int64)
callQP qpfunc =
    allocaBytes #{size LARGE_INTEGER} $ \ptr -> do
        ok <- qpfunc ptr
        if ok then do
            n <- #{peek LARGE_INTEGER, QuadPart} ptr
            return (Just n)
        else
            return Nothing

-- | Call the @QueryPerformanceCounter@ function.
--
-- Problems:
--
--  * Might not be available on some hardware.  Use 'queryPerformanceFrequency'
--    to test for availability before calling this function.
--
--  * On a multiprocessor computer, may produce different results on
--    different processors due to hardware bugs.
--
-- To get a monotonic time in seconds, divide the result of
-- 'queryPerformanceCounter' by that of 'queryPerformanceFrequency'.
--
-- <http://msdn.microsoft.com/en-us/library/windows/desktop/ms644904%28v=vs.85%29.aspx>
queryPerformanceCounter :: IO Int64
queryPerformanceCounter =
    callQP c_QueryPerformanceCounter
    >>= maybe (throwGetLastError "QueryPerformanceCounter") return

-- | Call the @QueryPerformanceFrequency@ function.  Return 'Nothing' if the
-- hardware does not provide a high-resolution performance counter.
--
-- <http://msdn.microsoft.com/en-us/library/windows/desktop/ms644905%28v=vs.85%29.aspx>
queryPerformanceFrequency :: IO (Maybe Int64)
queryPerformanceFrequency = do
    m <- callQP c_QueryPerformanceFrequency
    case m of
        Nothing   -> return Nothing
        Just 0    -> return Nothing -- Shouldn't happen; just a safeguard to
                                    -- avoid a zero denominator.
        Just freq -> return (Just freq)

------------------------------------------------------------------------
-- Miscellaneous

type ULONG_PTR = #type ULONG_PTR

takeStablePtr :: StablePtr a -> IO a
takeStablePtr ptr = do
    a <- deRefStablePtr ptr
    freeStablePtr ptr
    return a
