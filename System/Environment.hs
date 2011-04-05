{-# LANGUAGE CPP, ForeignFunctionInterface #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Environment
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Miscellaneous information about the system environment.
--
-----------------------------------------------------------------------------

module System.Environment
    (
      getArgs,       -- :: IO [String]
      getProgName,   -- :: IO String
      getEnv,        -- :: String -> IO String
#ifndef __NHC__
      withArgs,
      withProgName,
#endif
#ifdef __GLASGOW_HASKELL__
      getEnvironment,
#endif
  ) where

import Prelude

#ifdef __GLASGOW_HASKELL__
import Data.List
import Foreign
import Foreign.C
import Control.Exception.Base   ( bracket )
import Control.Monad
-- import GHC.IO
import GHC.IO.Exception
import GHC.IO.Encoding (fileSystemEncoding)
import qualified GHC.Foreign as GHC
#ifdef mingw32_HOST_OS
import GHC.Windows
#endif
#endif

#ifdef __HUGS__
import Hugs.System
#endif

#ifdef __NHC__
import System
  ( getArgs
  , getProgName
  , getEnv
  )
#endif

-- ---------------------------------------------------------------------------
-- getArgs, getProgName, getEnv

-- | Computation 'getArgs' returns a list of the program's command
-- line arguments (not including the program name).

#ifdef __GLASGOW_HASKELL__
getArgs :: IO [String]
getArgs =
  alloca $ \ p_argc ->
  alloca $ \ p_argv -> do
   getProgArgv p_argc p_argv
   p    <- fromIntegral `liftM` peek p_argc
   argv <- peek p_argv
   -- FIXME: we should use GetCommandLineW on Windows instead
   peekArray (p - 1) (advancePtr argv 1) >>= mapM (GHC.peekCString fileSystemEncoding)


foreign import ccall unsafe "getProgArgv"
  getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

{-|
Computation 'getProgName' returns the name of the program as it was
invoked.

However, this is hard-to-impossible to implement on some non-Unix
OSes, so instead, for maximum portability, we just return the leafname
of the program as invoked. Even then there are some differences
between platforms: on Windows, for example, a program invoked as foo
is probably really @FOO.EXE@, and that is what 'getProgName' will return.
-}
getProgName :: IO String
getProgName =
  alloca $ \ p_argc ->
  alloca $ \ p_argv -> do
     getProgArgv p_argc p_argv
     argv <- peek p_argv
     unpackProgName argv

unpackProgName  :: Ptr (Ptr CChar) -> IO String   -- argv[0]
unpackProgName argv = do
  -- FIXME: we should use GetCommandLineW on Windows instead
  s <- peekElemOff argv 0 >>= GHC.peekCString fileSystemEncoding
  return (basename s)
  where
   basename :: String -> String
   basename f = go f f
    where
      go acc [] = acc
      go acc (x:xs)
        | isPathSeparator x = go xs xs
        | otherwise         = go acc xs

   isPathSeparator :: Char -> Bool
   isPathSeparator '/'  = True
#ifdef mingw32_HOST_OS
   isPathSeparator '\\' = True
#endif
   isPathSeparator _    = False


-- | Computation 'getEnv' @var@ returns the value
-- of the environment variable @var@.  
--
-- This computation may fail with:
--
--  * 'System.IO.Error.isDoesNotExistError' if the environment variable
--    does not exist.

getEnv :: String -> IO String
#ifdef mingw32_HOST_OS
getEnv name = withCWString name $ \s -> do
    size <- c_GetEnvironmentVariable s nullPtr 0
    try_size s size
  where
    try_size s size = allocaArray (fromIntegral size) $ \p_value -> do
      res <- c_GetEnvironmentVariable s p_value size
      case res of
        0 -> throwGetLastError "getEnv"
        _ | res > size -> try_size s res -- Rare: size increased between calls to GetEnvironmentVariable
          | otherwise  -> peekCWString p_value

foreign import stdcall unsafe "windows.h GetEnvironmentVariableW"
  c_GetEnvironmentVariable :: LPTSTR -> LPTSTR -> DWORD -> IO DWORD
#else
getEnv name =
    withCString name $ \s -> do
      litstring <- c_getenv s
      if litstring /= nullPtr
        then GHC.peekCString fileSystemEncoding litstring
        else ioException (IOError Nothing NoSuchThing "getEnv"
                          "no environment variable" Nothing (Just name))
#endif

foreign import ccall unsafe "getenv"
   c_getenv :: CString -> IO (Ptr CChar)

{-|
'withArgs' @args act@ - while executing action @act@, have 'getArgs'
return @args@.
-}
withArgs :: [String] -> IO a -> IO a
withArgs xs act = do
   p <- System.Environment.getProgName
   withArgv (p:xs) act

{-|
'withProgName' @name act@ - while executing action @act@,
have 'getProgName' return @name@.
-}
withProgName :: String -> IO a -> IO a
withProgName nm act = do
   xs <- System.Environment.getArgs
   withArgv (nm:xs) act

-- Worker routine which marshals and replaces an argv vector for
-- the duration of an action.

withArgv :: [String] -> IO a -> IO a
withArgv new_args act = do
  pName <- System.Environment.getProgName
  existing_args <- System.Environment.getArgs
  bracket (setArgs new_args)
          (\argv -> do _ <- setArgs (pName:existing_args)
                       freeArgv argv)
          (const act)

freeArgv :: Ptr CString -> IO ()
freeArgv argv = do
  size <- lengthArray0 nullPtr argv
  sequence_ [peek (argv `advancePtr` i) >>= free | i <- [size, size-1 .. 0]]
  free argv

setArgs :: [String] -> IO (Ptr CString)
setArgs argv = do
  -- FIXME: do something else on Windows instead...
  vs <- mapM (GHC.newCString fileSystemEncoding) argv >>= newArray0 nullPtr
  setArgsPrim (genericLength argv) vs
  return vs

foreign import ccall unsafe "setProgArgv" 
  setArgsPrim  :: CInt -> Ptr CString -> IO ()

-- |'getEnvironment' retrieves the entire environment as a
-- list of @(key,value)@ pairs.
--
-- If an environment entry does not contain an @\'=\'@ character,
-- the @key@ is the whole entry and the @value@ is the empty string.
getEnvironment :: IO [(String, String)]

#ifdef mingw32_HOST_OS
getEnvironment = bracket c_GetEnvironmentStrings c_FreeEnvironmentStrings $ \pBlock ->
    if pBlock == nullPtr then return []
     else go pBlock
  where
    go pBlock = do
        -- The block is terminated by a null byte where there
        -- should be an environment variable of the form X=Y
        c <- peek pBlock
        if c == 0 then return []
         else do
          -- Seek the next pair (or terminating null):
          pBlock' <- seekNull pBlock False
          -- We now know the length in bytes, but ignore it when
          -- getting the actual String:
          str <- peekCWString pBlock
          fmap (divvy str :) $ go pBlock'
    
    -- Returns pointer to the byte *after* the next null
    seekNull pBlock done = do
        let pBlock' = pBlock `plusPtr` sizeOf (undefined :: CWchar)
        if done then return pBlock'
         else do
           c <- peek pBlock'
           seekNull pBlock' (c == (0 :: Word8 ))

foreign import stdcall unsafe "windows.h GetEnvironmentStringsW"
  c_GetEnvironmentStrings :: IO (Ptr CWchar)

foreign import stdcall unsafe "windows.h FreeEnvironmentStringsW"
  c_FreeEnvironmentStrings :: Ptr CWchar -> IO Bool
#else
getEnvironment = do
   pBlock <- getEnvBlock
   if pBlock == nullPtr then return []
    else do
      stuff <- peekArray0 nullPtr pBlock >>= mapM (GHC.peekCString fileSystemEncoding)
      return (map divvy stuff)
#endif

divvy :: String -> (String, String)
divvy str =
  case break (=='=') str of
    (xs,[])        -> (xs,[]) -- don't barf (like Posix.getEnvironment)
    (name,_:value) -> (name,value)

foreign import ccall unsafe "__hscore_environ" 
  getEnvBlock :: IO (Ptr CString)
#endif  /* __GLASGOW_HASKELL__ */
