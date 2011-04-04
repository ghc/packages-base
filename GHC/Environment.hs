{-# LANGUAGE ForeignFunctionInterface #-}

module GHC.Environment (getFullArgs) where

import Prelude
import Foreign
import Foreign.C
import Control.Monad

import GHC.IO.Encoding
import qualified GHC.Foreign as GHC

getFullArgs :: IO [String]
getFullArgs =
  alloca $ \ p_argc ->
  alloca $ \ p_argv -> do
   getFullProgArgv p_argc p_argv
   p    <- fromIntegral `liftM` peek p_argc
   argv <- peek p_argv
   -- FIXME: we should use GetCommandLineW on Windows instead
   peekArray (p - 1) (advancePtr argv 1) >>= mapM (GHC.peekCString fileSystemEncoding)

foreign import ccall unsafe "getFullProgArgv"
    getFullProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

