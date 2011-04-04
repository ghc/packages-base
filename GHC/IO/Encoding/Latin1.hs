{-# LANGUAGE NoImplicitPrelude
           , BangPatterns
           , PatternGuards
           , NondecreasingIndentation
  #-}
{-# OPTIONS_GHC  -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Encoding.Latin1
-- Copyright   :  (c) The University of Glasgow, 2009
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- UTF-32 Codecs for the IO library
--
-- Portions Copyright   : (c) Tom Harper 2008-2009,
--                        (c) Bryan O'Sullivan 2009,
--                        (c) Duncan Coutts 2009
--
-----------------------------------------------------------------------------

module GHC.IO.Encoding.Latin1 (
  latin1, latin1FailingWith,
  latin1_checked, latin1_checkedFailingWith,
  latin1_decode,
  latin1_encode,
  latin1_checked_encode,
  ) where

import GHC.Base
import GHC.Real
import GHC.Num
-- import GHC.IO
import GHC.IO.Exception
import GHC.IO.Buffer
import GHC.IO.Encoding.Failure
import GHC.IO.Encoding.Types
import Data.Maybe

-- -----------------------------------------------------------------------------
-- Latin1

latin1 :: TextEncoding
latin1 = latin1FailingWith ErrorOnCodingFailure

latin1FailingWith :: CodingFailureMode -> TextEncoding
latin1FailingWith cfm
  = TextEncoding { textEncodingName = "ISO8859-1" ++ codingFailureModeSuffix cfm,
                   mkTextDecoder = latin1_DF,
                   mkTextEncoder = latin1_EF }

latin1_DF :: IO (TextDecoder ())
latin1_DF =
  return (BufferCodec {
             encode   = latin1_decode,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })

latin1_EF :: IO (TextEncoder ())
latin1_EF =
  return (BufferCodec {
             encode   = latin1_encode,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })

latin1_checked :: TextEncoding
latin1_checked = latin1_checkedFailingWith ErrorOnCodingFailure

latin1_checkedFailingWith :: CodingFailureMode -> TextEncoding
latin1_checkedFailingWith cfm
  = TextEncoding { textEncodingName = "ISO8859-1(checked)" ++ codingFailureModeSuffix cfm,
                   mkTextDecoder = latin1_DF,
                   mkTextEncoder = latin1_checked_EF cfm }

latin1_checked_EF :: CodingFailureMode -> IO (TextEncoder ())
latin1_checked_EF cfm =
  return (BufferCodec {
             encode   = latin1_checked_encodeFailingWith cfm,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })


latin1_decode :: DecodeBuffer
latin1_decode 
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let 
       loop !ir !ow
         | ow >= os || ir >= iw =  done ir ow
         | otherwise = do
              c0 <- readWord8Buf iraw ir
              ow' <- writeCharBuf oraw ow (unsafeChr (fromIntegral c0))
              loop (ir+1) ow'

       -- lambda-lifted, to avoid thunks being built in the inner-loop:
       done !ir !ow = return (if ir == iw then input{ bufL=0, bufR=0 }
                                          else input{ bufL=ir },
                         output{ bufR=ow })
    in
    loop ir0 ow0

latin1_encode :: EncodeBuffer
latin1_encode
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let
      done !ir !ow = return (if ir == iw then input{ bufL=0, bufR=0 }
                                         else input{ bufL=ir },
                             output{ bufR=ow })
      loop !ir !ow
        | ow >= os || ir >= iw =  done ir ow
        | otherwise = do
           (c,ir') <- readCharBuf iraw ir
           writeWord8Buf oraw ow (fromIntegral (ord c))
           loop ir' (ow+1)
    in
    loop ir0 ow0

latin1_checked_encode :: EncodeBuffer
latin1_checked_encode = latin1_checked_encodeFailingWith ErrorOnCodingFailure

latin1_checked_encodeFailingWith :: CodingFailureMode -> EncodeBuffer
latin1_checked_encodeFailingWith cfm
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let
      done !ir !ow = return (if ir == iw then input{ bufL=0, bufR=0 }
                                         else input{ bufL=ir },
                             output{ bufR=ow })
      loop !ir !ow
        | ow >= os || ir >= iw =  done ir ow
        | otherwise = do
           (c,ir') <- readCharBuf iraw ir
           if ord c > 0xff then invalid c ir' else do
           writeWord8Buf oraw ow (fromIntegral (ord c))
           loop ir' (ow+1)
        where
           invalid c ir' = case cfm of
              IgnoreCodingFailure -> loop ir' ow
              SurrogateEscapeFailure | Just b <- encodeSurrogateCharacter c -> do
                writeWord8Buf oraw ow b
                loop ir' (ow+1)
              TransliterateCodingFailure -> do
                writeWord8Buf oraw ow unrepresentableByte
                loop ir' (ow+1)
              _ | ir > ir0  -> done ir ow
                | otherwise -> ioe_encodingError
    in
    loop ir0 ow0

ioe_encodingError :: IO a
ioe_encodingError = ioException
     (IOError Nothing InvalidArgument "latin1_checked_encode"
          "character is out of range for this encoding" Nothing Nothing)
