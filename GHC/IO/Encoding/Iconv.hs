{-# LANGUAGE CPP
           , NoImplicitPrelude
           , ForeignFunctionInterface
           , NondecreasingIndentation
           , PatternGuards
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Encoding.Iconv
-- Copyright   :  (c) The University of Glasgow, 2008-2009
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- This module provides text encoding/decoding using iconv
--
-----------------------------------------------------------------------------

-- #hide
module GHC.IO.Encoding.Iconv (
#if !defined(mingw32_HOST_OS)
   mkTextEncoding,
   latin1,
   utf8, 
   utf16, utf16le, utf16be,
   utf32, utf32le, utf32be,
   localeEncoding, localeEncoding_ignore
#endif
 ) where

#include "MachDeps.h"
#include "HsBaseConfig.h"

#if !defined(mingw32_HOST_OS)

import Foreign hiding (unsafePerformIO)
import Foreign.C
import Data.Maybe
import GHC.Base
import GHC.IO.Buffer
import GHC.IO.Encoding.Failure
import GHC.IO.Encoding.Types
import GHC.List (span)
import GHC.Num
import GHC.Show
import GHC.Real
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Internals

c_DEBUG_DUMP :: Bool
c_DEBUG_DUMP = False

iconv_trace :: String -> IO ()
iconv_trace s
 | c_DEBUG_DUMP = puts s
 | otherwise    = return ()

puts :: String -> IO ()
puts s = do _ <- withCAStringLen (s ++ "\n") $ \(p, len) ->
                      -- In reality should be withCString, but assume ASCII to avoid loop
                     c_write 1 (castPtr p) (fromIntegral len)
            return ()

-- -----------------------------------------------------------------------------
-- iconv encoders/decoders

{-# NOINLINE latin1 #-}
latin1 :: TextEncoding
latin1 = unsafePerformIO (mkTextEncoding "Latin1")

{-# NOINLINE utf8 #-}
utf8 :: TextEncoding
utf8 = unsafePerformIO (mkTextEncoding "UTF8")

{-# NOINLINE utf16 #-}
utf16 :: TextEncoding
utf16 = unsafePerformIO (mkTextEncoding "UTF16")

{-# NOINLINE utf16le #-}
utf16le :: TextEncoding
utf16le = unsafePerformIO (mkTextEncoding "UTF16LE")

{-# NOINLINE utf16be #-}
utf16be :: TextEncoding
utf16be = unsafePerformIO (mkTextEncoding "UTF16BE")

{-# NOINLINE utf32 #-}
utf32 :: TextEncoding
utf32 = unsafePerformIO (mkTextEncoding "UTF32")

{-# NOINLINE utf32le #-}
utf32le :: TextEncoding
utf32le = unsafePerformIO (mkTextEncoding "UTF32LE")

{-# NOINLINE utf32be #-}
utf32be :: TextEncoding
utf32be = unsafePerformIO (mkTextEncoding "UTF32BE")

{-# NOINLINE localeEncodingName #-}
localeEncodingName :: String
localeEncodingName = unsafePerformIO $ do
   -- Use locale_charset() or nl_langinfo(CODESET) to get the encoding
   -- if we have either of them.
   cstr <- c_localeEncoding
   peekCAString cstr -- Assume charset names are ASCII

{-# NOINLINE localeEncoding #-}
localeEncoding :: TextEncoding
localeEncoding = unsafePerformIO $ mkTextEncoding localeEncodingName

{-# NOINLINE localeEncoding_ignore #-}
localeEncoding_ignore :: TextEncoding
localeEncoding_ignore = unsafePerformIO $ mkTextEncodingFailingWith IgnoreCodingFailure localeEncodingName

-- We hope iconv_t is a storable type.  It should be, since it has at least the
-- value -1, which is a possible return value from iconv_open.
type IConv = CLong -- ToDo: (#type iconv_t)

foreign import ccall unsafe "hs_iconv_open"
    hs_iconv_open :: CString -> CString -> IO IConv

foreign import ccall unsafe "hs_iconv_close"
    hs_iconv_close :: IConv -> IO CInt

foreign import ccall unsafe "hs_iconv"
    hs_iconv :: IConv -> Ptr CString -> Ptr CSize -> Ptr CString -> Ptr CSize
	  -> IO CSize

foreign import ccall unsafe "localeEncoding"
    c_localeEncoding :: IO CString

haskellChar :: String
#ifdef WORDS_BIGENDIAN
haskellChar | charSize == 2 = "UTF-16BE"
            | otherwise     = "UTF-32BE"
#else
haskellChar | charSize == 2 = "UTF-16LE"
            | otherwise     = "UTF-32LE"
#endif

char_shift :: Int
char_shift | charSize == 2 = 1
           | otherwise     = 2

mkTextEncoding :: String -> IO TextEncoding
mkTextEncoding = mkTextEncodingFailingWith ErrorOnCodingFailure

mkTextEncodingFailingWith :: CodingFailureMode -> String -> IO TextEncoding
mkTextEncodingFailingWith cfm charset = do
  return (TextEncoding { 
                textEncodingName = charset,
		mkTextDecoder = newIConv raw_charset (haskellChar ++ suffix) (iconvDecode cfm),
		mkTextEncoder = newIConv haskellChar charset (iconvEncode cfm)})
  where
    -- An annoying feature of GNU iconv is that the //PREFIXES only take
    -- effect when they appear on the tocode parameter to iconv_open:
    (raw_charset, suffix) = span (/= '/') charset

newIConv :: String -> String
   -> (IConv -> Buffer a -> Buffer b -> IO (Buffer a, Buffer b))
   -> IO (BufferCodec a b ())
newIConv from to fn =
  -- Assume charset names are ASCII
  withCAString from $ \ from_str ->
  withCAString to   $ \ to_str -> do
    iconvt <- throwErrnoIfMinus1 "mkTextEncoding" $ hs_iconv_open to_str from_str
    let iclose = throwErrnoIfMinus1_ "Iconv.close" $ hs_iconv_close iconvt
    return BufferCodec{
                encode = fn iconvt,
                close  = iclose,
                -- iconv doesn't supply a way to save/restore the state
                getState = return (),
                setState = const $ return ()
                }

iconvDecode :: CodingFailureMode
            -> IConv -> Buffer Word8 -> Buffer CharBufElem
            -> IO (Buffer Word8, Buffer CharBufElem)
iconvDecode cfm iconv_t ibuf obuf = iconvRecode cfm transcribe surrogatify iconv_t ibuf 0 obuf char_shift
  where transcribe output@Buffer{ bufRaw=oraw, bufR=ow }
          | isFullBuffer output = Nothing
          | otherwise           = Just (do { ow' <- writeCharBuf oraw ow unrepresentableChar; return output { bufR = ow' } })
        surrogatify input@Buffer{ bufRaw=iraw, bufL=ir } output@Buffer{ bufRaw=oraw, bufR=ow } = do
            b <- readWord8Buf iraw ir
            ow' <- writeCharBuf oraw ow (decodeToSurrogateCharacter b)
            return $ Just (input { bufL = ir+1 }, output { bufR = ow' })

iconvEncode :: CodingFailureMode
            -> IConv -> Buffer CharBufElem -> Buffer Word8
            -> IO (Buffer CharBufElem, Buffer Word8)
iconvEncode cfm iconv_t ibuf obuf = iconvRecode cfm transcribe surrogatify iconv_t ibuf char_shift obuf 0
  where transcribe output@Buffer{ bufRaw=oraw, bufR=ow }
          | isFullBuffer output = Nothing
          | otherwise           = Just (do { writeWord8Buf oraw ow unrepresentableByte; return output { bufR = ow+1 } })
        surrogatify input@Buffer{ bufRaw=iraw, bufL=ir } output@Buffer{ bufRaw=oraw, bufR=ow } = do
            (c, ir') <- readCharBuf iraw ir
            case encodeSurrogateCharacter c of
                Nothing -> return Nothing
                Just b -> do
                    writeWord8Buf oraw ow b
                    return $ Just (input { bufL = ir' }, output { bufR = ow+1 })

iconvRecode :: CodingFailureMode
            -> (Buffer b -> Maybe (IO (Buffer b)))                       -- ^ How to transcribe
            -> (Buffer a -> Buffer b -> IO (Maybe (Buffer a, Buffer b))) -- ^ How to encode surrogate
            -> IConv -> Buffer a -> Int -> Buffer b -> Int
            -> IO (Buffer a, Buffer b)
iconvRecode cfm transcribe surrogatify iconv_t input0 iscale output0 oscale = go input0 output0
  where
    go input@Buffer{  bufRaw=iraw, bufL=ir, bufR=iw, bufSize=_  }
       output@Buffer{ bufRaw=oraw, bufL=_,  bufR=ow, bufSize=os }
      = do
        iconv_trace ("haskelChar=" ++ show haskellChar)
        iconv_trace ("iconvRecode before, input=" ++ show (summaryBuffer input))
        iconv_trace ("iconvRecode before, output=" ++ show (summaryBuffer output))
        withRawBuffer iraw $ \ piraw -> do
        withRawBuffer oraw $ \ poraw -> do
        with (piraw `plusPtr` (ir `shiftL` iscale)) $ \ p_inbuf -> do
        with (poraw `plusPtr` (ow `shiftL` oscale)) $ \ p_outbuf -> do
        with (fromIntegral ((iw-ir) `shiftL` iscale)) $ \ p_inleft -> do
        with (fromIntegral ((os-ow) `shiftL` oscale)) $ \ p_outleft -> do
          res <- hs_iconv iconv_t p_inbuf p_inleft p_outbuf p_outleft
          new_inleft  <- peek p_inleft
          new_outleft <- peek p_outleft
          let new_inleft'  = fromIntegral new_inleft `shiftR` iscale
              new_outleft' = fromIntegral new_outleft `shiftR` oscale
              new_input  
                | new_inleft == 0  = input { bufL = 0, bufR = 0 }
                | otherwise        = input { bufL = iw - new_inleft' }
              new_output = output{ bufR = os - new_outleft' }
          iconv_trace ("iconv res=" ++ show res)
          iconv_trace ("iconvRecode after,  input=" ++ show (summaryBuffer new_input))
          iconv_trace ("iconvRecode after,  output=" ++ show (summaryBuffer new_output))
          if (res /= -1)
           then do -- all input translated
            return (new_input, new_output)
           else do
            errno <- getErrno
            case errno of
              e |  e == eINVAL || e == e2BIG
                || e == eILSEQ && new_inleft' /= (iw-ir) -> do
                  iconv_trace ("iconv ignoring error: " ++ show (errnoToIOError "iconv" e Nothing Nothing))
                      -- Output overflow is harmless
                      --
                      -- Similarly, we ignore EILSEQ unless we converted no
                      -- characters.  Sometimes iconv reports EILSEQ for a
                      -- character in the input even when there is no room
                      -- in the output; in this case we might be about to
                      -- change the encoding anyway, so the following bytes
                      -- could very well be in a different encoding.
                      -- This also helps with pinpointing EILSEQ errors: we
                      -- don't report it until the rest of the characters in
                      -- the buffer have been drained.
                  return (new_input, new_output)
            
                 -- Custom code to try to ignore invalid byte sequences. Not as good as just using the //IGNORE
                 -- suffix to iconv, but we can't rely on that behaviour
              e -> do
                  mb_handler <- case cfm of
                      _ | e /= eINVAL, e /= eILSEQ -> return Nothing
                      ErrorOnCodingFailure         -> return Nothing
                      IgnoreCodingFailure          -> return $ Just (go (bufferAdjustL 1 new_input) new_output)
                      SurrogateEscapeFailure
                        | isEmptyBuffer new_input || isFullBuffer new_output
                        -> return $ Just $ return (new_input, new_output) -- Output/input overflow
                        | otherwise -> do
                          mb_inout <- surrogatify new_input new_output
                          case mb_inout of
                              Nothing -> return Nothing
                              Just (new_input', new_output') -> return $ Just $ go new_input' new_output'
                      TransliterateCodingFailure   -> return $ Just $ case transcribe new_output of
                                                                        Nothing  -> return (new_input, new_output)
                                                                        Just act -> act >>= go (bufferAdjustL 1 new_input)
                  case mb_handler of
                      Just handler -> handler
                      Nothing -> do
                          iconv_trace ("iconv returned error: " ++ show (errnoToIOError "iconv" e Nothing Nothing) ++ " "
                                                                ++ show (new_inleft', (iw-ir), new_outleft', (os-ow)))
                          throwErrno "iconvRecoder" -- illegal sequence, or some other error

#endif /* !mingw32_HOST_OS */
