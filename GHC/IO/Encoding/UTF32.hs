{-# LANGUAGE NoImplicitPrelude
           , BangPatterns
           , PatternGuards
           , NondecreasingIndentation
           , MagicHash
  #-}
{-# OPTIONS_GHC  -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Encoding.UTF32
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

module GHC.IO.Encoding.UTF32 (
  utf32,
  utf32FailingWith,
  utf32_decode,
  utf32_encode,

  utf32be,
  utf32beFailingWith,
  utf32be_decode,
  utf32be_encode,

  utf32le,
  utf32leFailingWith,
  utf32le_decode,
  utf32le_encode,
  ) where

import GHC.Base
import GHC.Real
import GHC.Num
-- import GHC.IO
import GHC.IO.Exception
import GHC.IO.Buffer
import GHC.IO.Encoding.Failure
import GHC.IO.Encoding.Types
import GHC.Word
import Data.Bits
import Data.Maybe
import GHC.IORef

-- -----------------------------------------------------------------------------
-- The UTF-32 codec: either UTF-32BE or UTF-32LE with a BOM

utf32  :: TextEncoding
utf32 = utf32FailingWith ErrorOnCodingFailure

utf32FailingWith :: CodingFailureMode -> TextEncoding
utf32FailingWith cfm
  = TextEncoding { textEncodingName = "UTF-32" ++ codingFailureModeSuffix cfm,
                   mkTextDecoder = utf32_DF cfm,
                   mkTextEncoder = utf32_EF cfm }

utf32_DF :: CodingFailureMode -> IO (TextDecoder (Maybe DecodeBuffer))
utf32_DF cfm = do
  seen_bom <- newIORef Nothing
  return (BufferCodec {
             encode   = utf32_decodeFailingWith cfm seen_bom,
             close    = return (),
             getState = readIORef seen_bom,
             setState = writeIORef seen_bom
          })

utf32_EF :: CodingFailureMode -> IO (TextEncoder Bool)
utf32_EF cfm = do
  done_bom <- newIORef False
  return (BufferCodec {
             encode   = utf32_encodeFailingWith cfm done_bom,
             close    = return (),
             getState = readIORef done_bom,
             setState = writeIORef done_bom
          })

utf32_encode :: IORef Bool -> EncodeBuffer
utf32_encode = utf32_encodeFailingWith ErrorOnCodingFailure

utf32_encodeFailingWith :: CodingFailureMode -> IORef Bool -> EncodeBuffer
utf32_encodeFailingWith cfm done_bom input
  output@Buffer{ bufRaw=oraw, bufL=_, bufR=ow, bufSize=os }
 = do
  b <- readIORef done_bom
  if b then utf32_native_encode cfm input output
       else if os - ow < 4
               then return (input,output)
               else do
                    writeIORef done_bom True
                    writeWord8Buf oraw ow     bom0
                    writeWord8Buf oraw (ow+1) bom1
                    writeWord8Buf oraw (ow+2) bom2
                    writeWord8Buf oraw (ow+3) bom3
                    utf32_native_encode cfm input output{ bufR = ow+4 }

utf32_decode :: IORef (Maybe DecodeBuffer) -> DecodeBuffer
utf32_decode = utf32_decodeFailingWith ErrorOnCodingFailure

utf32_decodeFailingWith :: CodingFailureMode -> IORef (Maybe DecodeBuffer) -> DecodeBuffer
utf32_decodeFailingWith cfm seen_bom
  input@Buffer{  bufRaw=iraw, bufL=ir, bufR=iw,  bufSize=_  }
  output
 = do
   mb <- readIORef seen_bom
   case mb of
     Just decode -> decode input output
     Nothing ->
       if iw - ir < 4 then return (input,output) else do
       c0 <- readWord8Buf iraw ir
       c1 <- readWord8Buf iraw (ir+1)
       c2 <- readWord8Buf iraw (ir+2)
       c3 <- readWord8Buf iraw (ir+3)
       case () of
        _ | c0 == bom0 && c1 == bom1 && c2 == bom2 && c3 == bom3 -> do
               writeIORef seen_bom (Just (utf32be_decodeFailingWith cfm))
               utf32be_decodeFailingWith cfm input{ bufL= ir+4 } output
        _ | c0 == bom3 && c1 == bom2 && c2 == bom1 && c3 == bom0 -> do
               writeIORef seen_bom (Just (utf32le_decodeFailingWith cfm))
               utf32le_decodeFailingWith cfm input{ bufL= ir+4 } output
          | otherwise -> do
               writeIORef seen_bom (Just (utf32_native_decode cfm))
               utf32_native_decode cfm input output


bom0, bom1, bom2, bom3 :: Word8
bom0 = 0
bom1 = 0
bom2 = 0xfe
bom3 = 0xff

-- choose UTF-32BE by default for UTF-32 output
utf32_native_decode :: CodingFailureMode -> DecodeBuffer
utf32_native_decode = utf32be_decodeFailingWith

utf32_native_encode :: CodingFailureMode -> EncodeBuffer
utf32_native_encode = utf32be_encodeFailingWith

-- -----------------------------------------------------------------------------
-- UTF32LE and UTF32BE

utf32be :: TextEncoding
utf32be = utf32beFailingWith ErrorOnCodingFailure

utf32beFailingWith :: CodingFailureMode -> TextEncoding
utf32beFailingWith cfm
  = TextEncoding { textEncodingName = "UTF-32BE" ++ codingFailureModeSuffix cfm,
                   mkTextDecoder = utf32be_DF cfm,
                   mkTextEncoder = utf32be_EF cfm }

utf32be_DF :: CodingFailureMode -> IO (TextDecoder ())
utf32be_DF cfm =
  return (BufferCodec {
             encode   = utf32be_decodeFailingWith cfm,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })

utf32be_EF :: CodingFailureMode -> IO (TextEncoder ())
utf32be_EF cfm =
  return (BufferCodec {
             encode   = utf32be_encodeFailingWith cfm,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })


utf32le :: TextEncoding
utf32le = utf32leFailingWith ErrorOnCodingFailure

utf32leFailingWith :: CodingFailureMode -> TextEncoding
utf32leFailingWith cfm
  = TextEncoding { textEncodingName = "UTF-32LE" ++ codingFailureModeSuffix cfm,
                   mkTextDecoder = utf32le_DF cfm,
                   mkTextEncoder = utf32le_EF cfm }

utf32le_DF :: CodingFailureMode -> IO (TextDecoder ())
utf32le_DF cfm =
  return (BufferCodec {
             encode   = utf32le_decodeFailingWith cfm,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })

utf32le_EF :: CodingFailureMode -> IO (TextEncoder ())
utf32le_EF cfm =
  return (BufferCodec {
             encode   = utf32le_encodeFailingWith cfm,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })


utf32be_decode :: DecodeBuffer
utf32be_decode = utf32be_decodeFailingWith ErrorOnCodingFailure

utf32be_decodeFailingWith :: CodingFailureMode -> DecodeBuffer
utf32be_decodeFailingWith cfm
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let 
       loop !ir !ow
         | ow >= os || iw - ir < 4 =  done ir ow
         | otherwise = do
              c0 <- readWord8Buf iraw ir
              c1 <- readWord8Buf iraw (ir+1)
              c2 <- readWord8Buf iraw (ir+2)
              c3 <- readWord8Buf iraw (ir+3)
              let x1 = chr4 c0 c1 c2 c3
              if not (validate x1) then invalid c0 c1 c2 c3 (ir+4) else do
              ow' <- writeCharBuf oraw ow x1
              loop (ir+4) ow'
         where
           invalid c0 c1 c2 c3 ir' = case cfm of
               IgnoreCodingFailure -> loop ir' ow
               TransliterateCodingFailure -> do
                 ow' <- writeCharBuf oraw ow unrepresentableChar
                 loop ir' ow'
               SurrogateEscapeFailure | (os-ow) < 4 -> done ir ow
                                      | otherwise -> do
                 -- We know that 4 characters is sufficient even with CHARBUF_UTF16
                 -- because surrogate characters are always of the form 0xDCxx
                 ow1 <- writeCharBuf oraw ow (decodeToSurrogateCharacter c0)
                 ow2 <- writeCharBuf oraw ow1 (decodeToSurrogateCharacter c1)
                 ow3 <- writeCharBuf oraw ow2 (decodeToSurrogateCharacter c2)
                 ow4 <- writeCharBuf oraw ow3 (decodeToSurrogateCharacter c3)
                 loop ir' ow4
               _ | ir > ir0  -> done ir ow
                 | otherwise -> ioe_decodingError

       -- lambda-lifted, to avoid thunks being built in the inner-loop:
       done !ir !ow = return (if ir == iw then input{ bufL=0, bufR=0 }
                                          else input{ bufL=ir },
                         output{ bufR=ow })
    in
    loop ir0 ow0

utf32le_decode :: DecodeBuffer
utf32le_decode = utf32le_decodeFailingWith ErrorOnCodingFailure

utf32le_decodeFailingWith :: CodingFailureMode -> DecodeBuffer
utf32le_decodeFailingWith cfm
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let 
       loop !ir !ow
         | ow >= os || iw - ir < 4 =  done ir ow
         | otherwise = do
              c0 <- readWord8Buf iraw ir
              c1 <- readWord8Buf iraw (ir+1)
              c2 <- readWord8Buf iraw (ir+2)
              c3 <- readWord8Buf iraw (ir+3)
              let x1 = chr4 c3 c2 c1 c0
              if not (validate x1) then invalid c0 c1 c2 c3 (ir+4) else do
              ow' <- writeCharBuf oraw ow x1
              loop (ir+4) ow'
         where
           invalid c0 c1 c2 c3 ir' = case cfm of
               IgnoreCodingFailure -> loop ir' ow
               TransliterateCodingFailure -> do
                 ow' <- writeCharBuf oraw ow unrepresentableChar
                 loop ir' ow'
               SurrogateEscapeFailure | (os-ow) < 4 -> done ir ow
                                      | otherwise -> do
                 -- We know that 4 characters is sufficient even with CHARBUF_UTF16
                 -- because surrogate characters are always of the form 0xDCxx
                 ow1 <- writeCharBuf oraw ow (decodeToSurrogateCharacter c0)
                 ow2 <- writeCharBuf oraw ow1 (decodeToSurrogateCharacter c1)
                 ow3 <- writeCharBuf oraw ow2 (decodeToSurrogateCharacter c2)
                 ow4 <- writeCharBuf oraw ow3 (decodeToSurrogateCharacter c3)
                 loop ir' ow4
               _ | ir > ir0  -> done ir ow
                 | otherwise -> ioe_decodingError

       -- lambda-lifted, to avoid thunks being built in the inner-loop:
       done !ir !ow = return (if ir == iw then input{ bufL=0, bufR=0 }
                                          else input{ bufL=ir },
                         output{ bufR=ow })
    in
    loop ir0 ow0

ioe_decodingError :: IO a
ioe_decodingError = ioException
     (IOError Nothing InvalidArgument "utf32_decode"
          "invalid UTF-32 byte sequence" Nothing Nothing)

utf32be_encode :: EncodeBuffer
utf32be_encode = utf32be_encodeFailingWith ErrorOnCodingFailure

utf32be_encodeFailingWith :: CodingFailureMode -> EncodeBuffer
utf32be_encodeFailingWith cfm
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let 
      done !ir !ow = return (if ir == iw then input{ bufL=0, bufR=0 }
                                         else input{ bufL=ir },
                             output{ bufR=ow })
      loop !ir !ow
        | ir >= iw     =  done ir ow
        | os - ow < 4  =  done ir ow
        | otherwise = do
           (c,ir') <- readCharBuf iraw ir
           case encodeSurrogateCharacter c of
             Just b -> case cfm of
               SurrogateEscapeFailure -> do
                 writeWord8Buf oraw ow b
                 loop ir' (ow+1)
               IgnoreCodingFailure -> loop ir' ow
               TransliterateCodingFailure -> do
                 writeWord8Buf oraw ow     0
                 writeWord8Buf oraw (ow+1) 0
                 writeWord8Buf oraw (ow+2) 0
                 writeWord8Buf oraw (ow+3) unrepresentableByte
                 loop ir' (ow+4)
               ErrorOnCodingFailure | ir > ir0  -> done ir ow
                                    | otherwise -> ioe_encodingError
             Nothing -> do
               let (c0,c1,c2,c3) = ord4 c
               writeWord8Buf oraw ow     c0
               writeWord8Buf oraw (ow+1) c1
               writeWord8Buf oraw (ow+2) c2
               writeWord8Buf oraw (ow+3) c3
               loop ir' (ow+4)
    in
    loop ir0 ow0

utf32le_encode :: EncodeBuffer
utf32le_encode = utf32le_encodeFailingWith ErrorOnCodingFailure

utf32le_encodeFailingWith :: CodingFailureMode -> EncodeBuffer
utf32le_encodeFailingWith cfm
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let
      done !ir !ow = return (if ir == iw then input{ bufL=0, bufR=0 }
                                         else input{ bufL=ir },
                             output{ bufR=ow })
      loop !ir !ow
        | ir >= iw     =  done ir ow
        | os - ow < 4  =  done ir ow
        | otherwise = do
           (c,ir') <- readCharBuf iraw ir
           case encodeSurrogateCharacter c of
             Just b -> case cfm of
               SurrogateEscapeFailure -> do
                 writeWord8Buf oraw ow b
                 loop ir' (ow+1)
               IgnoreCodingFailure -> loop ir' ow
               TransliterateCodingFailure -> do
                 writeWord8Buf oraw ow     unrepresentableByte
                 writeWord8Buf oraw (ow+1) 0
                 writeWord8Buf oraw (ow+2) 0
                 writeWord8Buf oraw (ow+3) 0
                 loop ir' (ow+4)
               ErrorOnCodingFailure | ir > ir0  -> done ir ow
                                    | otherwise -> ioe_encodingError
             Nothing -> do
               let (c0,c1,c2,c3) = ord4 c
               writeWord8Buf oraw ow     c3
               writeWord8Buf oraw (ow+1) c2
               writeWord8Buf oraw (ow+2) c1
               writeWord8Buf oraw (ow+3) c0
               loop ir' (ow+4)
    in
    loop ir0 ow0

ioe_encodingError :: IO a
ioe_encodingError = ioException
     (IOError Nothing InvalidArgument "utf32_encode"
          "surrogate bytes in input" Nothing Nothing)

chr4 :: Word8 -> Word8 -> Word8 -> Word8 -> Char
chr4 (W8# x1#) (W8# x2#) (W8# x3#) (W8# x4#) =
    C# (chr# (z1# +# z2# +# z3# +# z4#))
    where
      !y1# = word2Int# x1#
      !y2# = word2Int# x2#
      !y3# = word2Int# x3#
      !y4# = word2Int# x4#
      !z1# = uncheckedIShiftL# y1# 24#
      !z2# = uncheckedIShiftL# y2# 16#
      !z3# = uncheckedIShiftL# y3# 8#
      !z4# = y4#
{-# INLINE chr4 #-}

ord4 :: Char -> (Word8,Word8,Word8,Word8)
ord4 c = (fromIntegral (x `shiftR` 24), 
          fromIntegral (x `shiftR` 16), 
          fromIntegral (x `shiftR` 8),
          fromIntegral x)
  where
    x = ord c
{-# INLINE ord4 #-}


validate    :: Char -> Bool
validate c = (x1 >= 0x0 && x1 < 0xD800) || (x1 > 0xDFFF && x1 <= 0x10FFFF)
   where x1 = ord c
{-# INLINE validate #-}
