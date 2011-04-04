{-# LANGUAGE CPP, NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Encoding
-- Copyright   :  (c) The University of Glasgow, 2008-2011
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Foreign marshalling support for CStrings with configurable encodings
--
-----------------------------------------------------------------------------

module GHC.Foreign (
  -- * C strings with a configurable encoding
  
  -- conversion of C strings into Haskell strings
  --
  peekCString,       -- :: TextEncoding -> CString    -> IO String
  peekCStringLen,    -- :: TextEncoding -> CStringLen -> IO String

  -- conversion of Haskell strings into C strings
  --
  newCString,        -- :: TextEncoding -> String -> IO CString
  newCStringLen,     -- :: TextEncoding -> String -> IO CStringLen

  -- conversion of Haskell strings into C strings using temporary storage
  --
  withCString,       -- :: TextEncoding -> String -> (CString    -> IO a) -> IO a
  withCStringLen,    -- :: TextEncoding -> String -> (CStringLen -> IO a) -> IO a

  charIsRepresentable, -- :: TextEncoding -> Char -> IO Bool
  ) where

import Foreign.Marshal.Array
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

import Data.Word

-- Imports for the locale-encoding version of marshallers
import Control.Monad

import Data.Tuple (fst)

--import {-# SOURCE #-} Debug.Trace ( trace )
--import GHC.Show                   ( show )

import Foreign.Marshal.Alloc
import Foreign.ForeignPtr

import GHC.Err (undefined)
import GHC.List
import GHC.Num
import GHC.Base

import GHC.IO
import GHC.IO.Exception
import GHC.IO.Buffer
import GHC.IO.Encoding.Types

-- These definitions are identical to those in Foreign.C.String, but copied in here to avoid a cycle:
type CString    = Ptr CChar
type CStringLen = (Ptr CChar, Int)

-- exported functions
-- ------------------

-- | Marshal a NUL terminated C string into a Haskell string.
--
peekCString    :: TextEncoding -> CString -> IO String
peekCString enc cp = do
    sz <- lengthArray0 nUL cp
    peekEncodedCString enc (cp, sz * cCharSize)

-- | Marshal a C string with explicit length into a Haskell string.
--
peekCStringLen           :: TextEncoding -> CStringLen -> IO String
peekCStringLen = peekEncodedCString

-- | Marshal a Haskell string into a NUL terminated C string.
--
-- * the Haskell string may /not/ contain any NUL characters
--
-- * new storage is allocated for the C string and must be
--   explicitly freed using 'Foreign.Marshal.Alloc.free' or
--   'Foreign.Marshal.Alloc.finalizerFree'.
--
newCString :: TextEncoding -> String -> IO CString
newCString enc = liftM fst . newEncodedCString enc True

-- | Marshal a Haskell string into a C string (ie, character array) with
-- explicit length information.
--
-- * new storage is allocated for the C string and must be
--   explicitly freed using 'Foreign.Marshal.Alloc.free' or
--   'Foreign.Marshal.Alloc.finalizerFree'.
--
newCStringLen     :: TextEncoding -> String -> IO CStringLen
newCStringLen enc = newEncodedCString enc False

-- | Marshal a Haskell string into a NUL terminated C string using temporary
-- storage.
--
-- * the Haskell string may /not/ contain any NUL characters
--
-- * the memory is freed when the subcomputation terminates (either
--   normally or via an exception), so the pointer to the temporary
--   storage must /not/ be used after this.
--
withCString :: TextEncoding -> String -> (CString -> IO a) -> IO a
withCString enc s act = withEncodedCString enc True s $ \(cp, _sz) -> act cp

-- | Marshal a Haskell string into a C string (ie, character array)
-- in temporary storage, with explicit length information.
--
-- * the memory is freed when the subcomputation terminates (either
--   normally or via an exception), so the pointer to the temporary
--   storage must /not/ be used after this.
--
withCStringLen         :: TextEncoding -> String -> (CStringLen -> IO a) -> IO a
withCStringLen enc = withEncodedCString enc False


-- | Determines whether a character can be accurately encoded in a 'CString'.
--
-- For this to work properly, the supplied 'TextEncoding' must throw an
-- IOException upon encountering an invalid sequence.
--
-- Pretty much anyone who uses this function is in a state of sin because
-- whether or not a character is encodable will, in general, depend on the
-- context in which it occurs.
charIsRepresentable :: TextEncoding -> Char -> IO Bool
charIsRepresentable enc c = withEncodedCString enc False [c] (\_ -> return True) `catchException` (\e -> let _ = e :: IOException in return False)

-- auxiliary definitions
-- ----------------------

-- C's end of string character
nUL :: CChar
nUL  = 0

-- Size of a CChar in bytes
cCharSize :: Int
cCharSize = sizeOf (undefined :: CChar)


{-# INLINE peekEncodedCString #-}
peekEncodedCString :: TextEncoding -- ^ Encoding of CString
                   -> CStringLen
                   -> IO String    -- ^ String in Haskell terms
peekEncodedCString (TextEncoding { mkTextDecoder = mk_decoder }) (p, sz_bytes)
  = bracket mk_decoder close $ \decoder -> do
      let cHUNK_SIZE = 4096 -- Decode buffer chunk size in characters
      from0 <- fmap (\fp -> bufferAdd sz_bytes (emptyBuffer fp sz_bytes ReadBuffer)) $ newForeignPtr_ (castPtr p)
      to <- newCharBuffer cHUNK_SIZE WriteBuffer

      let go {- iteration -} from = {- trace ("peekEncodedCString: " ++ show iteration) $ -} do
            (from', to') <- encode decoder from to
            to_chars <- withBuffer to' $ peekArray (bufferElems to')
            if isEmptyBuffer from'
             then return to_chars
             else fmap (to_chars++) $ go {- (iteration + 1) -} from'

      go {- (0 :: Int) -} from0

{-# INLINE withEncodedCString #-}
withEncodedCString :: TextEncoding         -- ^ Encoding of CString to create
                   -> Bool                 -- ^ Null-terminate?
                   -> String               -- ^ String to encode
                   -> (CStringLen -> IO a) -- ^ Worker that can safely use the allocated memory
                   -> IO a
withEncodedCString = withEncodedCString' (\bytes act -> allocaBytes bytes (\p -> newForeignPtr_ p >>= act))

{-# INLINE newEncodedCString #-}
newEncodedCString :: TextEncoding  -- ^ Encoding of CString to create
                  -> Bool          -- ^ Null-terminate?
                  -> String        -- ^ String to encode
                  -> IO CStringLen
newEncodedCString encoding null_terminate s = withEncodedCString' (\bytes act -> mallocForeignPtrBytes bytes >>= act) encoding null_terminate s return

{-# INLINE withEncodedCString' #-}
withEncodedCString' :: (Int -> (ForeignPtr Word8 -> IO a) -> IO a) -- ^ How to allocate memory for the CString buffer
                    -> TextEncoding                                -- ^ Encoding of CString to create
                    -> Bool                                        -- ^ Null-terminate?
                    -> String                                      -- ^ String to encode
                    -> (CStringLen -> IO a)                        -- ^ Worker that can safely use the allocated memory
                    -> IO a
withEncodedCString' my_malloc (TextEncoding { mkTextEncoder = mk_encoder }) null_terminate s act
  = bracket mk_encoder close $ \encoder -> withArrayLen s $ \sz p -> do
      from <- fmap (\fp -> bufferAdd sz (emptyBuffer fp sz ReadBuffer)) $ newForeignPtr_ p

      let go {- iteration -} to_sz_bytes = {- trace ("withEncodedCString': " ++ show iteration) $ -} my_malloc to_sz_bytes $ \to_fp -> do
            let to = emptyBuffer to_fp to_sz_bytes WriteBuffer
            (from', to') <- encode encoder from to
            if isEmptyBuffer from' && (not null_terminate || (bufferAvailable to' > 0))
             then do
                 let bytes = bufferElems to'
                 withBuffer to' $ \to_ptr -> do
                     when null_terminate $ pokeElemOff to_ptr (bufR to') 0
                     act (castPtr to_ptr, bytes) -- NB: the length information is specified as being in *bytes*
             -- Only increase buffer size for next iteration if we used it all this time. This can occur if the encoder
             -- returns without consuming all the input that it could have because e.g. it encountered an invalid
             -- character and is going to throw an exception next time we invoke it.
             else -- trace (summaryBuffer from ++ " " ++ summaryBuffer to ++ " " ++ summaryBuffer from' ++ " " ++ summaryBuffer to') $
                  go {- (iteration + 1) -} (if bufferAvailable to' >= cCharSize then to_sz_bytes else to_sz_bytes * 2)

      -- If the input string is ASCII, this value will ensure we only allocate once
      go {- (0 :: Int) -} (cCharSize * (sz + 1))
