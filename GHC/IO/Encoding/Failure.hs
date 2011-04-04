{-# LANGUAGE NoImplicitPrelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Encoding.Failure
-- Copyright   :  (c) The University of Glasgow, 2008-2011
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Types for specifying how text encoding/decoding fails
--
-----------------------------------------------------------------------------

module GHC.IO.Encoding.Failure (
    CodingFailureMode(..), codingFailureModeSuffix,
    unrepresentableChar, unrepresentableByte,
    decodeToSurrogateCharacter, encodeSurrogateCharacter
  ) where

import GHC.Base
import GHC.Word
import GHC.Show
import GHC.Num
import GHC.Real ( fromIntegral )

import Data.Maybe

-- | The 'CodingFailureMode' is used to construct 'TextEncoding's, and specifies
-- how they handle illegal sequences.
data CodingFailureMode = ErrorOnCodingFailure         -- ^ Throw an error when an illegal sequence is encountered
                       | IgnoreCodingFailure          -- ^ Attempt to ignore and recover if an illegal sequence is encountered
                       | TransliterateCodingFailure   -- ^ Replace with the closest visual match upon an illegal sequence
                       | SurrogateEscapeFailure       -- ^ Use the surrogate escape mechanism to allow illegal sequences to be roundtripped
                       deriving (Show)

codingFailureModeSuffix :: CodingFailureMode -> String
codingFailureModeSuffix ErrorOnCodingFailure       = ""
codingFailureModeSuffix IgnoreCodingFailure        = "//IGNORE"
codingFailureModeSuffix TransliterateCodingFailure = "//TRANSLIT"
codingFailureModeSuffix SurrogateEscapeFailure     = "//SURROGATE" -- FIXME: parse

-- | In transliterate mode, we use this character when decoding unknown bytes.
--
-- This is the defined Unicode replacement character: <http://www.fileformat.info/info/unicode/char/0fffd/index.htm>
unrepresentableChar :: Char
unrepresentableChar = '\xFFFD'

-- | In transliterate mode, we use this character when encoding unrepresentable characters
--
-- We assume that all possible encodings are ASCII-supersets, so we can always decode to the
-- ASCII '?' character.
unrepresentableByte :: Word8
unrepresentableByte = 63

decodeToSurrogateCharacter :: Word8 -> Char
decodeToSurrogateCharacter b
  | b < 128   = chr (fromIntegral b) -- Disallow 'smuggling' of ASCII bytes. Assumes encoding is ASCII-superset.
  | otherwise = chr (0xDC00 + fromIntegral b)

encodeSurrogateCharacter :: Char -> Maybe Word8
encodeSurrogateCharacter c
    | 0xDC80 <= x && x < 0xDD00 = Just (fromIntegral x) -- Discard high byte
    | otherwise                 = Nothing
  where x = ord c
