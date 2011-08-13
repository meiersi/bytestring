{-# LANGUAGE ScopedTypeVariables, CPP, ForeignFunctionInterface #-}
-- | Copyright   : (c) 2010 Jasper Van der Jeugt 
--               (c) 2010 - 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Creating 'Builder's using UTF-8 encoded Unicode characters. 
--
module Data.ByteString.Lazy.Builder.Utf8
    ( 
      -- * Characters
      char
    , string

      -- * Decimal numbers
      -- | Decimal encoding of numbers using UTF-8 encoded characters.
    , int8Dec
    , int16Dec
    , int32Dec
    , int64Dec
    , intDec
    , integerDec

    , word8Dec
    , word16Dec
    , word32Dec
    , word64Dec
    , wordDec

    , floatDec
    , doubleDec

      -- * Hexadecimal numbers
      
      -- | Encoding positive integers as hexadecimal numbers using lower-case
      -- characters that are UTF-8 encoded. The shortest
      -- possible representation is used. For example,
      --
      -- > showEncoding word16Hex 0x0a10 = "a10"
      --
      -- Note that there is no support for using upper-case characters. Please
      -- contact the maintainer if your application cannot work without
      -- hexadecimal encodings that use upper-case characters.
      --
    , word8Hex
    , word16Hex
    , word32Hex
    , word64Hex
    , wordHex

      -- * Fixed-width hexadecimal numbers
      --
    , int8HexFixed
    , int16HexFixed
    , int32HexFixed
    , int64HexFixed
    , intHexFixed
    , word8HexFixed
    , word16HexFixed
    , word32HexFixed
    , word64HexFixed
    , wordHexFixed
    , floatHexFixed
    , doubleHexFixed

    ) where

import Foreign

import Data.ByteString.Lazy.Builder (Builder)

import qualified Data.ByteString.Lazy.Builder.BoundedEncoding      as E
import qualified Data.ByteString.Lazy.Builder.BoundedEncoding.Utf8 as E

-- | Encode a 'Char' using the UTF-8 encoding.
--
{-# INLINE char #-}
char :: Char -> Builder
char = E.encodeWith E.char

-- | Encode a 'String' using the UTF-8 encoding.
--
{-# INLINE string #-}
string :: String -> Builder
string = E.encodeListWith E.char


------------------------------------------------------------------------------
-- Decimal Encoding
------------------------------------------------------------------------------

-- Signed integers
------------------

-- | Decimal encoding of an 'Int8' using the ASCII digits.
{-# INLINE int8Dec #-}
int8Dec :: Int8 -> Builder
int8Dec = E.encodeWith E.int8Dec

-- | Decimal encoding of an 'Int16' using the ASCII digits.
{-# INLINE int16Dec #-}
int16Dec :: Int16 -> Builder
int16Dec = E.encodeWith E.int16Dec


-- | Decimal encoding of an 'Int32' using the ASCII digits.
{-# INLINE int32Dec #-}
int32Dec :: Int32 -> Builder
int32Dec = E.encodeWith E.int32Dec

-- | Decimal encoding of an 'Int64' using the ASCII digits.
{-# INLINE int64Dec #-}
int64Dec :: Int64 -> Builder
int64Dec = E.encodeWith E.int64Dec

-- | Decimal encoding of an 'Int' using the ASCII digits.
{-# INLINE intDec #-}
intDec :: Int -> Builder
intDec = E.encodeWith E.intDec

-- | /Currently slow./ Decimal encoding of an 'Integer' using the ASCII digits.
{-# INLINE integerDec #-}
integerDec :: Integer -> Builder
integerDec = string . show


-- Unsigned integers
--------------------

-- | Decimal encoding of a 'Word8' using the ASCII digits.
{-# INLINE word8Dec #-}
word8Dec :: Word8 -> Builder
word8Dec = E.encodeWith E.word8Dec

-- | Decimal encoding of a 'Word16' using the ASCII digits.
{-# INLINE word16Dec #-}
word16Dec :: Word16 -> Builder
word16Dec = E.encodeWith E.word16Dec

-- | Decimal encoding of a 'Word32' using the ASCII digits.
{-# INLINE word32Dec #-}
word32Dec :: Word32 -> Builder
word32Dec = E.encodeWith E.word32Dec

-- | Decimal encoding of a 'Word64' using the ASCII digits.
{-# INLINE word64Dec #-}
word64Dec :: Word64 -> Builder
word64Dec = E.encodeWith E.word64Dec

-- | Decimal encoding of a 'Word' using the ASCII digits.
{-# INLINE wordDec #-}
wordDec :: Word -> Builder
wordDec = E.encodeWith E.wordDec


-- Floating point numbers
-------------------------

-- TODO: Use Bryan O'Sullivan's double-conversion package to speed it up.

-- | /Currently slow./ Decimal encoding of an IEEE 'Float' using ASCII digits and characters.
{-# INLINE floatDec #-}
floatDec :: Float -> Builder
floatDec = string . show

-- | /Currently slow./ Decimal encoding of an IEEE 'Double' using ASCII digits and characters.
{-# INLINE doubleDec #-}
doubleDec :: Float -> Builder
doubleDec = string . show


------------------------------------------------------------------------------
-- Hexadecimal Encoding
------------------------------------------------------------------------------

-- without lead
---------------

-- | Shortest hexadecimal encoding of a 'Word8' using ASCII digits and lower-case characters.
{-# INLINE word8Hex #-}
word8Hex :: Word8 -> Builder
word8Hex = E.encodeWith E.word8Hex

-- | Shortest hexadecimal encoding of a 'Word16' using ASCII digits and lower-case characters.
{-# INLINE word16Hex #-}
word16Hex :: Word16 -> Builder
word16Hex = E.encodeWith E.word16Hex

-- | Shortest hexadecimal encoding of a 'Word32' using ASCII digits and lower-case characters.
{-# INLINE word32Hex #-}
word32Hex :: Word32 -> Builder
word32Hex = E.encodeWith E.word32Hex

-- | Shortest hexadecimal encoding of a 'Word64' using ASCII digits and lower-case characters.
{-# INLINE word64Hex #-}
word64Hex :: Word64 -> Builder
word64Hex = E.encodeWith E.word64Hex

-- | Shortest hexadecimal encoding of a 'Word' using ASCII digits and lower-case characters.
{-# INLINE wordHex #-}
wordHex :: Word -> Builder
wordHex = E.encodeWith E.wordHex


-- fixed width; leading zeroes
------------------------------


-- | Encode a 'Word8' using 2 nibbles (hexadecimal digits).
{-# INLINE word8HexFixed #-}
word8HexFixed :: Word8 -> Builder
word8HexFixed = E.encodeWith E.word8HexFixed

-- | Encode a 'Word16' using 4 nibbles.
{-# INLINE word16HexFixed #-}
word16HexFixed :: Word16 -> Builder
word16HexFixed = E.encodeWith E.word16HexFixed

-- | Encode a 'Word32' using 8 nibbles.
{-# INLINE word32HexFixed #-}
word32HexFixed :: Word32 -> Builder
word32HexFixed = E.encodeWith E.word32HexFixed

-- | Encode a 'Word64' using 16 nibbles.
{-# INLINE word64HexFixed #-}
word64HexFixed :: Word64 -> Builder
word64HexFixed = E.encodeWith E.word64HexFixed

-- | Encode a 'Word' using 8 or 16 nibbles depending on whether you are on a
-- 32-bit or a 64-bit machine. 
{-# INLINE wordHexFixed #-}
wordHexFixed :: Word -> Builder
wordHexFixed = E.encodeWith E.wordHexFixed


-- | Encode a 'Int8' using 2 nibbles (hexadecimal digits).
{-# INLINE int8HexFixed #-}
int8HexFixed :: Int8 -> Builder
int8HexFixed = E.encodeWith E.int8HexFixed

-- | Encode a 'Int16' using 4 nibbles.
{-# INLINE int16HexFixed #-}
int16HexFixed :: Int16 -> Builder
int16HexFixed = E.encodeWith E.int16HexFixed

-- | Encode a 'Int32' using 8 nibbles.
{-# INLINE int32HexFixed #-}
int32HexFixed :: Int32 -> Builder
int32HexFixed = E.encodeWith E.int32HexFixed

-- | Encode a 'Int64' using 16 nibbles.
{-# INLINE int64HexFixed #-}
int64HexFixed :: Int64 -> Builder
int64HexFixed = E.encodeWith E.int64HexFixed

-- | Encode a 'Int' using 8 or 16 nibbles depending on whether you are on a
-- 32-bit or a 64-bit machine. 
{-# INLINE intHexFixed #-}
intHexFixed :: Int -> Builder
intHexFixed = E.encodeWith E.intHexFixed

-- | Encode an IEEE 'Float' using 8 nibbles.
{-# INLINE floatHexFixed #-}
floatHexFixed :: Float -> Builder
floatHexFixed = E.encodeWith E.floatHexFixed

-- | Encode an IEEE 'Double' using 16 nibbles.
{-# INLINE doubleHexFixed #-}
doubleHexFixed :: Double -> Builder
doubleHexFixed = E.encodeWith E.doubleHexFixed
