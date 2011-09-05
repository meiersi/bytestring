{-# LANGUAGE ScopedTypeVariables, CPP, ForeignFunctionInterface #-}
-- | Copyright   : (c) 2010 Jasper Van der Jeugt 
--               (c) 2010 - 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Encodings using UTF-8 encoded Unicode characters. 
--
module Data.ByteString.Lazy.Builder.BasicEncoding.Utf8
    ( 
      -- * Characters
      char

      -- * Decimal numbers
      -- | Decimal encoding of numbers using UTF-8 encoded characters.
    , int8Dec
    , int16Dec
    , int32Dec
    , int64Dec
    , intDec

    , word8Dec
    , word16Dec
    , word32Dec
    , word64Dec
    , wordDec

    {-
    -- These are the function currently provided by Bryan O'Sullivans
    -- double-conversion library.
    -- 
    -- , float
    -- , floatWith
    -- , double
    -- , doubleWith
    -}

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
      -- | Encoding the bytes of fixed-width positive integers as hexadecimal
      -- numbers using lower-case characters that are UTF-8 encoded. For
      -- example,
      --
      -- > showEncoding word16HexFixed 0x0a10 = "0a10"
      --
    , int8HexFixed
    , int16HexFixed
    , int32HexFixed
    , int64HexFixed
    , word8HexFixed
    , word16HexFixed
    , word32HexFixed
    , word64HexFixed
    , floatHexFixed
    , doubleHexFixed

    ) where

import Foreign
import Foreign.C.Types
import Data.Char (ord)

import Codec.Bounded.Encoding.Floating
import Codec.Bounded.Encoding.Internal
import Codec.Bounded.Encoding.Internal.Base16

-- | Encode a 'Char' using the UTF-8 encoding.
--
{-# INLINE char #-}
char :: Encoding Char
char = boundedEncoding 4 (encodeCharUtf8 f1 f2 f3 f4)
  where
    pokeN n io op = io op >> return (op `plusPtr` n)

    f1 x1          = pokeN 1 $ \op -> do pokeByteOff op 0 x1

    f2 x1 x2       = pokeN 2 $ \op -> do pokeByteOff op 0 x1
                                         pokeByteOff op 1 x2
                   
    f3 x1 x2 x3    = pokeN 3 $ \op -> do pokeByteOff op 0 x1
                                         pokeByteOff op 1 x2
                                         pokeByteOff op 2 x3

    f4 x1 x2 x3 x4 = pokeN 4 $ \op -> do pokeByteOff op 0 x1
                                         pokeByteOff op 1 x2
                                         pokeByteOff op 2 x3
                                         pokeByteOff op 3 x4

-- | Encode a Unicode character to another datatype, using UTF-8. This function
-- acts as an abstract way of encoding characters, as it is unaware of what
-- needs to happen with the resulting bytes: you have to specify functions to
-- deal with those.
--
encodeCharUtf8 :: (Word8 -> a)                             -- ^ 1-byte UTF-8
               -> (Word8 -> Word8 -> a)                    -- ^ 2-byte UTF-8
               -> (Word8 -> Word8 -> Word8 -> a)           -- ^ 3-byte UTF-8
               -> (Word8 -> Word8 -> Word8 -> Word8 -> a)  -- ^ 4-byte UTF-8
               -> Char                                     -- ^ Input 'Char'
               -> a                                        -- ^ Result
encodeCharUtf8 f1 f2 f3 f4 c = case ord c of
    x | x <= 0x7F -> f1 $ fromIntegral x
      | x <= 0x07FF ->
           let x1 = fromIntegral $ (x `shiftR` 6) + 0xC0
               x2 = fromIntegral $ (x .&. 0x3F)   + 0x80
           in f2 x1 x2
      | x <= 0xFFFF ->
           let x1 = fromIntegral $ (x `shiftR` 12) + 0xE0
               x2 = fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80
               x3 = fromIntegral $ (x .&. 0x3F) + 0x80
           in f3 x1 x2 x3
      | otherwise ->
           let x1 = fromIntegral $ (x `shiftR` 18) + 0xF0
               x2 = fromIntegral $ ((x `shiftR` 12) .&. 0x3F) + 0x80
               x3 = fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80
               x4 = fromIntegral $ (x .&. 0x3F) + 0x80
           in f4 x1 x2 x3 x4
{-# INLINE encodeCharUtf8 #-}


------------------------------------------------------------------------------
-- Decimal Encoding
------------------------------------------------------------------------------

-- Signed integers
------------------

foreign import ccall unsafe "static int_dec" c_int_dec
    :: CInt -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "static long_long_int_dec" c_long_long_int_dec
    :: CLLong -> Ptr Word8 -> IO (Ptr Word8)

{-# INLINE encodeIntDecimal #-}
encodeIntDecimal :: Integral a => Int -> Encoding a
encodeIntDecimal bound = boundedEncoding bound $ c_int_dec . fromIntegral

-- | Decimal encoding of an 'Int8'.
{-# INLINE int8Dec #-}
int8Dec :: Encoding Int8
int8Dec = encodeIntDecimal 4

-- | Decimal encoding of an 'Int16'.
{-# INLINE int16Dec #-}
int16Dec :: Encoding Int16
int16Dec = encodeIntDecimal 6


-- | Decimal encoding of an 'Int32'.
{-# INLINE int32Dec #-}
int32Dec :: Encoding Int32
int32Dec = encodeIntDecimal 11

-- | Decimal encoding of an 'Int64'.
{-# INLINE int64Dec #-}
int64Dec :: Encoding Int64
int64Dec = boundedEncoding 20 $ c_long_long_int_dec . fromIntegral

-- | Decimal encoding of an 'Int'.
{-# INLINE intDec #-}
intDec :: Encoding Int
#if WORD_SIZE_IN_BITS < 64
intDec = int32Dec #. fromIntegral
#else
intDec = int64Dec #. fromIntegral
#endif


-- Unsigned integers
--------------------

foreign import ccall unsafe "static uint_dec" c_uint_dec
    :: CUInt -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "static long_long_uint_dec" c_long_long_uint_dec
    :: CULLong -> Ptr Word8 -> IO (Ptr Word8)

{-# INLINE encodeWordDecimal #-}
encodeWordDecimal :: Integral a => Int -> Encoding a
encodeWordDecimal bound = boundedEncoding bound $ c_uint_dec . fromIntegral

-- | Decimal encoding of a 'Word8'.
{-# INLINE word8Dec #-}
word8Dec :: Encoding Word8
word8Dec = encodeWordDecimal 3

-- | Decimal encoding of a 'Word16'.
{-# INLINE word16Dec #-}
word16Dec :: Encoding Word16
word16Dec = encodeWordDecimal 5

-- | Decimal encoding of a 'Word32'.
{-# INLINE word32Dec #-}
word32Dec :: Encoding Word32
word32Dec = encodeWordDecimal 10

-- | Decimal encoding of a 'Word64'.
{-# INLINE word64Dec #-}
word64Dec :: Encoding Word64
word64Dec = boundedEncoding 20 $ c_long_long_uint_dec . fromIntegral

-- | Decimal encoding of a 'Word'.
{-# INLINE wordDec #-}
wordDec :: Encoding Word
#if WORD_SIZE_IN_BITS < 64
wordDec = word32Dec #. fromIntegral
#else
wordDec = word64Dec #. fromIntegral
#endif

------------------------------------------------------------------------------
-- Hexadecimal Encoding
------------------------------------------------------------------------------

-- without lead
---------------

foreign import ccall unsafe "static uint_hex" c_uint_hex
    :: CUInt -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "static long_long_uint_hex" c_long_long_uint_hex
    :: CULLong -> Ptr Word8 -> IO (Ptr Word8)

{-# INLINE encodeWordHex #-}
encodeWordHex :: forall a. (Storable a, Integral a) => Encoding a
encodeWordHex = 
    boundedEncoding (2 * sizeOf (undefined :: a)) $ c_uint_hex  . fromIntegral

{-# INLINE word8Hex #-}
word8Hex :: Encoding Word8
word8Hex = encodeWordHex

{-# INLINE word16Hex #-}
word16Hex :: Encoding Word16
word16Hex = encodeWordHex

{-# INLINE word32Hex #-}
word32Hex :: Encoding Word32
word32Hex = encodeWordHex

{-# INLINE word64Hex #-}
word64Hex :: Encoding Word64
word64Hex = boundedEncoding 16 $ c_long_long_uint_hex . fromIntegral

{-# INLINE wordHex #-}
wordHex :: Encoding Word
#if WORD_SIZE_IN_BITS < 64
wordHex = word32Hex #. fromIntegral
#else
wordHex = word64Hex #. fromIntegral
#endif


-- fixed width; leading zeroes
------------------------------

-- | Encode a 'Word8' using 2 nibbles (hexadecimal digits).
{-# INLINE word8HexFixed #-}
word8HexFixed :: Encoding Word8
word8HexFixed = exactEncoding 2 $ 
    \x op -> poke (castPtr op) =<< encode8_as_16h lowerTable x

-- | Encode a 'Word16' using 4 nibbles.
{-# INLINE word16HexFixed #-}
word16HexFixed :: Encoding Word16
word16HexFixed = encodePair word8HexFixed word8HexFixed #.
    (\x -> (fromIntegral $ x `shiftR` 8, fromIntegral x))

-- | Encode a 'Word32' using 8 nibbles.
{-# INLINE word32HexFixed #-}
word32HexFixed :: Encoding Word32
word32HexFixed = encodePair word16HexFixed word16HexFixed #.
    (\x -> (fromIntegral $ x `shiftR` 16, fromIntegral x))

-- | Encode a 'Word64' using 16 nibbles.
{-# INLINE word64HexFixed #-}
word64HexFixed :: Encoding Word64
word64HexFixed = encodePair word32HexFixed word32HexFixed #.
    (\x -> (fromIntegral $ x `shiftR` 32, fromIntegral x))

-- | Encode a 'Int8' using 2 nibbles (hexadecimal digits).
{-# INLINE int8HexFixed #-}
int8HexFixed :: Encoding Int8
int8HexFixed = word8HexFixed #. fromIntegral

-- | Encode a 'Int16' using 4 nibbles.
{-# INLINE int16HexFixed #-}
int16HexFixed :: Encoding Int16
int16HexFixed = word16HexFixed #. fromIntegral

-- | Encode a 'Int32' using 8 nibbles.
{-# INLINE int32HexFixed #-}
int32HexFixed :: Encoding Int32
int32HexFixed = word32HexFixed #. fromIntegral

-- | Encode a 'Int64' using 16 nibbles.
{-# INLINE int64HexFixed #-}
int64HexFixed :: Encoding Int64
int64HexFixed = word64HexFixed #. fromIntegral

-- | Encode an IEEE 'Float' using 8 nibbles.
{-# INLINE floatHexFixed #-}
floatHexFixed :: Encoding Float
floatHexFixed = word32HexFixed #. coerceFloatToWord32

-- | Encode an IEEE 'Double' using 16 nibbles.
{-# INLINE doubleHexFixed #-}
doubleHexFixed :: Encoding Double
doubleHexFixed = word64HexFixed #. coerceDoubleToWord64
