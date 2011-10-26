{-# LANGUAGE ScopedTypeVariables, CPP, ForeignFunctionInterface #-}
-- | Copyright   : (c) 2010 Jasper Van der Jeugt 
--               (c) 2010 - 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Encodings using ASCII encoded Unicode characters. 
--
module Data.ByteString.Lazy.Builder.BasicEncoding.ASCII
    ( 
     
     charASCII

      -- *** Decimal numbers
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

      -- *** Hexadecimal numbers
      
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

      -- *** Fixed-width hexadecimal numbers
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

    -- *** Padded/truncated encodings
    , wordHexFixedBound
    , word64HexFixedBound

    , wordDecFixedBound
    , word64DecFixedBound

    ) where

import Data.ByteString.Lazy.Builder.BasicEncoding.Binary
import Data.ByteString.Lazy.Builder.BasicEncoding.Internal
import Data.ByteString.Lazy.Builder.BasicEncoding.Internal.Floating
import Data.ByteString.Lazy.Builder.BasicEncoding.Internal.Base16
import Data.ByteString.Lazy.Builder.BasicEncoding.Internal.UncheckedShifts

import Data.Char (ord)
import Control.Monad (unless)

import Foreign
import Foreign.C.Types

-- | Encode the least 7-bits of a 'Char' using the ASCII encoding.
{-# INLINE charASCII #-}
charASCII :: FixedEncoding Char
charASCII = (\c -> fromIntegral $ ord c .&. 0x7f) >$< word8


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
encodeIntDecimal :: Integral a => Int -> BoundedEncoding a
encodeIntDecimal bound = boundedEncoding bound $ c_int_dec . fromIntegral

-- | Decimal encoding of an 'Int8'.
{-# INLINE int8Dec #-}
int8Dec :: BoundedEncoding Int8
int8Dec = encodeIntDecimal 4

-- | Decimal encoding of an 'Int16'.
{-# INLINE int16Dec #-}
int16Dec :: BoundedEncoding Int16
int16Dec = encodeIntDecimal 6


-- | Decimal encoding of an 'Int32'.
{-# INLINE int32Dec #-}
int32Dec :: BoundedEncoding Int32
int32Dec = encodeIntDecimal 11

-- | Decimal encoding of an 'Int64'.
{-# INLINE int64Dec #-}
int64Dec :: BoundedEncoding Int64
int64Dec = boundedEncoding 20 $ c_long_long_int_dec . fromIntegral

-- | Decimal encoding of an 'Int'.
{-# INLINE intDec #-}
intDec :: BoundedEncoding Int
intDec = caseWordSize_32_64
    (fromIntegral >$< int32Dec)
    (fromIntegral >$< int64Dec)


-- Unsigned integers
--------------------

foreign import ccall unsafe "static uint_dec" c_uint_dec
    :: CUInt -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "static long_long_uint_dec" c_long_long_uint_dec
    :: CULLong -> Ptr Word8 -> IO (Ptr Word8)

{-# INLINE encodeWordDecimal #-}
encodeWordDecimal :: Integral a => Int -> BoundedEncoding a
encodeWordDecimal bound = boundedEncoding bound $ c_uint_dec . fromIntegral

-- | Decimal encoding of a 'Word8'.
{-# INLINE word8Dec #-}
word8Dec :: BoundedEncoding Word8
word8Dec = encodeWordDecimal 3

-- | Decimal encoding of a 'Word16'.
{-# INLINE word16Dec #-}
word16Dec :: BoundedEncoding Word16
word16Dec = encodeWordDecimal 5

-- | Decimal encoding of a 'Word32'.
{-# INLINE word32Dec #-}
word32Dec :: BoundedEncoding Word32
word32Dec = encodeWordDecimal 10

-- | Decimal encoding of a 'Word64'.
{-# INLINE word64Dec #-}
word64Dec :: BoundedEncoding Word64
word64Dec = boundedEncoding 20 $ c_long_long_uint_dec . fromIntegral

-- | Decimal encoding of a 'Word'.
{-# INLINE wordDec #-}
wordDec :: BoundedEncoding Word
wordDec = caseWordSize_32_64 
    (fromIntegral >$< word32Dec)
    (fromIntegral >$< word64Dec)

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
encodeWordHex :: forall a. (Storable a, Integral a) => BoundedEncoding a
encodeWordHex = 
    boundedEncoding (2 * sizeOf (undefined :: a)) $ c_uint_hex  . fromIntegral

{-# INLINE word8Hex #-}
word8Hex :: BoundedEncoding Word8
word8Hex = encodeWordHex

{-# INLINE word16Hex #-}
word16Hex :: BoundedEncoding Word16
word16Hex = encodeWordHex

{-# INLINE word32Hex #-}
word32Hex :: BoundedEncoding Word32
word32Hex = encodeWordHex

{-# INLINE word64Hex #-}
word64Hex :: BoundedEncoding Word64
word64Hex = boundedEncoding 16 $ c_long_long_uint_hex . fromIntegral

{-# INLINE wordHex #-}
wordHex :: BoundedEncoding Word
wordHex = caseWordSize_32_64
    (fromIntegral >$< word32Hex)
    (fromIntegral >$< word64Hex)


-- fixed width; leading zeroes
------------------------------

-- | Encode a 'Word8' using 2 nibbles (hexadecimal digits).
{-# INLINE word8HexFixed #-}
word8HexFixed :: FixedEncoding Word8
word8HexFixed = fixedEncoding 2 $ 
    \x op -> poke (castPtr op) =<< encode8_as_16h lowerTable x

-- | Encode a 'Word16' using 4 nibbles.
{-# INLINE word16HexFixed #-}
word16HexFixed :: FixedEncoding Word16
word16HexFixed = 
    (\x -> (fromIntegral $ x `shiftr_w16` 8, fromIntegral x))
      >$< pairF word8HexFixed word8HexFixed

-- | Encode a 'Word32' using 8 nibbles.
{-# INLINE word32HexFixed #-}
word32HexFixed :: FixedEncoding Word32
word32HexFixed = 
    (\x -> (fromIntegral $ x `shiftr_w32` 16, fromIntegral x))
      >$< pairF word16HexFixed word16HexFixed
-- | Encode a 'Word64' using 16 nibbles.
{-# INLINE word64HexFixed #-}
word64HexFixed :: FixedEncoding Word64
word64HexFixed = 
    (\x -> (fromIntegral $ x `shiftr_w64` 32, fromIntegral x)) 
      >$< pairF word32HexFixed word32HexFixed

-- | Encode a 'Int8' using 2 nibbles (hexadecimal digits).
{-# INLINE int8HexFixed #-}
int8HexFixed :: FixedEncoding Int8
int8HexFixed = fromIntegral >$< word8HexFixed

-- | Encode a 'Int16' using 4 nibbles.
{-# INLINE int16HexFixed #-}
int16HexFixed :: FixedEncoding Int16
int16HexFixed = fromIntegral >$< word16HexFixed

-- | Encode a 'Int32' using 8 nibbles.
{-# INLINE int32HexFixed #-}
int32HexFixed :: FixedEncoding Int32
int32HexFixed = fromIntegral >$< word32HexFixed

-- | Encode a 'Int64' using 16 nibbles.
{-# INLINE int64HexFixed #-}
int64HexFixed :: FixedEncoding Int64
int64HexFixed = fromIntegral >$< word64HexFixed

-- | Encode an IEEE 'Float' using 8 nibbles.
{-# INLINE floatHexFixed #-}
floatHexFixed :: FixedEncoding Float
floatHexFixed = coerceFloatToWord32 >$< word32HexFixed

-- | Encode an IEEE 'Double' using 16 nibbles.
{-# INLINE doubleHexFixed #-}
doubleHexFixed :: FixedEncoding Double
doubleHexFixed = coerceDoubleToWord64 >$< word64HexFixed


-- Padded Encodings
-------------------

{-# INLINE appsUntilZero #-}
appsUntilZero :: Num a => (a -> a) -> a -> Int
appsUntilZero f x0 = 
    count 0 x0
  where
    count !n 0 = n
    count !n x = count (succ n) (f x)
        

{-# INLINE genHexFixedBound #-}
genHexFixedBound :: (Num a, Bits a, Integral a) 
                 => (a -> Int -> a) -> Char -> a -> FixedEncoding a
genHexFixedBound shiftr padding0 bound = 
    fixedEncoding n0 io
  where
    n0 = appsUntilZero (`shiftr` 4) bound

    padding = fromIntegral (ord padding0) :: Word8

    io !x0 !op0 = 
        loop (op0 `plusPtr` n0) x0
      where
        loop !op !x = do
           let !op' = op `plusPtr` (-1)
           poke op' =<< encode4_as_8 lowerTable (fromIntegral $ x .&. 0xf)
           let !x' = x `shiftr` 4
           unless (op' <= op0) $
             if x' == 0
               then pad (op' `plusPtr` (-1))
               else loop op' x'

        pad !op
          | op < op0  = return ()
          | otherwise = poke op padding >> pad (op `plusPtr` (-1))


{-# INLINE wordHexFixedBound #-}
wordHexFixedBound :: Char -> Word -> FixedEncoding Word
wordHexFixedBound = genHexFixedBound shiftr_w

{-# INLINE word64HexFixedBound #-}
word64HexFixedBound :: Char -> Word64 -> FixedEncoding Word64
word64HexFixedBound = genHexFixedBound shiftr_w64
{- TODO: Cleanup
word64HexFixedBound padding bound
#if WORD_SIZE_IN_BITS < 64
  | bound <= fromIntegral (maxBound :: Word) =
      fromIntegral >$< wordHexFixedBound padding (fromIntegral bound)
  | otherwise = genHexFixedBound shiftr_w64 padding bound
#else
    = genHexFixedBound shiftr_w64 padding bound
#endif
-}


-- | Note: Works only for positive numbers.
{-# INLINE genDecFixedBound #-}
genDecFixedBound :: (Num a, Bits a, Integral a) 
                 => Char -> a -> FixedEncoding a
genDecFixedBound padding0 bound = 
    fixedEncoding n0 io
  where
    n0 = appsUntilZero (`div` 10) bound

    padding = fromIntegral (ord padding0) :: Word8

    io !x0 !op0 = 
        loop (op0 `plusPtr` n0) x0
      where
        loop !op !x = do
           let !op' = op `plusPtr` (-1)
               !x'  = x `div` 10
           poke op' ((fromIntegral $ (x - x' * 10) + 48) :: Word8)
           unless (op' <= op0) $
             if x' == 0
               then pad (op' `plusPtr` (-1))
               else loop op' x'

        pad !op
          | op < op0  = return ()
          | otherwise = poke op padding >> pad (op `plusPtr` (-1))


{-# INLINE wordDecFixedBound #-}
wordDecFixedBound :: Char -> Word -> FixedEncoding Word
wordDecFixedBound = genDecFixedBound 

{-# INLINE word64DecFixedBound #-}
word64DecFixedBound :: Char -> Word64 -> FixedEncoding Word64
word64DecFixedBound = genDecFixedBound 
{- TODO: Cleanup
word64DecFixedBound padding bound
#if WORD_SIZE_IN_BITS < 64
  | bound <= fromIntegral (maxBound :: Word) =
      fromIntegral >$< wordDecFixedBound padding (fromIntegral bound)
  | otherwise = genDecFixedBound padding bound
#else
    = genDecFixedBound padding bound
#endif
-}


