{-# LANGUAGE CPP, BangPatterns #-}
-- | Copyright   : (c) 2010-2011 Simon Meier
-- License       : BSD3-style (see LICENSE)
--
-- Maintainer    : Simon Meier <iridcode@gmail.com>
-- Portability   : GHC
--
module Data.ByteString.Lazy.Builder.BasicEncoding.Binary (

  -- ** Binary encodings
    int8
  , word8

  -- *** Fixed-length

  -- **** Big-endian
  , int16BE
  , int32BE
  , int64BE

  , word16BE
  , word32BE
  , word64BE

  , floatBE
  , doubleBE

  -- **** Little-endian
  , int16LE
  , int32LE
  , int64LE

  , word16LE
  , word32LE
  , word64LE

  , floatLE
  , doubleLE

  -- **** Non-portable, host-dependent
  , intHost
  , int16Host
  , int32Host
  , int64Host

  , wordHost
  , word16Host
  , word32Host
  , word64Host

  , floatHost
  , doubleHost

  -- *** Variable-length
  -- **** Little-endian, base-128
  {- |
This variable-length encoding of integers is the one used by
Google's protocol buffer library
<http://code.google.com/apis/protocolbuffers/docs/encoding.html#varints>. This
encoding can be implemented efficiently and it allows small positive integers
to be encoded using short sequences of bytes. It works as follows.

The most-significant bit (MSB) of each output byte indicates whether
there is a following byte (MSB set to 1) or it is the last byte (MSB set to 0).
The remaining 7-bits are used to encode the input starting with the least
significant 7-bit group of the input, i.e., a little-endian ordering of the
7-bit groups is used. The encoding uses always the fewest number of bytes
possible. 

This encoding is very space efficient.
It uses @ceiling (n / 7)@ bytes for encoding an integer whose highest, set bit
is at the @n@-th position.
For example, the value @1 :: Int32@, is encoded as @[0x01]@.
The variable length encoding therefore saves @3@ bytes compared to the
standard, fixed-width encoding.
The value @128 :: Int32@, whose binary representation is @1000 0000@, is
encoded as @[0x80, 0x01]@; i.e., the first byte has its MSB set and the least
significant 7-bit group is @000 0000@, the second byte has its MSB not set (it
is the last byte) and its 7-bit group is @000 0001@.
-}
  , word8Base128LE
  , word16Base128LE
  , word32Base128LE
  , word64Base128LE
  , wordBase128LE

{- |
The following encodings work by casting the signed integer 
  to the equally sized unsigned integer.
This works well for positive integers, but for negative integers 
  it always results in the /longest/ possible sequence of bytes,
  as their MSB is (by definition) always set.
Use the so-called ZigZag encoding explained below for compactly encoding both
  negative and positive integers of small magnitude.
-}

  , int8Base128LE
  , int16Base128LE
  , int32Base128LE
  , int64Base128LE
  , intBase128LE
  
  -- **** Little-endian, base-128, zig-zag

{- |
Positive and negative integers of small magnitude can be encoded compactly
  using the so-called ZigZag encoding
  (<http://code.google.com/apis/protocolbuffers/docs/encoding.html#types>).
The /ZigZag encoding/ uses
  even numbers to encode the postive integers and
  odd numbers to encode the negative integers.
For example,
  @0@ is encoded as @0@, @-1@ as @1@, @1@ as @2@, @-2@ as @3@, @2@ as @4@, and
  so on.
Its efficient implementation uses some bit-level magic; i.e.,

@
zigZag32 :: 'Int32' -> 'Word32'
zigZag32 n = fromIntegral ((n \`shiftL\` 1) \`xor\` (n \`shiftR\` 31))
@

Note that the 'shiftR' is an arithmetic shift that performs sign extension.
The ZigZag encoding essentially swaps the LSB with the MSB and additionally
inverts all bits if the MSB is set.

The following encodings implement the combintion of ZigZag encoding
  together with the above variable-length, little-endian, base-128 encodings.
-}
  , int8ZigZagBase128LE
  , int16ZigZagBase128LE
  , int32ZigZagBase128LE
  , int64ZigZagBase128LE
  , intZigZagBase128LE

  ) where

import Data.ByteString.Lazy.Builder.BasicEncoding.Internal
import Data.ByteString.Lazy.Builder.BasicEncoding.Internal.UncheckedShifts
import Data.ByteString.Lazy.Builder.BasicEncoding.Internal.Floating

import Foreign

#include "MachDeps.h"

------------------------------------------------------------------------------
-- Binary encoding
------------------------------------------------------------------------------

-- Word encodings
-----------------

-- | Encoding single unsigned bytes as-is.
--
{-# INLINE word8 #-}
word8 :: FixedEncoding Word8
word8 = storableToF

--
-- We rely on the fromIntegral to do the right masking for us.
-- The inlining here is critical, and can be worth 4x performance
--

-- | Encoding 'Word16's in big endian format.
{-# INLINE word16BE #-}
word16BE :: FixedEncoding Word16
#ifdef WORD_BIGENDIAN
word16BE = word16Host
#else
word16BE = fixedEncoding 2 $ \w p -> do
    poke p               (fromIntegral (shiftr_w16 w 8) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (w)              :: Word8)
#endif

-- | Encoding 'Word16's in little endian format.
{-# INLINE word16LE #-}
word16LE :: FixedEncoding Word16
#ifdef WORD_BIGENDIAN
word16LE = fixedEncoding 2 $ \w p -> do
    poke p               (fromIntegral (w)              :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w16 w 8) :: Word8)
#else
word16LE = word16Host
#endif

-- | Encoding 'Word32's in big endian format.
{-# INLINE word32BE #-}
word32BE :: FixedEncoding Word32
#ifdef WORD_BIGENDIAN
word32BE = word32Host
#else
word32BE = fixedEncoding 4 $ \w p -> do
    poke p               (fromIntegral (shiftr_w32 w 24) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (w)               :: Word8)
#endif

-- | Encoding 'Word32's in little endian format.
{-# INLINE word32LE #-}
word32LE :: FixedEncoding Word32
#ifdef WORD_BIGENDIAN
word32LE = fixedEncoding 4 $ \w p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w32 w 24) :: Word8)
#else
word32LE = word32Host
#endif

-- on a little endian machine:
-- word32LE w32 = fixedEncoding 4 (\w p -> poke (castPtr p) w32)

-- | Encoding 'Word64's in big endian format.
{-# INLINE word64BE #-}
word64BE :: FixedEncoding Word64
#ifdef WORD_BIGENDIAN
word64BE = word64Host
#else
#if WORD_SIZE_IN_BITS < 64
--
-- To avoid expensive 64 bit shifts on 32 bit machines, we cast to
-- Word32, and write that
--
word64BE =
    fixedEncoding 8 $ \w p -> do
        let a = fromIntegral (shiftr_w64 w 32) :: Word32
            b = fromIntegral w                 :: Word32
        poke p               (fromIntegral (shiftr_w32 a 24) :: Word8)
        poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 a 16) :: Word8)
        poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 a  8) :: Word8)
        poke (p `plusPtr` 3) (fromIntegral (a)               :: Word8)
        poke (p `plusPtr` 4) (fromIntegral (shiftr_w32 b 24) :: Word8)
        poke (p `plusPtr` 5) (fromIntegral (shiftr_w32 b 16) :: Word8)
        poke (p `plusPtr` 6) (fromIntegral (shiftr_w32 b  8) :: Word8)
        poke (p `plusPtr` 7) (fromIntegral (b)               :: Word8)
#else
word64BE = fixedEncoding 8 $ \w p -> do
    poke p               (fromIntegral (shiftr_w64 w 56) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (w)               :: Word8)
#endif
#endif

-- | Encoding 'Word64's in little endian format.
{-# INLINE word64LE #-}
word64LE :: FixedEncoding Word64
#ifdef WORD_BIGENDIAN
#if WORD_SIZE_IN_BITS < 64
word64LE =
    fixedEncoding 8 $ \w p -> do
        let b = fromIntegral (shiftr_w64 w 32) :: Word32
            a = fromIntegral w                 :: Word32
        poke (p)             (fromIntegral (a)               :: Word8)
        poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 a  8) :: Word8)
        poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 a 16) :: Word8)
        poke (p `plusPtr` 3) (fromIntegral (shiftr_w32 a 24) :: Word8)
        poke (p `plusPtr` 4) (fromIntegral (b)               :: Word8)
        poke (p `plusPtr` 5) (fromIntegral (shiftr_w32 b  8) :: Word8)
        poke (p `plusPtr` 6) (fromIntegral (shiftr_w32 b 16) :: Word8)
        poke (p `plusPtr` 7) (fromIntegral (shiftr_w32 b 24) :: Word8)
#else
word64LE = fixedEncoding 8 $ \w p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (shiftr_w64 w 56) :: Word8)
#endif
#else
word64LE = word64Host
#endif


-- | Encode a single native machine 'Word'. The 'Word's is encoded in host order,
-- host endian form, for the machine you are on. On a 64 bit machine the 'Word'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values encoded this way
-- are not portable to different endian or word sized machines, without
-- conversion.
--
{-# INLINE wordHost #-}
wordHost :: FixedEncoding Word
wordHost = storableToF

-- | Encoding 'Word16's in native host order and host endianness.
{-# INLINE word16Host #-}
word16Host :: FixedEncoding Word16
word16Host = storableToF

-- | Encoding 'Word32's in native host order and host endianness.
{-# INLINE word32Host #-}
word32Host :: FixedEncoding Word32
word32Host = storableToF

-- | Encoding 'Word64's in native host order and host endianness.
{-# INLINE word64Host #-}
word64Host :: FixedEncoding Word64
word64Host = storableToF


------------------------------------------------------------------------------
-- Int encodings
------------------------------------------------------------------------------
--
-- We rely on 'fromIntegral' to do a loss-less conversion to the corresponding
-- 'Word' type
--
------------------------------------------------------------------------------

-- | Encoding single signed bytes as-is.
--
{-# INLINE int8 #-}
int8 :: FixedEncoding Int8
int8 = fromIntegral >$< word8

-- | Encoding 'Int16's in big endian format.
{-# INLINE int16BE #-}
int16BE :: FixedEncoding Int16
int16BE = fromIntegral >$< word16BE

-- | Encoding 'Int16's in little endian format.
{-# INLINE int16LE #-}
int16LE :: FixedEncoding Int16
int16LE = fromIntegral >$< word16LE

-- | Encoding 'Int32's in big endian format.
{-# INLINE int32BE #-}
int32BE :: FixedEncoding Int32
int32BE = fromIntegral >$< word32BE

-- | Encoding 'Int32's in little endian format.
{-# INLINE int32LE #-}
int32LE :: FixedEncoding Int32
int32LE = fromIntegral >$< word32LE

-- | Encoding 'Int64's in big endian format.
{-# INLINE int64BE #-}
int64BE :: FixedEncoding Int64
int64BE = fromIntegral >$< word64BE

-- | Encoding 'Int64's in little endian format.
{-# INLINE int64LE #-}
int64LE :: FixedEncoding Int64
int64LE = fromIntegral >$< word64LE


-- TODO: Ensure that they are safe on architectures where an unaligned write is
-- an error.

-- | Encode a single native machine 'Int'. The 'Int's is encoded in host order,
-- host endian form, for the machine you are on. On a 64 bit machine the 'Int'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values encoded this way
-- are not portable to different endian or integer sized machines, without
-- conversion.
--
{-# INLINE intHost #-}
intHost :: FixedEncoding Int
intHost = storableToF

-- | Encoding 'Int16's in native host order and host endianness.
{-# INLINE int16Host #-}
int16Host :: FixedEncoding Int16
int16Host = storableToF

-- | Encoding 'Int32's in native host order and host endianness.
{-# INLINE int32Host #-}
int32Host :: FixedEncoding Int32
int32Host = storableToF

-- | Encoding 'Int64's in native host order and host endianness.
{-# INLINE int64Host #-}
int64Host :: FixedEncoding Int64
int64Host = storableToF

-- IEEE Floating Point Numbers
------------------------------

-- | Encode a 'Float' in big endian format.
{-# INLINE floatBE #-}
floatBE :: FixedEncoding Float
floatBE = encodeFloatViaWord32F word32BE

-- | Encode a 'Float' in little endian format.
{-# INLINE floatLE #-}
floatLE :: FixedEncoding Float
floatLE = encodeFloatViaWord32F word32LE

-- | Encode a 'Double' in big endian format.
{-# INLINE doubleBE #-}
doubleBE :: FixedEncoding Double
doubleBE = encodeDoubleViaWord64F word64BE

-- | Encode a 'Double' in little endian format.
{-# INLINE doubleLE #-}
doubleLE :: FixedEncoding Double
doubleLE = encodeDoubleViaWord64F word64LE


-- | Encode a 'Float' in native host order and host endianness. Values written
-- this way are not portable to different endian machines, without conversion.
--
{-# INLINE floatHost #-}
floatHost :: FixedEncoding Float
floatHost = storableToF

-- | Encode a 'Double' in native host order and host endianness.
{-# INLINE doubleHost #-}
doubleHost :: FixedEncoding Double
doubleHost = storableToF


------------------------------------------------------------------------------
-- Variable-Length, Little-endian, Base-128 Encodings
------------------------------------------------------------------------------

-- | Generic variable-length, little-endian, base-128 encoder.
{-# INLINE encodeBase128 #-}
encodeBase128
    :: forall a b. (Integral a, Bits a, Storable b, Integral b, Num b)
    => (a -> Int -> a) -> BoundedEncoding b
encodeBase128 shiftr =
    -- We add 6 because we require the result of (`div` 7) to be rounded up.
    boundedEncoding ((8 * sizeOf (undefined :: b) + 6) `div` 7) (io . fromIntegral)
  where
    io !x !op
      | x' == 0   = do poke8 (x .&. 0x7f)
                       return $! op `plusPtr` 1
      | otherwise = do poke8 ((x .&. 0x7f) .|. 0x80)
                       io x' (op `plusPtr` 1)
      where
        x'    = x `shiftr` 7
        poke8 = poke op . fromIntegral

-- | Variable-length, little-endian, base-128 encoding of a 'Word8'.
{-# INLINE word8Base128LE #-}
word8Base128LE :: BoundedEncoding Word8
word8Base128LE = encodeBase128 shiftr_w

-- | Variable-length, little-endian, base-128 encoding of a 'Word16'.
{-# INLINE word16Base128LE #-}
word16Base128LE :: BoundedEncoding Word16
word16Base128LE = encodeBase128 shiftr_w

-- | Variable-length, little-endian, base-128 encoding of a 'Word32'.
{-# INLINE word32Base128LE #-}
word32Base128LE :: BoundedEncoding Word32
word32Base128LE = encodeBase128 shiftr_w32

-- | Variable-length, little-endian, base-128 encoding of a 'Word64'.
{-# INLINE word64Base128LE #-}
word64Base128LE :: BoundedEncoding Word64
word64Base128LE = encodeBase128 shiftr_w64

-- | Variable-length, little-endian, base-128 encoding of a 'Word'.
--
-- Note that in contrast to the fixed-width binary encoding of a 'Word',
--   whose width depends on the register-width of a machine,
--   this encoding is /machine-independent/ for values small enough to
--   be represented using a 'Word' on all relevant machines.
{-# INLINE wordBase128LE #-}
wordBase128LE :: BoundedEncoding Word
wordBase128LE = encodeBase128 shiftr_w


-- | Variable-length, little-endian, base-128 encoding of an 'Int8'.
-- Use 'int8ZigZagBase128LE' for encoding negative numbers.
{-# INLINE int8Base128LE #-}
int8Base128LE :: BoundedEncoding Int8
int8Base128LE = fromIntegral >$< word8Base128LE

-- | Variable-length, little-endian, base-128 encoding of an 'Int16'.
-- Use 'int16ZigZagBase128LE' for encoding negative numbers.
{-# INLINE int16Base128LE #-}
int16Base128LE :: BoundedEncoding Int16
int16Base128LE = fromIntegral >$< word16Base128LE

-- | Variable-length, little-endian, base-128 encoding of an 'Int32'.
-- Use 'int32ZigZagBase128LE' for encoding negative numbers.
{-# INLINE int32Base128LE #-}
int32Base128LE :: BoundedEncoding Int32
int32Base128LE = fromIntegral >$< word32Base128LE

-- | Variable-length, little-endian, base-128 encoding of an 'Int64'.
-- Use 'int64ZigZagBase128LE' for encoding negative numbers.
{-# INLINE int64Base128LE #-}
int64Base128LE :: BoundedEncoding Int64
int64Base128LE = fromIntegral >$< word64Base128LE

-- | Variable-length, little-endian, base-128 encoding of an 'Int'.
-- Use 'intZigZagBase128LE' for encoding negative numbers.
--
-- Note that in contrast to the fixed-width binary encoding of an 'Int',
--   whose width depends on the register-width of a machine,
--   this encoding is /machine-independent/ for values small enough to
--   be represented using an 'Int' on all relevant machines.
{-# INLINE intBase128LE #-}
intBase128LE :: BoundedEncoding Int
intBase128LE = fromIntegral >$< wordBase128LE

{-# INLINE zigZag #-}
zigZag :: (Storable a, Bits a) => a -> a
zigZag x = (x `shiftL` 1) `xor` (x `shiftR` (8 * sizeOf x - 1))

-- | Variable-length, little-endian, base-128, zig-zag encoding of an 'Int8'.
{-# INLINE int8ZigZagBase128LE #-}
int8ZigZagBase128LE :: BoundedEncoding Int8
int8ZigZagBase128LE = zigZag >$< int8Base128LE

-- | Variable-length, little-endian, base-128, zig-zag encoding of an 'Int16'.
{-# INLINE int16ZigZagBase128LE #-}
int16ZigZagBase128LE :: BoundedEncoding Int16
int16ZigZagBase128LE = zigZag >$< int16Base128LE

-- | Variable-length, little-endian, base-128, zig-zag encoding of an 'Int32'.
{-# INLINE int32ZigZagBase128LE #-}
int32ZigZagBase128LE :: BoundedEncoding Int32
int32ZigZagBase128LE = zigZag >$< int32Base128LE

-- | Variable-length, little-endian, base-128, zig-zag encoding of an 'Int64'.
{-# INLINE int64ZigZagBase128LE #-}
int64ZigZagBase128LE :: BoundedEncoding Int64
int64ZigZagBase128LE = zigZag >$< int64Base128LE

-- | Variable-length, little-endian, base-128, zig-zag encoding of an 'Int'.
--
-- Note that in contrast to the fixed-width binary encoding of an 'Int',
--   whose width depends on the register-width of a machine,
--   this encoding is /machine-independent/ for values small enough to
--   be represented using an 'Int' on all relevant machines.
{-# INLINE intZigZagBase128LE #-}
intZigZagBase128LE :: BoundedEncoding Int
intZigZagBase128LE = zigZag >$< intBase128LE


