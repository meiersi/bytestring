{-# LANGUAGE ScopedTypeVariables, CPP, BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
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
  , word8Base128LE
  , word16Base128LE
  , word32Base128LE
  , word64Base128LE
  , wordBase128LE

  -- **** Little-endian, base-128, zig-zag
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
import Foreign.C.Types

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

{- Pure Haskell implementation of variable length encoder. Test its
 - performance once more, when GHC's code generator has improved. The
 - performance is especially bad for encoding large values.
 
{-# INLINE genericBase128LEPadded #-}
genericBase128LEPadded :: (Eq b, Show b, Bits b, Num a, Integral b)
                => (b -> a -> b)  -- ^ Shift function to use.
                -> b -> FixedEncoding b
genericBase128LEPadded shiftRight bound =
    fixedEncoding n0 io
  where
    n0 = max 1 $ appsUntilZero (`shiftRight` 7) bound

    io !x0 !op
      | x0 > bound = error err
      | otherwise  = loop 0 x0
      where
        err = "genericBase128LEPadded: value " ++ show x0 ++ " > bound " ++ show bound
        loop !n !x
          | n0 <= n + 1 = do poke8 (x .&. 0x7f)
          | otherwise   = do poke8 ((x .&. 0x7f) .|. 0x80)
                             loop (n + 1) (x `shiftRight` 7)
          where
            poke8 = pokeElemOff op n . fromIntegral
-}

-- | GHC's code generator is not (yet) on par with gcc. We therefore use a
-- C-based implementation.
foreign import ccall unsafe "static _hs_bytestring_word64Base128LE"
    c_word64Base128LE :: CULLong -> Ptr Word8 -> IO (Ptr Word8)

-- | Generic variable-length, little-endian, base-128 encoder for values
-- fitting into 64-bit integers.
{-# INLINE encodeBase128ViaWord64 #-}
encodeBase128ViaWord64 :: forall a. (Integral a, Storable a) => BoundedEncoding a
encodeBase128ViaWord64 =
    -- We add 6 because we require the result of (`div` 7) to be rounded up.
    boundedEncoding ((8 * sizeOf (undefined :: a) + 6) `div` 7) 
                    (c_word64Base128LE . fromIntegral)

-- | A 32 bit version to speed up computations on 32-bit machines.
foreign import ccall unsafe "static _hs_bytestring_word32Base128LE"
    c_word32Base128LE :: CUInt -> Ptr Word8 -> IO (Ptr Word8)

-- | Generic variable-length, little-endian, base-128 encoder for values
-- fitting into 32-bit integers.
{-# INLINE encodeBase128ViaWord32 #-}
encodeBase128ViaWord32 :: forall a. (Integral a, Storable a) => BoundedEncoding a
encodeBase128ViaWord32 =
    boundedEncoding ((8 * sizeOf (undefined :: a) + 6) `div` 7) 
                    (c_word32Base128LE . fromIntegral)

-- | Variable-length, little-endian, base-128 encoding of a 'Word8'.
{-# INLINE word8Base128LE #-}
word8Base128LE :: BoundedEncoding Word8
word8Base128LE = 
    ifB (< 0x80) (fromF word8) 
                 (fromF $ (\x -> (x, 0x01)) >$< word8 `pairF` word8)

-- | Variable-length, little-endian, base-128 encoding of a 'Word16'.
{-# INLINE word16Base128LE #-}
word16Base128LE :: BoundedEncoding Word16
word16Base128LE = encodeBase128ViaWord32

-- | Variable-length, little-endian, base-128 encoding of a 'Word32'.
{-# INLINE word32Base128LE #-}
word32Base128LE :: BoundedEncoding Word32
word32Base128LE = encodeBase128ViaWord32

-- | Variable-length, little-endian, base-128 encoding of a 'Word64'.
{-# INLINE word64Base128LE #-}
word64Base128LE :: BoundedEncoding Word64
word64Base128LE = encodeBase128ViaWord64

-- | Variable-length, little-endian, base-128 encoding of a 'Word'.
--
-- Note that in contrast to the fixed-width binary encoding of a 'Word',
--   whose width depends on the register-width of a machine,
--   this encoding is /machine-independent/ for values small enough to
--   be represented using a 'Word' on all relevant machines.
{-# INLINE wordBase128LE #-}
wordBase128LE :: BoundedEncoding Word
wordBase128LE = caseWordSize_32_64 
    (fromIntegral >$< word32Base128LE) 
    (fromIntegral >$< word64Base128LE)

{-# INLINE zigZag #-}
zigZag :: forall a b. (Storable a, Bits a, Integral a, Num b) => a -> b
zigZag x = (fromIntegral :: a -> b) $ 
    (x `shiftL` 1) `xor` (x `shiftR` (8 * sizeOf x - 1))

-- | Variable-length, little-endian, base-128, zig-zag encoding of an 'Int8'.
{-# INLINE int8ZigZagBase128LE #-}
int8ZigZagBase128LE :: BoundedEncoding Int8
int8ZigZagBase128LE = zigZag >$< word8Base128LE

-- | Variable-length, little-endian, base-128, zig-zag encoding of an 'Int16'.
{-# INLINE int16ZigZagBase128LE #-}
int16ZigZagBase128LE :: BoundedEncoding Int16
int16ZigZagBase128LE = zigZag >$< word16Base128LE

-- | Variable-length, little-endian, base-128, zig-zag encoding of an 'Int32'.
{-# INLINE int32ZigZagBase128LE #-}
int32ZigZagBase128LE :: BoundedEncoding Int32
int32ZigZagBase128LE = zigZag >$< word32Base128LE

-- | Variable-length, little-endian, base-128, zig-zag encoding of an 'Int64'.
{-# INLINE int64ZigZagBase128LE #-}
int64ZigZagBase128LE :: BoundedEncoding Int64
int64ZigZagBase128LE = zigZag >$< word64Base128LE

-- | Variable-length, little-endian, base-128, zig-zag encoding of an 'Int'.
--
-- Note that in contrast to the fixed-width binary encoding of an 'Int',
--   whose width depends on the register-width of a machine,
--   this encoding is /machine-independent/ for values small enough to
--   be represented using an 'Int' on all relevant machines.
{-# INLINE intZigZagBase128LE #-}
intZigZagBase128LE :: BoundedEncoding Int
intZigZagBase128LE = zigZag >$< wordBase128LE
