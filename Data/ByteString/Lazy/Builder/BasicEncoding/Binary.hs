{-# LANGUAGE CPP, BangPatterns, MonoPatBinds #-}
-- |
-- Module      : Data.ByteString.Lazy.Builder.BasicEncoding
-- Copyright   : (c) 2010-2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
module Data.ByteString.Lazy.Builder.BasicEncoding.Binary (

  -- ** Binary encoding
    int8
  , word8

  -- *** Big-endian
  , int16BE
  , int32BE
  , int64BE

  , word16BE
  , word32BE
  , word64BE

  , floatBE
  , doubleBE

  -- *** Little-endian
  , int16LE
  , int32LE
  , int64LE

  , word16LE
  , word32LE
  , word64LE

  , floatLE
  , doubleLE

  -- *** Base 128 Variable-length
  , int8Var
  , int16Var
  , int32Var
  , int64Var
  , intVar

  , word8Var
  , word16Var
  , word32Var
  , word64Var
  , wordVar

  , int8VarSigned
  , int16VarSigned
  , int32VarSigned
  , int64VarSigned
  , intVarSigned

  , wordVarFixed
  , word64VarFixed

  -- *** Non-portable, host-dependent
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

  ) where

import Data.ByteString.Lazy.Builder.BasicEncoding.Internal
import Data.ByteString.Lazy.Builder.BasicEncoding.Internal.UncheckedShifts
import Data.ByteString.Lazy.Builder.BasicEncoding.Internal.Floating

import Foreign

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
floatBE = coerceFloatToWord32 >$< word32BE 

-- | Encode a 'Float' in little endian format.
{-# INLINE floatLE #-}
floatLE :: FixedEncoding Float
floatLE = coerceFloatToWord32 >$< word32LE

-- | Encode a 'Double' in big endian format.
{-# INLINE doubleBE #-}
doubleBE :: FixedEncoding Double
doubleBE = coerceDoubleToWord64 >$< word64BE 

-- | Encode a 'Double' in little endian format.
{-# INLINE doubleLE #-}
doubleLE :: FixedEncoding Double
doubleLE = coerceDoubleToWord64 >$< word64LE 


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
-- Base-128 Variable-Length Encodings
------------------------------------------------------------------------------

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

{-# INLINE word8Var #-}
word8Var :: BoundedEncoding Word8
word8Var = encodeBase128 shiftr_w

{-# INLINE word16Var #-}
word16Var :: BoundedEncoding Word16
word16Var = encodeBase128 shiftr_w

{-# INLINE word32Var #-}
word32Var :: BoundedEncoding Word32
word32Var = encodeBase128 shiftr_w32

{-# INLINE word64Var #-}
word64Var :: BoundedEncoding Word64
word64Var = encodeBase128 shiftr_w64

{-# INLINE wordVar #-}
wordVar :: BoundedEncoding Word
wordVar = encodeBase128 shiftr_w


{-# INLINE int8Var #-}
int8Var :: BoundedEncoding Int8
int8Var = fromIntegral >$< word8Var

{-# INLINE int16Var #-}
int16Var :: BoundedEncoding Int16
int16Var = fromIntegral >$< word16Var

{-# INLINE int32Var #-}
int32Var :: BoundedEncoding Int32
int32Var = fromIntegral >$< word32Var

{-# INLINE int64Var #-}
int64Var :: BoundedEncoding Int64
int64Var = fromIntegral >$< word64Var

{-# INLINE intVar #-}
intVar :: BoundedEncoding Int
intVar = fromIntegral >$< wordVar

{-# INLINE zigZag #-}
zigZag :: (Storable a, Bits a) => a -> a
zigZag x = (x `shiftL` 1) `xor` (x `shiftR` (8 * sizeOf x - 1))

{-# INLINE int8VarSigned #-}
int8VarSigned :: BoundedEncoding Int8
int8VarSigned = zigZag >$< int8Var

{-# INLINE int16VarSigned #-}
int16VarSigned :: BoundedEncoding Int16
int16VarSigned = zigZag >$< int16Var

{-# INLINE int32VarSigned #-}
int32VarSigned :: BoundedEncoding Int32
int32VarSigned = zigZag >$< int32Var

{-# INLINE int64VarSigned #-}
int64VarSigned :: BoundedEncoding Int64
int64VarSigned = zigZag >$< int64Var

{-# INLINE intVarSigned #-}
intVarSigned :: BoundedEncoding Int
intVarSigned = zigZag >$< intVar


-- Padded versions for streamed prefixing of output sizes
---------------------------------------------------------

{-# INLINE appsUntilZero #-}
appsUntilZero :: Num a => (a -> a) -> a -> Int
appsUntilZero f x0 = 
    count 0 x0
  where
    count !n 0 = n
    count !n x = count (succ n) (f x)
        

{-# INLINE genericVarFixed #-}
genericVarFixed :: (Bits b, Num a, Integral b) 
                => (b -> a -> b) -> b -> FixedEncoding b
genericVarFixed shiftRight bound = 
    fixedEncoding n0 io
  where
    n0 = max 1 $ appsUntilZero (`shiftRight` 7) bound

    io !x0 !op 
      | x0 > bound = error err
      | otherwise  = loop 0 x0
      where
        err = "genericVarFixed: value " ++ show x0 ++ " > bound " ++ show bound
        loop !n !x
          | n0 <= n + 1 = do poke8 (x .&. 0x7f)
          | otherwise   = do poke8 ((x .&. 0x7f) .|. 0x80)
                             loop (n + 1) (x `shiftRight` 7)
          where
            poke8 = pokeElemOff op n . fromIntegral

{-# INLINE wordVarFixed #-}
wordVarFixed :: Word -> FixedEncoding Word
wordVarFixed = genericVarFixed shiftr_w

{-# INLINE word64VarFixed #-}
word64VarFixed :: Word64 -> FixedEncoding Word64
word64VarFixed = genericVarFixed shiftr_w64


-- Somehow this function doesn't really make sense, as the bound must be
-- greater when interpreted as an unsigned integer. These conversions and
-- decisions should be left to the user.
--
--{-# INLINE intVarFixed #-}
--intVarFixed :: Size -> FixedEncoding Size
--intVarFixed bound = fromIntegral >$< wordVarFixed (fromIntegral bound)

