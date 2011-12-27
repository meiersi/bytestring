{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- | Copyright : (c) 2010      Jasper Van der Jeugt
--               (c) 2010-2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Portability : GHC
--
-- Extra functions for creating and executing 'Builder's. They are intended
-- for application-specific fine-tuning the performance of 'Builder's.
--
-----------------------------------------------------------------------------
module Data.ByteString.Lazy.Builder.Extras
    (
    -- * Execution strategies
      toLazyByteStringWith
    , AllocationStrategy
    , safeStrategy
    , untrimmedStrategy
    , smallChunkSize
    , defaultChunkSize

    -- * Controlling chunk boundaries
    , byteStringCopy
    , byteStringInsert
    , byteStringThreshold

    , lazyByteStringCopy
    , lazyByteStringInsert
    , lazyByteStringThreshold

    , flush

    -- * Additional encodings
    -- ** Host-specific binary encodings
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

    -- ** Variable-length binary encodings
    -- *** Little-endian, base-128
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
  
    -- *** Little-endian, base-128, zig-zag

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
The following encodings implement the combintion of ZigZag encoding
  together with the above variable-length, little-endian, base-128 encodings.
-}
    , int8ZigZagBase128LE
    , int16ZigZagBase128LE
    , int32ZigZagBase128LE
    , int64ZigZagBase128LE
    , intZigZagBase128LE


    , module Data.ByteString.Lazy.Builder.Transformers

    ) where


import           Data.ByteString.Lazy.Builder.Internal
import           Data.ByteString.Lazy.Builder.Transformers
import qualified Data.ByteString.Lazy.Builder.BasicEncoding as E

import           Foreign



------------------------------------------------------------------------------
-- Host-specific encodings
------------------------------------------------------------------------------

-- | Encode a single native machine 'Int'. The 'Int' is encoded in host order,
-- host endian form, for the machine you're on. On a 64 bit machine the 'Int'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values encoded this way
-- are not portable to different endian or int sized machines, without
-- conversion.
--
{-# INLINE intHost #-}
intHost :: Int -> Builder
intHost = E.encodeWithF E.intHost

-- | Encode a 'Int16' in native host order and host endianness.
{-# INLINE int16Host #-}
int16Host :: Int16 -> Builder
int16Host = E.encodeWithF E.int16Host

-- | Encode a 'Int32' in native host order and host endianness.
{-# INLINE int32Host #-}
int32Host :: Int32 -> Builder
int32Host = E.encodeWithF E.int32Host

-- | Encode a 'Int64' in native host order and host endianness.
{-# INLINE int64Host #-}
int64Host :: Int64 -> Builder
int64Host = E.encodeWithF E.int64Host

-- | Encode a single native machine 'Word'. The 'Word' is encoded in host order,
-- host endian form, for the machine you're on. On a 64 bit machine the 'Word'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values encoded this way
-- are not portable to different endian or word sized machines, without
-- conversion.
--
{-# INLINE wordHost #-}
wordHost :: Word -> Builder
wordHost = E.encodeWithF E.wordHost

-- | Encode a 'Word16' in native host order and host endianness.
{-# INLINE word16Host #-}
word16Host :: Word16 -> Builder
word16Host = E.encodeWithF E.word16Host

-- | Encode a 'Word32' in native host order and host endianness.
{-# INLINE word32Host #-}
word32Host :: Word32 -> Builder
word32Host = E.encodeWithF E.word32Host

-- | Encode a 'Word64' in native host order and host endianness.
{-# INLINE word64Host #-}
word64Host :: Word64 -> Builder
word64Host = E.encodeWithF E.word64Host

-- | Encode a 'Float' in native host order. Values encoded this way are not
-- portable to different endian machines, without conversion.
{-# INLINE floatHost #-}
floatHost :: Float -> Builder
floatHost = E.encodeWithF E.floatHost

-- | Encode a 'Double' in native host order.
{-# INLINE doubleHost #-}
doubleHost :: Double -> Builder
doubleHost = E.encodeWithF E.doubleHost

------------------------------------------------------------------------------
-- Variable-length, little-endian, base-128 encodings
------------------------------------------------------------------------------

-- | Variable-length, little-endian, base-128 encoding of a 'Word8'.
{-# INLINE word8Base128LE #-}
word8Base128LE :: Word8 -> Builder
word8Base128LE = E.encodeWithB E.word8Base128LE

-- | Variable-length, little-endian, base-128 encoding of a 'Word16'.
{-# INLINE word16Base128LE #-}
word16Base128LE :: Word16 -> Builder
word16Base128LE = E.encodeWithB E.word16Base128LE

-- | Variable-length, little-endian, base-128 encoding of a 'Word32'.
{-# INLINE word32Base128LE #-}
word32Base128LE :: Word32 -> Builder
word32Base128LE = E.encodeWithB E.word32Base128LE

-- | Variable-length, little-endian, base-128 encoding of a 'Word64'.
{-# INLINE word64Base128LE #-}
word64Base128LE :: Word64 -> Builder
word64Base128LE = E.encodeWithB E.word64Base128LE

-- | Variable-length, little-endian, base-128 encoding of a 'Word'.
--
-- Note that in contrast to the fixed-width binary encoding of a 'Word',
--   whose width depends on the register-width of a machine,
--   this encoding is /machine-independent/ for values small enough to
--   be represented using a 'Word' on all relevant machines.
{-# INLINE wordBase128LE #-}
wordBase128LE :: Word -> Builder
wordBase128LE = E.encodeWithB E.wordBase128LE


-- | Variable-length, little-endian, base-128 encoding of an 'Int8'.
-- Use 'int8ZigZagBase128LE' for encoding negative numbers.
{-# INLINE int8Base128LE #-}
int8Base128LE :: Int8 -> Builder
int8Base128LE = E.encodeWithB E.int8Base128LE

-- | Variable-length, little-endian, base-128 encoding of an 'Int16'.
-- Use 'int16ZigZagBase128LE' for encoding negative numbers.
{-# INLINE int16Base128LE #-}
int16Base128LE :: Int16 -> Builder
int16Base128LE = E.encodeWithB E.int16Base128LE

-- | Variable-length, little-endian, base-128 encoding of an 'Int32'.
-- Use 'int32ZigZagBase128LE' for encoding negative numbers.
{-# INLINE int32Base128LE #-}
int32Base128LE :: Int32 -> Builder
int32Base128LE = E.encodeWithB E.int32Base128LE

-- | Variable-length, little-endian, base-128 encoding of an 'Int64'.
-- Use 'int64ZigZagBase128LE' for encoding negative numbers.
{-# INLINE int64Base128LE #-}
int64Base128LE :: Int64 -> Builder
int64Base128LE = E.encodeWithB E.int64Base128LE

-- | Variable-length, little-endian, base-128 encoding of an 'Int'.
-- Use 'intZigZagBase128LE' for encoding negative numbers.
--
-- Note that in contrast to the fixed-width binary encoding of an 'Int',
--   whose width depends on the register-width of a machine,
--   this encoding is /machine-independent/ for values small enough to
--   be represented using an 'Int' on all relevant machines.
{-# INLINE intBase128LE #-}
intBase128LE :: Int -> Builder
intBase128LE = E.encodeWithB E.intBase128LE

-- | Variable-length, little-endian, base-128, zig-zag encoding of an 'Int8'.
{-# INLINE int8ZigZagBase128LE #-}
int8ZigZagBase128LE :: Int8 -> Builder
int8ZigZagBase128LE = E.encodeWithB E.int8Base128LE

-- | Variable-length, little-endian, base-128, zig-zag encoding of an 'Int16'.
{-# INLINE int16ZigZagBase128LE #-}
int16ZigZagBase128LE :: Int16 -> Builder
int16ZigZagBase128LE = E.encodeWithB E.int16Base128LE

-- | Variable-length, little-endian, base-128, zig-zag encoding of an 'Int32'.
{-# INLINE int32ZigZagBase128LE #-}
int32ZigZagBase128LE :: Int32 -> Builder
int32ZigZagBase128LE = E.encodeWithB E.int32Base128LE

-- | Variable-length, little-endian, base-128, zig-zag encoding of an 'Int64'.
{-# INLINE int64ZigZagBase128LE #-}
int64ZigZagBase128LE :: Int64 -> Builder
int64ZigZagBase128LE = E.encodeWithB E.int64Base128LE

-- | Variable-length, little-endian, base-128, zig-zag encoding of an 'Int'.
--
-- Note that in contrast to the fixed-width binary encoding of an 'Int',
--   whose width depends on the register-width of a machine,
--   this encoding is /machine-independent/ for values small enough to
--   be represented using an 'Int' on all relevant machines.
{-# INLINE intZigZagBase128LE #-}
intZigZagBase128LE :: Int -> Builder
intZigZagBase128LE = E.encodeWithB E.intBase128LE


