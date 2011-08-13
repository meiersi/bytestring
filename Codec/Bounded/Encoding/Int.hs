{-# LANGUAGE MonoPatBinds #-}
-- |
-- Copyright   : (c) 2010 Simon Meier
--
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Encoding fixed-width signed integers in big-endian,
-- little-endian, and host-dependent formats.
--
module Codec.Bounded.Encoding.Int
    ( 

    -- * Host-independent encodings
      int8

    -- ** Big-endian encodings
    , int16BE
    , int32BE
    , int64BE

    -- ** Little-endian encodings
    , int16LE
    , int32LE
    , int64LE

    -- * Host-dependent encodings
    , intHost  
    , int16Host
    , int32Host
    , int64Host

    ) where

import Codec.Bounded.Encoding.Internal (Encoding, writeStorable, comapEncoding)
import Codec.Bounded.Encoding.Word

import Foreign

------------------------------------------------------------------------------
-- Int encodings
--------------
--
-- we rely on 'fromIntegral' to do a loss-less conversion to the corresponding
-- 'Word' type
-- 
------------------------------------------------------------------------------

-- TODO: Speed up encodings for architectures where they agree with the
-- host-aligned/ordered writes.


-- | Encoding single signed bytes as-is.
--
{-# INLINE int8 #-}
int8 :: Encoding Int8
int8 = comapEncoding fromIntegral word8

-- | Encoding 'Int16's in big endian format.
{-# INLINE int16BE #-}
int16BE :: Encoding Int16
int16BE = comapEncoding fromIntegral word16BE

-- | Encoding 'Int16's in little endian format.
{-# INLINE int16LE #-}
int16LE :: Encoding Int16
int16LE = comapEncoding fromIntegral word16LE

-- | Encoding 'Int32's in big endian format.
{-# INLINE int32BE #-}
int32BE :: Encoding Int32
int32BE = comapEncoding fromIntegral word32BE

-- | Encoding 'Int32's in little endian format.
{-# INLINE int32LE #-}
int32LE :: Encoding Int32
int32LE = comapEncoding fromIntegral word32LE

-- | Encoding 'Int64's in big endian format.
{-# INLINE int64BE #-}
int64BE :: Encoding Int64
int64BE = comapEncoding fromIntegral word64BE

-- | Encoding 'Int64's in little endian format.
{-# INLINE int64LE #-}
int64LE :: Encoding Int64
int64LE = comapEncoding fromIntegral word64LE


------------------------------------------------------------------------
-- Unaligned, integer size ops

-- TODO: Ensure that they are safe on architectures where an unaligned write is
-- an error.


-- | Encode a single native machine 'Int'. The 'Int's is encoded in host order,
-- host endian form, for the machine you are on. On a 64 bit machine the 'Int'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values encoded this way
-- are not portable to different endian or integer sized machines, without
-- conversion.
--
{-# INLINE intHost #-}
intHost :: Encoding Int
intHost = writeStorable

-- | Encoding 'Int16's in native host order and host endianness.
{-# INLINE int16Host #-}
int16Host :: Encoding Int16
int16Host = writeStorable

-- | Encoding 'Int32's in native host order and host endianness.
{-# INLINE int32Host #-}
int32Host :: Encoding Int32
int32Host = writeStorable

-- | Encoding 'Int64's in native host order and host endianness.
{-# INLINE int64Host #-}
int64Host :: Encoding Int64
int64Host = writeStorable


