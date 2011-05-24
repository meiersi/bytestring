{-# LANGUAGE MonoPatBinds #-}
-- |
-- Copyright   : (c) 2010-2011 Simon Meier
--
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- 'Write's and 'Builder's for serializing integers.
--
-- See "Blaze.ByteString.Builder.Word" for information about how to best write several
-- integers at once.
--
module Data.ByteString.Builder.Int
    ( 
    -- * Creating builders from integers
    
    -- | We provide serialization functions both for singleton integers as well as
    -- for lists of integers. Using these list serialization functions is /much/ faster
    -- than using @mconcat . map fromInt/<n/>@, as the list serialization
    -- functions use a tighter inner loop.

      fromInt8
    , fromInt8s

    -- ** Big-endian serialization
    , fromInt16BE            -- :: Int16   -> Builder
    , fromInt32BE            -- :: Int32   -> Builder
    , fromInt64BE            -- :: Int64   -> Builder
    , fromInt32sbe           -- :: [Int32] -> Builder
    , fromInt16sbe           -- :: [Int16] -> Builder
    , fromInt64sbe           -- :: [Int64] -> Builder

    -- ** Little-endian serialization
    , fromInt16LE            -- :: Int16   -> Builder
    , fromInt32LE            -- :: Int32   -> Builder
    , fromInt64LE            -- :: Int64   -> Builder
    , fromInt16sle           -- :: [Int16] -> Builder
    , fromInt32sle           -- :: [Int32] -> Builder
    , fromInt64sle           -- :: [Int64] -> Builder

    -- ** Host-endian serialization
    , fromInthost            -- :: Int     -> Builder
    , fromInt16Host          -- :: Int16   -> Builder
    , fromInt32Host          -- :: Int32   -> Builder
    , fromInt64Host          -- :: Int64   -> Builder
    , fromIntshost           -- :: [Int]   -> Builder
    , fromInt16shost         -- :: [Int16] -> Builder
    , fromInt32shost         -- :: [Int32] -> Builder
    , fromInt64shost         -- :: [Int64] -> Builder

    ) where

import Data.ByteString.Builder.Internal
import Data.ByteString.Builder.Write

import System.IO.Write.Int

import Foreign

------------------------------------------------------------------------------
-- Builders corresponding to the integer writes
------------------------------------------------------------------------------

-- Single bytes
------------------------------------------------------------------------------

-- | Serialize a single byte.
--
fromInt8 :: Int8 -> Builder
fromInt8 = fromWrite int8

-- | Serialize a list of bytes.
--
fromInt8s :: [Int8] -> Builder
fromInt8s = fromWriteList int8


-- Int16
------------------------------------------------------------------------------

-- | Serialize an 'Int16' in big endian format.
fromInt16BE :: Int16 -> Builder
fromInt16BE = fromWrite int16BE 
{-# INLINE fromInt16BE #-}

-- | Serialize a list of 'Int16's in big endian format.
fromInt16sbe :: [Int16] -> Builder
fromInt16sbe = fromWriteList int16BE 
{-# INLINE fromInt16sbe #-}

-- | Serialize an 'Int16' in little endian format.
fromInt16LE :: Int16 -> Builder
fromInt16LE = fromWrite int16LE 
{-# INLINE fromInt16LE #-}

-- | Serialize a list of 'Int16's in little endian format.
fromInt16sle :: [Int16] -> Builder
fromInt16sle = fromWriteList int16LE 
{-# INLINE fromInt16sle #-}


-- Int32
-----------------------------------------------------------------------------

-- | Serialize an 'Int32' in big endian format.
fromInt32BE :: Int32 -> Builder
fromInt32BE = fromWrite int32BE 
{-# INLINE fromInt32BE #-}

-- | Serialize a list of 'Int32's in big endian format.
fromInt32sbe :: [Int32] -> Builder
fromInt32sbe = fromWriteList int32BE 
{-# INLINE fromInt32sbe #-}

-- | Serialize an 'Int32' in little endian format.
fromInt32LE :: Int32 -> Builder
fromInt32LE = fromWrite int32LE 
{-# INLINE fromInt32LE #-}

-- | Serialize a list of 'Int32's in little endian format.
fromInt32sle :: [Int32] -> Builder
fromInt32sle = fromWriteList int32LE 
{-# INLINE fromInt32sle #-}

-- | Serialize an 'Int64' in big endian format.
fromInt64BE :: Int64 -> Builder
fromInt64BE = fromWrite int64BE 
{-# INLINE fromInt64BE #-}

-- | Serialize a list of 'Int64's in big endian format.
fromInt64sbe :: [Int64] -> Builder
fromInt64sbe = fromWriteList int64BE 
{-# INLINE fromInt64sbe #-}

-- | Serialize an 'Int64' in little endian format.
fromInt64LE :: Int64 -> Builder
fromInt64LE = fromWrite int64LE 
{-# INLINE fromInt64LE #-}

-- | Serialize a list of 'Int64's in little endian format.
fromInt64sle :: [Int64] -> Builder
fromInt64sle = fromWriteList int64LE 
{-# INLINE fromInt64sle #-}


------------------------------------------------------------------------
-- Unaligned, integer size ops

-- | Serialize a single native machine 'Int'. The 'Int' is serialized in host
-- order, host endian form, for the machine you're on. On a 64 bit machine the
-- 'Int' is an 8 byte value, on a 32 bit machine, 4 bytes. Values written this
-- way are not portable to different endian or integer sized machines, without
-- conversion.
--
fromInthost :: Int -> Builder
fromInthost = fromWrite intHost 
{-# INLINE fromInthost #-}

-- | Serialize a list of 'Int's.
-- See 'fromInthost' for usage considerations.
fromIntshost :: [Int] -> Builder
fromIntshost = fromWriteList intHost 
{-# INLINE fromIntshost #-}

-- | Write an 'Int16' in native host order and host endianness.
fromInt16Host :: Int16 -> Builder
fromInt16Host = fromWrite int16Host 
{-# INLINE fromInt16Host #-}

-- | Write a list of 'Int16's in native host order and host endianness.
fromInt16shost :: [Int16] -> Builder
fromInt16shost = fromWriteList int16Host 
{-# INLINE fromInt16shost #-}

-- | Write an 'Int32' in native host order and host endianness.
fromInt32Host :: Int32 -> Builder
fromInt32Host = fromWrite int32Host 
{-# INLINE fromInt32Host #-}

-- | Write a list of 'Int32's in native host order and host endianness.
fromInt32shost :: [Int32] -> Builder
fromInt32shost = fromWriteList int32Host 
{-# INLINE fromInt32shost #-}

-- | Write an 'Int64' in native host order and host endianness.
fromInt64Host :: Int64 -> Builder
fromInt64Host = fromWrite int64Host
{-# INLINE fromInt64Host #-}

-- | Write a list of 'Int64's in native host order and host endianness.
fromInt64shost :: [Int64] -> Builder
fromInt64shost = fromWriteList int64Host
{-# INLINE fromInt64shost #-}
