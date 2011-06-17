{-# LANGUAGE CPP, MonoPatBinds #-}
-- | Copyright: (c) 2010 Simon Meier
-- License    : BSD3-style (see LICENSE)
-- 
-- Maintainer : Simon Meier <iridcode@gmail.com>
-- Stability  : experimental
-- Portability: tested on GHC only
--
-- Constructing 'Builder's by encoding signed integers using big-endian,
-- little-endian, and host-endian encodings.
--
module Data.ByteString.Builder.Int
    ( 
      int8

    -- * Big-endian encodings
    , int16BE 
    , int32BE 
    , int64BE 

    -- * Little-endian encodings
    , int16LE
    , int32LE
    , int64LE

    -- * Host-endian encodings
    , intHost  
    , int16Host
    , int32Host
    , int64Host

    ) where

import Data.ByteString.Builder.Internal
import Data.ByteString.Builder.Write
import qualified System.IO.Write as W
import Foreign

-- | Encode a single unsigned byte as-is.
--
{-# INLINE int8 #-}
int8 :: Int8 -> Builder
int8 = fromWrite W.int8

-- | Encode a 'Int16' in big endian format.
{-# INLINE int16BE #-}
int16BE :: Int16 -> Builder
int16BE = fromWrite W.int16BE

-- | Encode a 'Int16' in little endian format.
{-# INLINE int16LE #-}
int16LE :: Int16 -> Builder
int16LE = fromWrite W.int16LE

-- | Encode a 'Int32' in big endian format.
{-# INLINE int32BE #-}
int32BE :: Int32 -> Builder
int32BE = fromWrite W.int32BE

-- | Encode a 'Int32' in little endian format.
{-# INLINE int32LE #-}
int32LE :: Int32 -> Builder
int32LE = fromWrite W.int32LE

-- | Encode a 'Int64' in big endian format.
{-# INLINE int64BE #-}
int64BE :: Int64 -> Builder
int64BE = fromWrite W.int64BE

-- | Encode a 'Int64' in little endian format.
{-# INLINE int64LE #-}
int64LE :: Int64 -> Builder
int64LE = fromWrite W.int64LE

-- | Encode a single native machine 'Int'. The 'Int' is encoded in host order,
-- host endian form, for the machine you're on. On a 64 bit machine the 'Int'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values encoded this way
-- are not portable to different endian or int sized machines, without
-- conversion.
--
{-# INLINE intHost #-}
intHost :: Int -> Builder
intHost = fromWrite W.intHost

-- | Encode a 'Int16' in native host order and host endianness.
{-# INLINE int16Host #-}
int16Host :: Int16 -> Builder
int16Host = fromWrite W.int16Host

-- | Encode a 'Int32' in native host order and host endianness.
{-# INLINE int32Host #-}
int32Host :: Int32 -> Builder
int32Host = fromWrite W.int32Host

-- | Encode a 'Int64' in native host order and host endianness.
{-# INLINE int64Host #-}
int64Host :: Int64 -> Builder
int64Host = fromWrite W.int64Host


