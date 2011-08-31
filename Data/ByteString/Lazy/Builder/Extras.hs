{-# LANGUAGE BangPatterns, MonoPatBinds #-}
-----------------------------------------------------------------------------
-- | Copyright : (c) 2010      Jasper Van der Jeugt 
--               (c) 2010-2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--   
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
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
    , L.smallChunkSize
    , L.defaultChunkSize

    -- * Controlling chunk boundaries
    , byteStringCopy
    , byteStringInsert
    , byteStringThreshold

    , lazyByteStringCopy
    , lazyByteStringInsert
    , lazyByteStringThreshold

    , flush

    -- * Host-specific binary encodings
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

    -- * ASCII encoding
    -- | #ASCII#The /Char8/ encoding encodes each 'Char' as its Unicode codepoint
    -- modulo 256. For codepoints below 128, the Char8 encoding is equal to the
    -- ASCII encoding. It is useful for implementing binary protocols that use
    -- ASCII or Latin1 characters. Use the functions in
    -- "Data.ByteString.Lazy.Builder.Utf8" for efficiently encoding primitive
    -- Haskell values.
    , char8
    , string8
   
    ) where


import Data.ByteString.Lazy.Builder.Internal

import qualified Data.ByteString.Lazy.Builder.BoundedEncoding as E

import qualified Data.ByteString               as S
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy.Internal as L

import Foreign
import Data.Monoid



------------------------------------------------------------------------------
-- Char8 encoding
------------------------------------------------------------------------------

-- | Encode a 'Char' using the Char8 encoding.
char8 :: Char -> Builder
char8 = E.encodeWithF E.char8

-- | Encode a 'String' using the Char8 encoding.
string8 :: String -> Builder
string8 = E.encodeListWithF E.char8


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

