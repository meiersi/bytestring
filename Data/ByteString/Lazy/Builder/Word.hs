{-# LANGUAGE CPP, MonoPatBinds #-}
-- | Copyright: (c) 2010 Simon Meier
-- License    : BSD3-style (see LICENSE)
-- 
-- Maintainer : Simon Meier <iridcode@gmail.com>
-- Stability  : experimental
-- Portability: tested on GHC only
--
-- Constructing 'Builder's by encoding unsigned integers using big-endian,
-- little-endian, and host-endian encodings.
--
module Data.ByteString.Lazy.Builder.Word
    ( 
      word8

    -- * Big-endian encodings
    , word16BE 
    , word32BE 
    , word64BE 

    -- * Little-endian encodings
    , word16LE
    , word32LE
    , word64LE

    -- * Host-endian encodings
    , wordHost  
    , word16Host
    , word32Host
    , word64Host

    ) where

import Data.ByteString.Lazy.Builder.Internal
import qualified Data.ByteString.Lazy.Builder.BoundedEncoding as E
import Foreign

-- | Encode a single unsigned byte as-is.
--
{-# INLINE word8 #-}
word8 :: Word8 -> Builder
word8 = E.encodeWith E.word8

-- | Encode a 'Word16' in big endian format.
{-# INLINE word16BE #-}
word16BE :: Word16 -> Builder
word16BE = E.encodeWith E.word16BE

-- | Encode a 'Word16' in little endian format.
{-# INLINE word16LE #-}
word16LE :: Word16 -> Builder
word16LE = E.encodeWith E.word16LE

-- | Encode a 'Word32' in big endian format.
{-# INLINE word32BE #-}
word32BE :: Word32 -> Builder
word32BE = E.encodeWith E.word32BE

-- | Encode a 'Word32' in little endian format.
{-# INLINE word32LE #-}
word32LE :: Word32 -> Builder
word32LE = E.encodeWith E.word32LE

-- | Encode a 'Word64' in big endian format.
{-# INLINE word64BE #-}
word64BE :: Word64 -> Builder
word64BE = E.encodeWith E.word64BE

-- | Encode a 'Word64' in little endian format.
{-# INLINE word64LE #-}
word64LE :: Word64 -> Builder
word64LE = E.encodeWith E.word64LE

-- | Encode a single native machine 'Word'. The 'Word' is encoded in host order,
-- host endian form, for the machine you're on. On a 64 bit machine the 'Word'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values encoded this way
-- are not portable to different endian or word sized machines, without
-- conversion.
--
{-# INLINE wordHost #-}
wordHost :: Word -> Builder
wordHost = E.encodeWith E.wordHost

-- | Encode a 'Word16' in native host order and host endianness.
{-# INLINE word16Host #-}
word16Host :: Word16 -> Builder
word16Host = E.encodeWith E.word16Host

-- | Encode a 'Word32' in native host order and host endianness.
{-# INLINE word32Host #-}
word32Host :: Word32 -> Builder
word32Host = E.encodeWith E.word32Host

-- | Encode a 'Word64' in native host order and host endianness.
{-# INLINE word64Host #-}
word64Host :: Word64 -> Builder
word64Host = E.encodeWith E.word64Host


