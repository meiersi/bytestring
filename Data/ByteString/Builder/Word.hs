{-# LANGUAGE CPP, MonoPatBinds #-}
-- |
-- Copyright   : (c) 2010      Jasper Van der Jeugt 
--               (c) 2010-2011 Simon Meier
--
--               Original serialization code from 'Data.Binary.Builder':
--               (c) Lennart Kolmodin, Ross Patterson
--
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif
module Data.ByteString.Builder.Word
    ( 
    -- * Creating builders from words
    
    -- | We provide serialization functions both for singleton words as well as
    -- for lists of words. Using these list serialization functions is /much/ faster
    -- than using @mconcat . map fromWord/<n/>@, as the list serialization
    -- functions use a tighter inner loop.

      fromWord8
    , fromWord8s

    -- ** Big-endian serialization
    , fromWord16BE            -- :: Word16   -> Builder
    , fromWord32BE            -- :: Word32   -> Builder
    , fromWord64BE            -- :: Word64   -> Builder
    , fromWord32sbe           -- :: [Word32] -> Builder
    , fromWord16sbe           -- :: [Word16] -> Builder
    , fromWord64sbe           -- :: [Word64] -> Builder

    -- ** Little-endian serialization
    , fromWord16LE            -- :: Word16   -> Builder
    , fromWord32LE            -- :: Word32   -> Builder
    , fromWord64LE            -- :: Word64   -> Builder
    , fromWord16sle           -- :: [Word16] -> Builder
    , fromWord32sle           -- :: [Word32] -> Builder
    , fromWord64sle           -- :: [Word64] -> Builder

    -- ** Host-endian serialization
    , fromWordHost            -- :: Word     -> Builder
    , fromWord16Host          -- :: Word16   -> Builder
    , fromWord32Host          -- :: Word32   -> Builder
    , fromWord64Host          -- :: Word64   -> Builder
    , fromWordshost           -- :: [Word]   -> Builder
    , fromWord16shost         -- :: [Word16] -> Builder
    , fromWord32shost         -- :: [Word32] -> Builder
    , fromWord64shost         -- :: [Word64] -> Builder

    ) where

import Data.ByteString.Builder.Internal
import Data.ByteString.Builder.Write

import System.IO.Write.Word

import Foreign

------------------------------------------------------------------------------
-- Builders corresponding to the word writes
------------------------------------------------------------------------------

-- Single bytes
------------------------------------------------------------------------------

-- | Serialize a single byte.
--
fromWord8 :: Word8 -> Builder
fromWord8 = fromWrite word8

-- | Serialize a list of bytes.
--
fromWord8s :: [Word8] -> Builder
fromWord8s = fromWriteList word8


-- Word16
------------------------------------------------------------------------------

-- | Serialize a 'Word16' in big endian format.
fromWord16BE :: Word16 -> Builder
fromWord16BE = fromWrite word16BE 
{-# INLINE fromWord16BE #-}

-- | Serialize a list of 'Word16's in big endian format.
fromWord16sbe :: [Word16] -> Builder
fromWord16sbe = fromWriteList word16BE 
{-# INLINE fromWord16sbe #-}

-- | Serialize a 'Word16' in little endian format.
fromWord16LE :: Word16 -> Builder
fromWord16LE = fromWrite word16LE 
{-# INLINE fromWord16LE #-}

-- | Serialize a list of 'Word16's in little endian format.
fromWord16sle :: [Word16] -> Builder
fromWord16sle = fromWriteList word16LE 
{-# INLINE fromWord16sle #-}


-- Word32
-----------------------------------------------------------------------------

-- | Serialize a 'Word32' in big endian format.
fromWord32BE :: Word32 -> Builder
fromWord32BE = fromWrite word32BE 
{-# INLINE fromWord32BE #-}

-- | Serialize a list of 'Word32's in big endian format.
fromWord32sbe :: [Word32] -> Builder
fromWord32sbe = fromWriteList word32BE 
{-# INLINE fromWord32sbe #-}

-- | Serialize a 'Word32' in little endian format.
fromWord32LE :: Word32 -> Builder
fromWord32LE = fromWrite word32LE 
{-# INLINE fromWord32LE #-}

-- | Serialize a list of 'Word32's in little endian format.
fromWord32sle :: [Word32] -> Builder
fromWord32sle = fromWriteList word32LE 
{-# INLINE fromWord32sle #-}

-- | Serialize a 'Word64' in big endian format.
fromWord64BE :: Word64 -> Builder
fromWord64BE = fromWrite word64BE 
{-# INLINE fromWord64BE #-}

-- | Serialize a list of 'Word64's in big endian format.
fromWord64sbe :: [Word64] -> Builder
fromWord64sbe = fromWriteList word64BE 
{-# INLINE fromWord64sbe #-}

-- | Serialize a 'Word64' in little endian format.
fromWord64LE :: Word64 -> Builder
fromWord64LE = fromWrite word64LE 
{-# INLINE fromWord64LE #-}

-- | Serialize a list of 'Word64's in little endian format.
fromWord64sle :: [Word64] -> Builder
fromWord64sle = fromWriteList word64LE 
{-# INLINE fromWord64sle #-}


------------------------------------------------------------------------
-- Unaligned, word size ops

-- | Serialize a single native machine 'Word'. The 'Word' is serialized in host
-- order, host endian form, for the machine you're on. On a 64 bit machine the
-- 'Word' is an 8 byte value, on a 32 bit machine, 4 bytes. Values written this
-- way are not portable to different endian or word sized machines, without
-- conversion.
--
fromWordHost :: Word -> Builder
fromWordHost = fromWrite wordHost 
{-# INLINE fromWordHost #-}

-- | Serialize a list of 'Word's.
-- See 'fromWordHost' for usage considerations.
fromWordshost :: [Word] -> Builder
fromWordshost = fromWriteList wordHost 
{-# INLINE fromWordshost #-}

-- | Write a 'Word16' in native host order and host endianness.
fromWord16Host :: Word16 -> Builder
fromWord16Host = fromWrite word16Host 
{-# INLINE fromWord16Host #-}

-- | Write a list of 'Word16's in native host order and host endianness.
fromWord16shost :: [Word16] -> Builder
fromWord16shost = fromWriteList word16Host 
{-# INLINE fromWord16shost #-}

-- | Write a 'Word32' in native host order and host endianness.
fromWord32Host :: Word32 -> Builder
fromWord32Host = fromWrite word32Host 
{-# INLINE fromWord32Host #-}

-- | Write a list of 'Word32's in native host order and host endianness.
fromWord32shost :: [Word32] -> Builder
fromWord32shost = fromWriteList word32Host 
{-# INLINE fromWord32shost #-}

-- | Write a 'Word64' in native host order and host endianness.
fromWord64Host :: Word64 -> Builder
fromWord64Host = fromWrite word64Host
{-# INLINE fromWord64Host #-}

-- | Write a list of 'Word64's in native host order and host endianness.
fromWord64shost :: [Word64] -> Builder
fromWord64shost = fromWriteList word64Host
{-# INLINE fromWord64shost #-}
