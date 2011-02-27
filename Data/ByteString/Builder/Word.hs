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
    , fromWord16be            -- :: Word16   -> Builder
    , fromWord32be            -- :: Word32   -> Builder
    , fromWord64be            -- :: Word64   -> Builder
    , fromWord32sbe           -- :: [Word32] -> Builder
    , fromWord16sbe           -- :: [Word16] -> Builder
    , fromWord64sbe           -- :: [Word64] -> Builder

    -- ** Little-endian serialization
    , fromWord16le            -- :: Word16   -> Builder
    , fromWord32le            -- :: Word32   -> Builder
    , fromWord64le            -- :: Word64   -> Builder
    , fromWord16sle           -- :: [Word16] -> Builder
    , fromWord32sle           -- :: [Word32] -> Builder
    , fromWord64sle           -- :: [Word64] -> Builder

    -- ** Host-endian serialization
    , fromWordhost            -- :: Word     -> Builder
    , fromWord16host          -- :: Word16   -> Builder
    , fromWord32host          -- :: Word32   -> Builder
    , fromWord64host          -- :: Word64   -> Builder
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
fromWord8 = fromWriteSingleton writeWord8

-- | Serialize a list of bytes.
--
fromWord8s :: [Word8] -> Builder
fromWord8s = fromWriteList writeWord8


-- Word16
------------------------------------------------------------------------------

-- | Serialize a 'Word16' in big endian format.
fromWord16be :: Word16 -> Builder
fromWord16be = fromWriteSingleton writeWord16be 
{-# INLINE fromWord16be #-}

-- | Serialize a list of 'Word16's in big endian format.
fromWord16sbe :: [Word16] -> Builder
fromWord16sbe = fromWriteList writeWord16be 
{-# INLINE fromWord16sbe #-}

-- | Serialize a 'Word16' in little endian format.
fromWord16le :: Word16 -> Builder
fromWord16le = fromWriteSingleton writeWord16le 
{-# INLINE fromWord16le #-}

-- | Serialize a list of 'Word16's in little endian format.
fromWord16sle :: [Word16] -> Builder
fromWord16sle = fromWriteList writeWord16le 
{-# INLINE fromWord16sle #-}


-- Word32
-----------------------------------------------------------------------------

-- | Serialize a 'Word32' in big endian format.
fromWord32be :: Word32 -> Builder
fromWord32be = fromWriteSingleton writeWord32be 
{-# INLINE fromWord32be #-}

-- | Serialize a list of 'Word32's in big endian format.
fromWord32sbe :: [Word32] -> Builder
fromWord32sbe = fromWriteList writeWord32be 
{-# INLINE fromWord32sbe #-}

-- | Serialize a 'Word32' in little endian format.
fromWord32le :: Word32 -> Builder
fromWord32le = fromWriteSingleton writeWord32le 
{-# INLINE fromWord32le #-}

-- | Serialize a list of 'Word32's in little endian format.
fromWord32sle :: [Word32] -> Builder
fromWord32sle = fromWriteList writeWord32le 
{-# INLINE fromWord32sle #-}

-- | Serialize a 'Word64' in big endian format.
fromWord64be :: Word64 -> Builder
fromWord64be = fromWriteSingleton writeWord64be 
{-# INLINE fromWord64be #-}

-- | Serialize a list of 'Word64's in big endian format.
fromWord64sbe :: [Word64] -> Builder
fromWord64sbe = fromWriteList writeWord64be 
{-# INLINE fromWord64sbe #-}

-- | Serialize a 'Word64' in little endian format.
fromWord64le :: Word64 -> Builder
fromWord64le = fromWriteSingleton writeWord64le 
{-# INLINE fromWord64le #-}

-- | Serialize a list of 'Word64's in little endian format.
fromWord64sle :: [Word64] -> Builder
fromWord64sle = fromWriteList writeWord64le 
{-# INLINE fromWord64sle #-}


------------------------------------------------------------------------
-- Unaligned, word size ops

-- | Serialize a single native machine 'Word'. The 'Word' is serialized in host
-- order, host endian form, for the machine you're on. On a 64 bit machine the
-- 'Word' is an 8 byte value, on a 32 bit machine, 4 bytes. Values written this
-- way are not portable to different endian or word sized machines, without
-- conversion.
--
fromWordhost :: Word -> Builder
fromWordhost = fromWriteSingleton writeWordhost 
{-# INLINE fromWordhost #-}

-- | Serialize a list of 'Word's.
-- See 'fromWordhost' for usage considerations.
fromWordshost :: [Word] -> Builder
fromWordshost = fromWriteList writeWordhost 
{-# INLINE fromWordshost #-}

-- | Write a 'Word16' in native host order and host endianness.
fromWord16host :: Word16 -> Builder
fromWord16host = fromWriteSingleton writeWord16host 
{-# INLINE fromWord16host #-}

-- | Write a list of 'Word16's in native host order and host endianness.
fromWord16shost :: [Word16] -> Builder
fromWord16shost = fromWriteList writeWord16host 
{-# INLINE fromWord16shost #-}

-- | Write a 'Word32' in native host order and host endianness.
fromWord32host :: Word32 -> Builder
fromWord32host = fromWriteSingleton writeWord32host 
{-# INLINE fromWord32host #-}

-- | Write a list of 'Word32's in native host order and host endianness.
fromWord32shost :: [Word32] -> Builder
fromWord32shost = fromWriteList writeWord32host 
{-# INLINE fromWord32shost #-}

-- | Write a 'Word64' in native host order and host endianness.
fromWord64host :: Word64 -> Builder
fromWord64host = fromWriteSingleton writeWord64host
{-# INLINE fromWord64host #-}

-- | Write a list of 'Word64's in native host order and host endianness.
fromWord64shost :: [Word64] -> Builder
fromWord64shost = fromWriteList writeWord64host
{-# INLINE fromWord64shost #-}
