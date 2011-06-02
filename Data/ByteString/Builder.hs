-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) 2010 Jasper Van der Jeugt & Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-----------------------------------------------------------------------------

module Data.ByteString.Builder
    ( 
      -- * The 'Builder' type
      Builder

      -- * Creating builders

      -- ** Signed and unsigned integers
    , word8
    , word8s
    , int8
    , int8s

    , BigEndian(..)
    , LittleEndian(..)
    , HostEndian(..)
      
      -- ** Strict and lazy ByteStrings
    , module Data.ByteString.Builder.ByteString

      -- ** Special primitives
    , flush
      

      -- * Executing builders
    , toLazyByteString
    , toLazyByteStringWith
    -- , toByteString
    -- , toByteStringIO
    -- , toByteStringIOWith

    ) where

import Data.ByteString.Builder.Internal
import Data.ByteString.Builder.ByteString
import Data.ByteString.Builder.Write

import Data.Monoid

import qualified System.IO.Write as W

import Foreign

------------------------------------------------------------------------------
-- Word and Int serialization
------------------------------------------------------------------------------

-- single bytes
---------------

-- | Serialize a single unsigned byte.
--
{-# INLINE word8 #-}
word8 :: Word8 -> Builder
word8 = fromWrite W.word8

-- | Serialize a list of unsigned bytes.
--
word8s :: [Word8] -> Builder
word8s = fromWriteList W.word8

-- | Serialize a single signed byte.
--
{-# INLINE int8 #-}
int8 :: Int8 -> Builder
int8 = fromWrite W.int8

-- | Serialize a list of signed bytes.
--
int8s :: [Int8] -> Builder
int8s = fromWriteList W.int8


-- multiple bytes: class based endianness selection
---------------------------------------------------

class BigEndian a where
    bigEndian     :: a -> Builder

    {-# INLINE bigEndianList #-}
    bigEndianList :: [a] -> Builder
    bigEndianList = mconcat . map bigEndian

class LittleEndian a where
    littleEndian     :: a -> Builder

    {-# INLINE littleEndianList #-}
    littleEndianList :: [a] -> Builder
    littleEndianList = mconcat . map littleEndian

class HostEndian a where
    hostEndian     :: a -> Builder

    {-# INLINE hostEndianList #-}
    hostEndianList :: [a] -> Builder
    hostEndianList = mconcat . map hostEndian


-- list instances

instance BigEndian a => BigEndian [a] where
    {-# INLINE bigEndian #-}
    bigEndian     = bigEndianList

instance LittleEndian a => LittleEndian [a] where
    {-# INLINE littleEndian #-}
    littleEndian     = littleEndianList

instance HostEndian a => HostEndian [a] where
    {-# INLINE hostEndian #-}
    hostEndian     = hostEndianList


-- word instances

instance BigEndian Word8 where
    {-# INLINE bigEndian #-}
    {-# INLINE bigEndianList #-}
    bigEndian     = fromWrite     W.word8
    bigEndianList = fromWriteList W.word8

instance BigEndian Word16 where
    {-# INLINE bigEndian #-}
    {-# INLINE bigEndianList #-}
    bigEndian     = fromWrite     W.word16BE
    bigEndianList = fromWriteList W.word16BE

instance BigEndian Word32 where
    {-# INLINE bigEndian #-}
    {-# INLINE bigEndianList #-}
    bigEndian     = fromWrite     W.word32BE
    bigEndianList = fromWriteList W.word32BE

instance BigEndian Word64 where
    {-# INLINE bigEndian #-}
    {-# INLINE bigEndianList #-}
    bigEndian     = fromWrite     W.word64BE
    bigEndianList = fromWriteList W.word64BE


instance LittleEndian Word8 where
    {-# INLINE littleEndian #-}
    {-# INLINE littleEndianList #-}
    littleEndian     = fromWrite     W.word8
    littleEndianList = fromWriteList W.word8

instance LittleEndian Word16 where
    {-# INLINE littleEndian #-}
    {-# INLINE littleEndianList #-}
    littleEndian     = fromWrite     W.word16LE
    littleEndianList = fromWriteList W.word16LE

instance LittleEndian Word32 where
    {-# INLINE littleEndian #-}
    {-# INLINE littleEndianList #-}
    littleEndian     = fromWrite     W.word32LE
    littleEndianList = fromWriteList W.word32LE

instance LittleEndian Word64 where
    {-# INLINE littleEndian #-}
    {-# INLINE littleEndianList #-}
    littleEndian     = fromWrite     W.word64LE
    littleEndianList = fromWriteList W.word64LE


instance HostEndian Word8 where
    {-# INLINE hostEndian #-}
    {-# INLINE hostEndianList #-}
    hostEndian     = fromWrite     W.word8
    hostEndianList = fromWriteList W.word8

instance HostEndian Word16 where
    {-# INLINE hostEndian #-}
    {-# INLINE hostEndianList #-}
    hostEndian     = fromWrite     W.word16Host
    hostEndianList = fromWriteList W.word16Host

instance HostEndian Word32 where
    {-# INLINE hostEndian #-}
    {-# INLINE hostEndianList #-}
    hostEndian     = fromWrite     W.word32Host
    hostEndianList = fromWriteList W.word32Host

instance HostEndian Word64 where
    {-# INLINE hostEndian #-}
    {-# INLINE hostEndianList #-}
    hostEndian     = fromWrite     W.word64Host
    hostEndianList = fromWriteList W.word64Host

instance HostEndian Word where
    {-# INLINE hostEndian #-}
    {-# INLINE hostEndianList #-}
    hostEndian     = fromWrite     W.wordHost
    hostEndianList = fromWriteList W.wordHost


-- int instances

instance BigEndian Int8 where
    {-# INLINE bigEndian #-}
    {-# INLINE bigEndianList #-}
    bigEndian     = fromWrite     W.int8
    bigEndianList = fromWriteList W.int8

instance BigEndian Int16 where
    {-# INLINE bigEndian #-}
    {-# INLINE bigEndianList #-}
    bigEndian     = fromWrite     W.int16BE
    bigEndianList = fromWriteList W.int16BE

instance BigEndian Int32 where
    {-# INLINE bigEndian #-}
    {-# INLINE bigEndianList #-}
    bigEndian     = fromWrite     W.int32BE
    bigEndianList = fromWriteList W.int32BE

instance BigEndian Int64 where
    {-# INLINE bigEndian #-}
    {-# INLINE bigEndianList #-}
    bigEndian     = fromWrite     W.int64BE
    bigEndianList = fromWriteList W.int64BE


instance LittleEndian Int8 where
    {-# INLINE littleEndian #-}
    {-# INLINE littleEndianList #-}
    littleEndian     = fromWrite     W.int8
    littleEndianList = fromWriteList W.int8

instance LittleEndian Int16 where
    {-# INLINE littleEndian #-}
    {-# INLINE littleEndianList #-}
    littleEndian     = fromWrite     W.int16LE
    littleEndianList = fromWriteList W.int16LE

instance LittleEndian Int32 where
    {-# INLINE littleEndian #-}
    {-# INLINE littleEndianList #-}
    littleEndian     = fromWrite     W.int32LE
    littleEndianList = fromWriteList W.int32LE

instance LittleEndian Int64 where
    {-# INLINE littleEndian #-}
    {-# INLINE littleEndianList #-}
    littleEndian     = fromWrite     W.int64LE
    littleEndianList = fromWriteList W.int64LE


instance HostEndian Int8 where
    {-# INLINE hostEndian #-}
    {-# INLINE hostEndianList #-}
    hostEndian     = fromWrite     W.int8
    hostEndianList = fromWriteList W.int8

instance HostEndian Int16 where
    {-# INLINE hostEndian #-}
    {-# INLINE hostEndianList #-}
    hostEndian     = fromWrite     W.int16Host
    hostEndianList = fromWriteList W.int16Host

instance HostEndian Int32 where
    {-# INLINE hostEndian #-}
    {-# INLINE hostEndianList #-}
    hostEndian     = fromWrite     W.int32Host
    hostEndianList = fromWriteList W.int32Host

instance HostEndian Int64 where
    {-# INLINE hostEndian #-}
    {-# INLINE hostEndianList #-}
    hostEndian     = fromWrite     W.int64Host
    hostEndianList = fromWriteList W.int64Host

instance HostEndian Int where
    {-# INLINE hostEndian #-}
    {-# INLINE hostEndianList #-}
    hostEndian     = fromWrite     W.intHost
    hostEndianList = fromWriteList W.intHost


