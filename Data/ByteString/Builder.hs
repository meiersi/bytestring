{-# LANGUAGE BangPatterns, MonoPatBinds #-}
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
      -- * The Builder type
      Builder

      -- * Creating builders
    , flush

      -- ** Signed and unsigned integers
    , word8
    , int8

    , BigEndian(..)
    , LittleEndian(..)
    , HostEndian(..)
      
      -- ** Strict and lazy ByteStrings
    , module Data.ByteString.Builder.ByteString


      -- * Executing builders
    , toLazyByteString
    , toLazyByteStringWith
    , toLazyByteStringUntrimmed
    ) where

import Data.ByteString.Builder.Internal
import Data.ByteString.Builder.ByteString
import Data.ByteString.Builder.Write

import qualified Data.ByteString               as S
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy.Internal as L

import qualified System.IO.Write as W

import Foreign

------------------------------------------------------------------------------
-- Builder execution
------------------------------------------------------------------------------

-- allocation strategies

data AllocationStrategy = AllocationStrategy 
         {-# UNPACK #-} !Int  -- size of first buffer
         {-# UNPACK #-} !Int  -- size of successive buffers
         (Int -> Int -> Bool) -- trim

untrimmedStrategy :: Int -- ^ Size of the first buffer
                  -> Int -- ^ Size of successive buffers
                  -> AllocationStrategy
untrimmedStrategy firstSize bufSize = 
    AllocationStrategy firstSize bufSize (\_ _ -> False)


safeStrategy :: Int  -- ^ Size of first buffer
             -> Int  -- ^ Size of successive buffers
             -> AllocationStrategy
safeStrategy firstSize bufSize = 
    AllocationStrategy firstSize bufSize (\used size -> 2*used < size)


-- | Extract the lazy 'L.ByteString' from the builder by running it with default
-- buffer sizes. Use this function, if you do not have any special
-- considerations with respect to buffer sizes.
--
-- @ 'toLazyByteString' b = 'toLazyByteStringWith' 'defaultBufferSize' 'defaultMinimalBufferSize' 'defaultFirstBufferSize' b L.empty@
--
-- Note that @'toLazyByteString'@ is a 'Monoid' homomorphism.
--
-- > toLazyByteString mempty          == mempty
-- > toLazyByteString (x `mappend` y) == toLazyByteString x `mappend` toLazyByteString y
--
-- However, in the second equation, the left-hand-side is generally faster to
-- execute.
--
toLazyByteString :: Builder -> L.ByteString
toLazyByteString = toLazyByteStringWith
    (safeStrategy L.smallChunkSize L.defaultChunkSize) L.Empty

toLazyByteStringUntrimmed :: Builder -> L.ByteString
toLazyByteStringUntrimmed = toLazyByteStringWith
    (untrimmedStrategy L.smallChunkSize L.defaultChunkSize) L.Empty

{-# INLINE toLazyByteStringWith #-}
toLazyByteStringWith :: AllocationStrategy
                      -> L.ByteString 
                      -> Builder 
                      -> L.ByteString
toLazyByteStringWith (AllocationStrategy firstSize bufSize trim) k b = 
    S.inlinePerformIO $ fillNew (runBuilder b) firstSize 
  where
    fillNew !step0 !size = do
        S.mallocByteString size >>= fill step0
      where
        fill !step !fpbuf =
            fillWithBuildStep step doneH fullH insertChunkH br
          where
            op  = unsafeForeignPtrToPtr fpbuf -- safe due to mkbs
            pe  = op `plusPtr` size
            !br = BufferRange op pe
            
            -- we are done: return lazy bytestring continuation
            doneH op' _ 
              | op' == op = return k
              | otherwise = mkbs op' k

            -- buffer full: add chunk if it is non-empty and fill next buffer
            fullH op' minSize nextStep 
              | op' == op = fillNew nextStep (max minSize bufSize)

              | otherwise = 
                  mkbs op' $ S.inlinePerformIO
                           $ fillNew nextStep (max minSize bufSize)
            
            -- insert a chunk: prepend current chunk, if there is one
            insertChunkH op' bs nextStep
              | op' == op =
                  return $ nonEmptyChunk bs 
                         $ S.inlinePerformIO 
                         $ fill nextStep fpbuf

              | otherwise =
                  mkbs op' $ nonEmptyChunk bs 
                           $ S.inlinePerformIO 
                           $ fillNew nextStep bufSize

            -- add a chunk to a lazy bytestring, trimming the chunk if necesary
            mkbs !op' lbs
              | trim filledSize size = do
                  fpbuf' <- S.mallocByteString filledSize
                  copyBytes (unsafeForeignPtrToPtr fpbuf') op filledSize
                  touchForeignPtr fpbuf
                  return $ L.Chunk (S.PS fpbuf' 0 filledSize) lbs
              | otherwise                     = 
                  return $ L.Chunk (S.PS fpbuf 0 filledSize) lbs
              where
                filledSize = op' `minusPtr` op

                    

-- | Prepend the chunk if it is non-empty.
{-# INLINE nonEmptyChunk #-}
nonEmptyChunk :: S.ByteString -> L.ByteString -> L.ByteString
nonEmptyChunk bs lbs | S.null bs = lbs 
                     | otherwise = L.Chunk bs lbs


------------------------------------------------------------------------------
-- Word and Int serialization
------------------------------------------------------------------------------

-- single bytes
---------------

-- | Encode a single unsigned byte as-is.
--
{-# INLINE word8 #-}
word8 :: Word8 -> Builder
word8 = fromWrite W.word8

-- | Encode a single signed byte as-is.
--
{-# INLINE int8 #-}
int8 :: Int8 -> Builder
int8 = fromWrite W.int8


-- multiple bytes: class based endianness selection
---------------------------------------------------

-- | Encode a value using big-endian byte order. 
class BigEndian a where
    bigEndian     :: a -> Builder

-- | Encode a value using little-endian byte-order.
class LittleEndian a where
    littleEndian     :: a -> Builder

-- | Encode a value using host-native byte-order. Note that values encoded in
-- this way are not portable.
class HostEndian a where
    hostEndian     :: a -> Builder


-- word instances

instance BigEndian Word16 where
    {-# INLINE bigEndian #-}
    bigEndian     = fromWrite     W.word16BE

instance BigEndian Word32 where
    {-# INLINE bigEndian #-}
    bigEndian     = fromWrite     W.word32BE

instance BigEndian Word64 where
    {-# INLINE bigEndian #-}
    bigEndian     = fromWrite     W.word64BE

instance LittleEndian Word16 where
    {-# INLINE littleEndian #-}
    littleEndian     = fromWrite     W.word16LE

instance LittleEndian Word32 where
    {-# INLINE littleEndian #-}
    littleEndian     = fromWrite     W.word32LE

instance LittleEndian Word64 where
    {-# INLINE littleEndian #-}
    littleEndian     = fromWrite     W.word64LE

instance HostEndian Word16 where
    {-# INLINE hostEndian #-}
    hostEndian     = fromWrite     W.word16Host

instance HostEndian Word32 where
    {-# INLINE hostEndian #-}
    hostEndian     = fromWrite     W.word32Host

instance HostEndian Word64 where
    {-# INLINE hostEndian #-}
    hostEndian     = fromWrite     W.word64Host

instance HostEndian Word where
    {-# INLINE hostEndian #-}
    hostEndian     = fromWrite     W.wordHost


-- int instances

instance BigEndian Int16 where
    {-# INLINE bigEndian #-}
    bigEndian     = fromWrite     W.int16BE

instance BigEndian Int32 where
    {-# INLINE bigEndian #-}
    bigEndian     = fromWrite     W.int32BE

instance BigEndian Int64 where
    {-# INLINE bigEndian #-}
    bigEndian     = fromWrite     W.int64BE

instance LittleEndian Int16 where
    {-# INLINE littleEndian #-}
    littleEndian     = fromWrite     W.int16LE

instance LittleEndian Int32 where
    {-# INLINE littleEndian #-}
    littleEndian     = fromWrite     W.int32LE

instance LittleEndian Int64 where
    {-# INLINE littleEndian #-}
    littleEndian     = fromWrite     W.int64LE

instance HostEndian Int16 where
    {-# INLINE hostEndian #-}
    hostEndian     = fromWrite     W.int16Host

instance HostEndian Int32 where
    {-# INLINE hostEndian #-}
    hostEndian     = fromWrite     W.int32Host

instance HostEndian Int64 where
    {-# INLINE hostEndian #-}
    hostEndian     = fromWrite     W.int64Host

instance HostEndian Int where
    {-# INLINE hostEndian #-}
    hostEndian     = fromWrite     W.intHost
