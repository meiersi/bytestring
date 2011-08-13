{-# LANGUAGE CPP, BangPatterns, OverloadedStrings, MonoPatBinds #-}
-- | Module    : Data.ByteString.Lazy.Builder.ByteString
-- Copyright   : (c) 2010      Jasper Van der Jeugt 
--               (c) 2010-2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Controlling chunk allocation and copying when converting strict and lazy
-- bytestrings to 'Builder's.
--
module Data.ByteString.Lazy.Builder.ByteString
    ( 
      byteStringCopy
    , byteStringInsert
    , byteStringThreshold

    , lazyByteStringCopy
    , lazyByteStringInsert
    , lazyByteStringThreshold
    ) where

import Data.ByteString.Lazy.Builder.Internal 

import Foreign
import Data.Monoid


import qualified Data.ByteString          as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy.Internal as L


------------------------------------------------------------------------------
-- Strict ByteStrings
------------------------------------------------------------------------------


-- | Create a 'Builder' denoting the same sequence of bytes as a strict
-- 'S.ByteString'.
--
-- A 'Builder' defined as @'byteStringThreshold' maxCopySize bs@ copies @bs@, if
-- it is shorter than @maxCopySize@, and inserts it directly, otherwise.
--
{-# INLINE byteStringThreshold #-}
byteStringThreshold :: Int -> S.ByteString -> Builder     
byteStringThreshold maxCopySize = 
    \bs -> builder $ step bs
  where
    step !bs !k br@(BufferRange !op _)
      | maxCopySize < S.length bs = return $ insertChunk op bs k
      | otherwise                 = byteStringCopyStep bs k br

-- | The created 'Builder' always copies the 'S.ByteString'. Use this function
-- to create 'Builder's from smallish (@<= 4kb@) 'S.ByteString's or if you need
-- to guarantee that the 'S.ByteString' is not shared with the chunks generated
-- by the 'Builder'.
--
{-# INLINE byteStringCopy #-}
byteStringCopy :: S.ByteString -> Builder
byteStringCopy = \bs -> builder $ byteStringCopyStep bs

{-# INLINE byteStringCopyStep #-}
byteStringCopyStep :: S.ByteString 
                   -> (BufferRange -> IO (BuildSignal a))
                   -> (BufferRange -> IO (BuildSignal a))
byteStringCopyStep (S.PS ifp ioff isize) !k = 
    goBS (unsafeForeignPtrToPtr ifp `plusPtr` ioff)
  where
    !ipe = unsafeForeignPtrToPtr ifp `plusPtr` (ioff + isize)
    goBS !ip !(BufferRange op ope)
      | inpRemaining <= outRemaining = do
          copyBytes op ip inpRemaining
          touchForeignPtr ifp -- input consumed: OK to release from here
          let !br' = BufferRange (op `plusPtr` inpRemaining) ope
          k br'
      | otherwise = do
          copyBytes op ip outRemaining
          let !ip' = ip `plusPtr` outRemaining
          return $ bufferFull 1 ope (goBS ip')
      where
        outRemaining = ope `minusPtr` op
        inpRemaining = ipe `minusPtr` ip

-- | The created 'Builder' always inserts the 'S.ByteString' directly as a chunk. 
-- This implies flushing the output buffer; even if it contains just
-- a single byte! Hence, you should use 'byteStringInsert' only for large (@>
-- 8kb@) 'S.ByteString's. Otherwise, the generated chunks are too fragmented to
-- be processed efficiently.
--
{-# INLINE byteStringInsert #-}
byteStringInsert :: S.ByteString -> Builder
byteStringInsert = 
    \bs -> builder $ step bs
  where
    step !bs !k !(BufferRange op _) = return $ insertChunk op bs k


-- Lazy bytestrings
------------------------------------------------------------------------------

-- | Chunk-wise application of 'byteStringThreshold' to a lazy 'L.ByteString'.
--
{-# INLINE lazyByteStringThreshold #-}
lazyByteStringThreshold :: Int -> L.ByteString -> Builder
lazyByteStringThreshold maxCopySize = 
  L.foldrChunks (\bs b -> byteStringThreshold maxCopySize bs `mappend` b) mempty

-- | Chunk-wise application of 'byteStringCopy' to a lazy 'L.ByteString'.
--
{-# INLINE lazyByteStringCopy #-}
lazyByteStringCopy :: L.ByteString -> Builder
lazyByteStringCopy = 
  L.foldrChunks (\bs b -> byteStringCopy bs `mappend` b) mempty

-- This function costs /O(n)/ where /n/ is the number of chunks of the lazy
-- 'L.ByteString'. The design of the 'Builder' could be changed to support an
-- /O(1)/ insertion of a difference-list style lazy bytestring. Please contact
-- me, if you have a use case for that.

-- | Chunk-wise application of 'byteStringInsert' to a lazy 'L.ByteString'.
--
{-# INLINE lazyByteStringInsert #-}
lazyByteStringInsert :: L.ByteString -> Builder
lazyByteStringInsert =
  L.foldrChunks (\bs b -> byteStringInsert bs `mappend` b) mempty
