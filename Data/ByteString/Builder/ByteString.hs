{-# LANGUAGE CPP, BangPatterns, OverloadedStrings, MonoPatBinds #-}
-- | Module    : Data.ByteString.Builder.ByteString
-- Copyright   : (c) 2010      Jasper Van der Jeugt 
--               (c) 2010-2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Converting strict and lazy ByteStrings to 'Builder's.
--
module Data.ByteString.Builder.ByteString
    ( 
      byteString
    , lazyByteString

    -- * Controlling copying and chunk insertion
    , byteStringWith
    , copyByteString
    , insertByteString

    , lazyByteStringWith
    , copyLazyByteString
    , insertLazyByteString

    ) where

import Data.ByteString.Builder.Internal 

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
-- The 'Builder' copies short 'S.ByteString's and inserts long (>= 8kb)
-- 'S.ByteString's directly. This way the 'Builder' will always generate large
-- enough (> 4kb) chunks on average, which is important for the efficiency of
-- consumers of the generated chunks. If you have a special application that
-- requires more precise control over chunk handling, then see the module
-- "Data.ByteString.Builder.ByteString".
--
{-# INLINE byteString #-}
byteString :: S.ByteString -> Builder
byteString = byteStringWith defaultMaximalCopySize


-- | Create a 'Builder' denoting the same sequence of bytes as a strict
-- 'S.ByteString'.
--
-- A 'Builder' defined as @'byteStringWith' maxCopySize bs@ copies @bs@, if
-- it is shorter than @maxCopySize@, and inserts it directly, otherwise.
--
{-# INLINE byteStringWith #-}
byteStringWith :: Int -> S.ByteString -> Builder     
byteStringWith maxCopySize = 
    \bs -> builder $ step bs
  where
    step !bs !k br@(BufferRange !op _)
      | maxCopySize < S.length bs = return $ insertChunk op bs k
      | otherwise                 = copyByteStringStep bs k br

-- | The created 'Builder' always copies the 'S.ByteString'. Use this function
-- to create 'Builder's from smallish (@<= 4kb@) 'S.ByteString's or if you need
-- to guarantee that the 'S.ByteString' is not shared with the chunks generated
-- by the 'Builder'.
--
{-# INLINE copyByteString #-}
copyByteString :: S.ByteString -> Builder
copyByteString = \bs -> builder $ copyByteStringStep bs

{-# INLINE copyByteStringStep #-}
copyByteStringStep :: S.ByteString 
                   -> (BufferRange -> IO (BuildSignal a))
                   -> (BufferRange -> IO (BuildSignal a))
copyByteStringStep (S.PS ifp ioff isize) !k = 
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
-- Note that this implies flushing the output buffer; even if it contains just
-- a single byte. Hence, you should use 'insertByteString' only for large (@>
-- 8kb@) 'S.ByteString's. Otherwise, the generated chunks are too fragmented to
-- be processed efficiently.
--
{-# INLINE insertByteString #-}
insertByteString :: S.ByteString -> Builder
insertByteString = 
    \bs -> builder $ step bs
  where
    step !bs !k !(BufferRange op _) = return $ insertChunk op bs k


-- Lazy bytestrings
------------------------------------------------------------------------------

-- | Chunk-wise application of 'byteString' to a lazy 'L.ByteString'.
--
{-# INLINE lazyByteString #-}
lazyByteString :: L.ByteString -> Builder
lazyByteString = lazyByteStringWith defaultMaximalCopySize

-- | Chunk-wise application of 'byteStringWith' to a lazy 'L.ByteString'.
--
{-# INLINE lazyByteStringWith #-}
lazyByteStringWith :: Int          -- ^ Maximal number of bytes to copy.
                   -> L.ByteString -- ^ Lazy 'L.ByteString' to serialize.
                   -> Builder      -- ^ Resulting 'Builder'.
lazyByteStringWith maxCopySize = 
  L.foldrChunks (\bs b -> byteStringWith maxCopySize bs `mappend` b) mempty

-- | Chunk-wise application of 'copyByteString' to a lazy 'L.ByteString'.
--
{-# INLINE copyLazyByteString #-}
copyLazyByteString :: L.ByteString -> Builder
copyLazyByteString = 
  L.foldrChunks (\bs b -> copyByteString bs `mappend` b) mempty

-- This function costs /O(n)/ where /n/ is the number of chunks of the lazy
-- 'L.ByteString'. The design of the 'Builder' could be changed to support an
-- /O(1)/ insertion of a difference-list style lazy bytestring. Please contact
-- me, if you have a use case for that.

-- | Chunk-wise application of 'insertByteString' to a lazy 'L.ByteString'.
--
{-# INLINE insertLazyByteString #-}
insertLazyByteString :: L.ByteString -> Builder
insertLazyByteString =
  L.foldrChunks (\bs b -> insertByteString bs `mappend` b) mempty

-- | The maxiamal size of a bytestring that is copied. 
-- @2 * 'L.smallChunkSize'@ to guarantee that on average a chunk is of
-- 'L.smallChunkSize'.
defaultMaximalCopySize :: Int
defaultMaximalCopySize = 2 * L.smallChunkSize
