{-# LANGUAGE BangPatterns, MonoPatBinds #-}
-----------------------------------------------------------------------------
-- | Copyright : (c) 2010 Jasper Van der Jeugt 
--               (c) 2010 - 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--   
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- TODO: Intro and example
-----------------------------------------------------------------------------

module Data.ByteString.Builder
    ( 
      -- * The Builder type
      Builder
      -- | Converting a list to a 'Builder' often works by converting each
      -- element and concatenating the resulting 'Builder's. The higher-order
      -- function abstracting this pattern is 'foldMap' from the
      -- "Data.Foldable" module. We reexport it here for convenience.
    , foldMap

      -- * Creating Builders
    , flush
    , byteString
    , lazyByteString

      -- ** Encoding characters
    , module Data.ByteString.Builder.Char.Utf8

      -- ** Encoding integers
    , module Data.ByteString.Builder.Word
    , module Data.ByteString.Builder.Int
      
      -- * Executing Builders
    , toLazyByteString

    -- ** Controlling chunk allocation
    , AllocationStrategy
    , toLazyByteStringWith
    , toLazyByteStringUntrimmed
    , L.smallChunkSize
    , L.defaultChunkSize
    , safeStrategy
    , untrimmedStrategy
    ) where

import Data.ByteString.Builder.Internal
import Data.ByteString.Builder.ByteString
import Data.ByteString.Builder.Word
import Data.ByteString.Builder.Int
import Data.ByteString.Builder.Char.Utf8

import qualified Data.ByteString               as S
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy.Internal as L

import Data.Foldable (foldMap)

import Foreign


------------------------------------------------------------------------------
-- Builder execution
------------------------------------------------------------------------------


-- | A buffer allocation strategy for executing 'Builder's. 

-- The strategy
--
-- > 'AllocationStrategy' firstBufSize bufSize trim
--
-- states that the first buffer is of size @firstBufSize@, all following buffers
-- are of size @bufSize@, and a buffer of size @n@ filled with @k@ bytes should
-- be trimmed iff @trim k n@ is 'True'.
data AllocationStrategy = AllocationStrategy 
         {-# UNPACK #-} !Int  -- size of first buffer
         {-# UNPACK #-} !Int  -- size of successive buffers
         (Int -> Int -> Bool) -- trim

-- | Use this strategy for generating lazy 'L.ByteString's whose chunks are
-- discarded right after they are generated. For example, if you just generate
-- them to write them to a network socket.
untrimmedStrategy :: Int -- ^ Size of the first buffer
                  -> Int -- ^ Size of successive buffers
                  -> AllocationStrategy 
                  -- ^ An allocation strategy that does not trim any of the
                  -- filled buffers before converting it to a chunk.
untrimmedStrategy firstSize bufSize = 
    AllocationStrategy firstSize bufSize (\_ _ -> False)


-- | Use this strategy for generating lazy 'L.ByteString's whose chunks are
-- likely to survive one garbage collection. Note that
--
-- > toLazyByteString = 
-- >   toLazyByteStringWith (safeStrategy smallChunkSize defaultChunkSize) empty
--
-- where @empty@ is the zero-length lazy 'L.ByteString'.
safeStrategy :: Int  -- ^ Size of first buffer
             -> Int  -- ^ Size of successive buffers
             -> AllocationStrategy
             -- ^ An allocation strategy that guarantees that at least half
             -- of the allocated memory is used for live data
safeStrategy firstSize bufSize = 
    AllocationStrategy firstSize bufSize (\used size -> 2*used < size)


-- | Execute a 'Builder' and record the generated chunks as a lazy
-- 'L.ByteString'. 
--
-- Execution works such that a buffer is allocated and the 'Builder' is told to
-- fill it. Once the 'Builder' returns, the buffer is converted to a chunk of
-- the lazy 'L.ByteString' as follows. If less than half of the buffer is filled,
-- then the filled part is copied to a new chunk of the right size. Otherwise,
-- the buffer is converted directly to a chunk. This scheme guarantees that
-- at least half of the reserved memory is used for live data. 
--
-- The first allocated buffer is of size 'L.smallChunkSize' too keep the
-- allocation overhead small for short output. The following buffers are of
-- size 'L.defaultChunkSize' to ensure that the average chunk size is large.
-- These numbers have worked well in practice. See 'toLazyByteStringWith', if
-- you need more control over buffer allocation.
toLazyByteString :: Builder -> L.ByteString
toLazyByteString = toLazyByteStringWith
    (safeStrategy L.smallChunkSize L.defaultChunkSize) L.Empty

-- | Execute a 'Builder' with the 'untrimmedStrategy' and the same buffer
-- sizes as 'toLazyByteString'. 
--
-- Use this function for generating lazy 'L.ByteString's whose chunks are
-- discarded right after they are generated. For example, if you just generate
-- and write them them to a network socket.
toLazyByteStringUntrimmed :: Builder -> L.ByteString
toLazyByteStringUntrimmed = toLazyByteStringWith
    (untrimmedStrategy L.smallChunkSize L.defaultChunkSize) L.Empty

-- | Execute a 'Builder' with custom execution parameters.
--
-- In most cases, the parameters used by 'toLazyByteString' give good
-- performance. A slightly sub-performing case is generating lots of short
-- (<128 bytes) 'L.ByteString's using 'Builder's. In this case, you might gain
-- additional performance by executing the 'Builder's using
--
-- > toLazyByteStringWith (safeStrategy 128 smallChunkSize) empty
--
-- This reduces the allocation and trimming overhead, as all generated
-- 'L.ByteString's fit into ther first allocated buffer and chances are better
-- that the buffer doesn't have to be trimmed.
--
{-# INLINE toLazyByteStringWith #-}
toLazyByteStringWith 
    :: AllocationStrategy
       -- ^ Buffer allocation strategy to use
    -> L.ByteString  
       -- ^ Lazy 'L.ByteString' to use as the tail of the generated lazy
       -- 'L.ByteString'
    -> Builder 
       -- ^ Builder to execute
    -> L.ByteString
       -- ^ Resulting lazy 'L.ByteString'
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
