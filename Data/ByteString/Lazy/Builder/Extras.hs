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

-- | Sanitize a buffer size; i.e., make it at least the size of a 'Int'.
sanitize :: Int -> Int
sanitize = max (sizeOf (undefined :: Int))

-- | Use this strategy for generating lazy 'L.ByteString's whose chunks are
-- discarded right after they are generated. For example, if you just generate
-- them to write them to a network socket.
untrimmedStrategy :: Int -- ^ Size of the first buffer
                  -> Int -- ^ Size of successive buffers
                  -> AllocationStrategy 
                  -- ^ An allocation strategy that does not trim any of the
                  -- filled buffers before converting it to a chunk.
untrimmedStrategy firstSize bufSize = 
    AllocationStrategy (sanitize firstSize) (sanitize bufSize) (\_ _ -> False)


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
    AllocationStrategy (sanitize firstSize) (sanitize bufSize) 
                       (\used size -> 2*used < size)

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
-- 'L.ByteString's fit into the first allocated buffer and chances are better
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


------------------------------------------------------------------------------
-- ByteString insertion / controlling chunk boundaries
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

------------------------------------------------------------------------------
-- Char8 encoding
------------------------------------------------------------------------------

-- | Encode a 'Char' using the Char8 encoding.
char8 :: Char -> Builder
char8 = E.encodeWith E.char8

-- | Encode a 'String' using the Char8 encoding.
string8 :: String -> Builder
string8 = E.encodeListWith E.char8


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
intHost = E.encodeWith E.intHost

-- | Encode a 'Int16' in native host order and host endianness.
{-# INLINE int16Host #-}
int16Host :: Int16 -> Builder
int16Host = E.encodeWith E.int16Host

-- | Encode a 'Int32' in native host order and host endianness.
{-# INLINE int32Host #-}
int32Host :: Int32 -> Builder
int32Host = E.encodeWith E.int32Host

-- | Encode a 'Int64' in native host order and host endianness.
{-# INLINE int64Host #-}
int64Host :: Int64 -> Builder
int64Host = E.encodeWith E.int64Host

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

-- | Encode a 'Float' in native host order. Values encoded this way are not
-- portable to different endian machines, without conversion.
{-# INLINE floatHost #-}
floatHost :: Float -> Builder
floatHost = E.encodeWith E.floatHost

-- | Encode a 'Double' in native host order.
{-# INLINE doubleHost #-}
doubleHost :: Double -> Builder
doubleHost = E.encodeWith E.doubleHost

