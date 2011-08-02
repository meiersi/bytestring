{-# LANGUAGE BangPatterns, MonoPatBinds #-}
-----------------------------------------------------------------------------
-- | Copyright : (c) 2010 - 2011 Simon Meier
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
    -- * Execution
      AllocationStrategy
    , toLazyByteStringWith
    , toLazyByteStringUntrimmed
    , L.smallChunkSize
    , L.defaultChunkSize
    , safeStrategy
    , untrimmedStrategy

    -- * Creating Builders
    
    -- ** Inserting and copying bytestrings
    , module Data.ByteString.Lazy.Builder.ByteString

    -- ** Host-specific encodings of integers
    , intHost  
    , int16Host
    , int32Host
    , int64Host

    , wordHost  
    , word16Host
    , word32Host
    , word64Host

    -- ** Using bounded encodings
    
    -- | Bounded 'E.Encoding's abstract encodings of Haskell values that can be implemented by
    -- writing a bounded-length sequence of bytes directly to memory. They are
    -- lifted to conversions from Haskell values to 'Builder's by wrapping them
    -- with a bound-check. The compiler can implement this bound-check very
    -- efficiently (i.e, a single comparison of the difference of two pointers to a
    -- constant), because the bound of a 'E.Encoding' is always independent of the
    -- value being encoded and, in most cases, a literal constant.
    --
    -- 'E.Encoding's are the primary means for defining conversion functions from
    -- primitive Haskell values to 'Builder's. Most 'Builder' constructors
    -- provided by this library are implemented that way. 
    -- 'E.Encoding's are also used to construct conversions that exploit the internal
    -- representation of data-structures. 
    --
    -- For example, 'encodeByteStringWith' works directly on the underlying byte
    -- array and uses some tricks to reduce the number of variables in its inner
    -- loop. Its efficiency is exploited for implementing the @filter@ and @map@
    -- functions in "Data.ByteString.Lazy" as
    --
    -- > import qualified Codec.Bounded.Encoding as E
    -- >
    -- > filter :: (Word8 -> Bool) -> ByteString -> ByteString
    -- > filter p = toLazyByteString . encodeLazyByteStringWith write
    -- >   where
    -- >     write = E.encodeIf p E.word8 E.emptyEncoding
    -- >
    -- > map :: (Word8 -> Word8) -> ByteString -> ByteString
    -- > map f = toLazyByteString . encodeLazyByteStringWith (E.word8 E.#. f)
    --
    -- Compared to earlier versions of @filter@ and @map@ on lazy 'L.ByteString's,
    -- these versions use a more efficient inner loop and have the additional
    -- advantage that they always result in well-chunked 'L.ByteString's; i.e, they
    -- also perform automatic defragmentation.
    --
    -- We can also use 'E.Encoding's to improve the efficiency of the following
    -- 'renderString' function from our UTF-8 CSV table encoding example in
    -- "Data.ByteString.Lazy.Builder".
    -- 
    -- > renderString :: String -> Builder
    -- > renderString cs = charUtf8 '"' <> foldMap escape cs <> charUtf8 '"'
    -- >   where
    -- >     escape '\\' = charUtf8 '\\' <> charUtf8 '\\'
    -- >     escape '\"' = charUtf8 '\\' <> charUtf8 '\"'
    -- >     escape c    = charUtf8 c
    --
    -- The idea is to save on 'mappend's by implementing a 'E.Encoding' that escapes
    -- characters and using 'encodeListWith', which implements writing a list of
    -- values with a tighter inner loop and no 'mappend'.
    --
    -- > import Data.ByteString.Lazy.Builder.Extras       -- assume these two 
    -- > import Codec.Bounded.Encoding                     as E  -- imports are present
    -- >          ( Encoding, encodeIf, encode2, (#.), utf8 )
    -- > 
    -- > renderString :: String -> Builder
    -- > renderString cs = 
    -- >     charUtf8 '"' <> encodeListWith escapedUtf8 cs <> charUtf8 '"'
    -- >   where
    -- >     escapedUtf8 :: Encoding Char
    -- >     escapedUtf8 = 
    -- >       encodeIf (== '\\') (encode2 E.utf8 E.utf8 #. const ('\\', '\\')) $
    -- >       encodeIf (== '\"') (encode2 E.utf8 E.utf8 #. const ('\\', '\"')) $
    -- >       E.utf8
    --
    -- This 'Builder' considers a buffer with less than 8 free bytes as full. As
    -- all functions are inlined, the compiler is able to optimize the constant
    -- 'E.Encoding's as two sequential 'poke's. Compared to the first implementation of
    -- 'renderString' this implementation is 1.7x faster.
    --
    , module Data.ByteString.Lazy.Builder.BoundedEncoding
   
    ) where

import Data.ByteString.Lazy.Builder.Internal
import Data.ByteString.Lazy.Builder.ByteString
import Data.ByteString.Lazy.Builder.Word
import Data.ByteString.Lazy.Builder.Int
import Data.ByteString.Lazy.Builder.BoundedEncoding

import qualified Data.ByteString               as S
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy.Internal as L

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

