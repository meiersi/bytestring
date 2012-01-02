{-# LANGUAGE CPP, BangPatterns, ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{- | Copyright : (c) 2010-2011 Simon Meier
License        : BSD3-style (see LICENSE)

Maintainer     : Simon Meier <iridcode@gmail.com>
Stability      : experimental
Portability    : GHC

(Almost) zero-copy algorithms for prefixing 'Builder's with their (chunk) sizes.
-}
module Data.ByteString.Lazy.Builder.Transformers (

  -- * Prefixing Builders with their (chunk) size
{- |
(Almost) zero-copy algorithms for prefixing 'Builder's with their (chunk) sizes.
-}
    encodeSizePrefixed
  , encodeChunked

  , PaddedSizeEncoding
  , word64Base128LEPadded
  , word64HexPadded
  , word64DecPadded



  ) where

import           Data.ByteString.Lazy.Builder.Internal
import           Data.ByteString.Lazy.Builder.BasicEncoding.Internal.UncheckedShifts
import           Data.ByteString.Lazy.Builder.BasicEncoding.Internal.Base16 (lowerTable, encode4_as_8)

import qualified Data.ByteString               as S
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy.Internal as L

import           Data.Monoid
import           Data.List (unfoldr)  -- HADDOCK ONLY
import           Data.Char (chr, ord)
import           Control.Monad ((<=<), unless)
import           Control.Exception (evaluate)

import           Data.ByteString.Lazy.Builder.BasicEncoding.Internal as E
import           Data.ByteString.Lazy.Builder.BasicEncoding.Binary as E
import           Data.ByteString.Lazy.Builder.BasicEncoding.ASCII as E
import           Data.ByteString.Lazy.Builder.BasicEncoding as E

import           Foreign
import           Foreign.C.Types

------------------------------------------------------------------------------
-- Padded encodings
------------------------------------------------------------------------------

-- | 'PaddedSizeEncoding's are used by the 'Builder' transformers
-- 'encodeSizePrefixed' and 'encodeChunked' to implement (almost) zero-copy
-- algorithms for prefixing 'Builder's with their (chunk) sizes. Given an
-- upper bound on the (chunk) sizes to be encoded, a 'FixedEncoding' of
-- minimal 'size' is constructed that can encode all (chunk) sizes less or
-- equal to the upper bound.
type PaddedSizeEncoding = Word64 -> FixedEncoding Word64   

-- | Count how many digits are necessary to encode a given number.
{-# INLINE countDigits #-}
countDigits :: (Word64 -> Word64)  -- ^ Shift number by one digit
            -> (Word64 -> Word64)  -- ^ Shift number by two digits
            -> Word64              -- ^ Number whose of digits we want to count
            -> Int
countDigits shift1 shift2 x0 =
    -- TODO: implement using CPU instruction that returns top-most set bit.
    count 2 x0
  where
    count !n x
      | x' > 0        = count (n + 2) x'
      | shift1 x == 0 = n - 1
      | otherwise     = n
      where
        x' = shift2 x

base128LESize :: Word64 -> Int
base128LESize = countDigits (`shiftr_w64` 7) (`shiftr_w64` 14)

decSize :: Word64 -> Int
decSize = countDigits (`div` 10) (`div` 100)

hexSize :: Word64 -> Int
hexSize = countDigits (`shiftr_w64` 4) (`shiftr_w64` 8)

-- | GHC's code generator is not (yet) on par with gcc. We therefore use a
-- C-based implementation.
foreign import ccall unsafe "static _hs_bytestring_word64Base128LEFixed"
    c_word64Base128LEFixed :: CInt -> CULLong -> Ptr Word8 -> IO ()

-- | GHC's code generator is not (yet) on par with gcc. We therefore use a
-- C-based implementation.
foreign import ccall unsafe "static _hs_bytestring_word64DecFixed"
    c_word64DecFixed :: CChar -> CInt -> CULLong -> Ptr Word8 -> IO ()

-- | GHC's code generator is not (yet) on par with gcc. We therefore use a
-- C-based implementation.
foreign import ccall unsafe "static _hs_bytestring_word64HexFixed"
    c_word64HexFixed :: CChar -> CInt -> CULLong -> Ptr Word8 -> IO ()

-- | A 'PaddedSizeEncoding' using the variable-length, little-endian, base-128
-- encoding defined above.
--
-- >  toLazyByteString (E.encodeWithF (word64Base128LEPadded 1032) 12)
-- >= Chunk "\x8c\x00" Empty
--
{-# INLINE word64Base128LEPadded #-}
word64Base128LEPadded :: PaddedSizeEncoding
word64Base128LEPadded bound =
    fixedEncoding len 
                  (c_word64Base128LEFixed (fromIntegral len) . fromIntegral)
  where
    len = base128LESize bound

-- | A 'PaddedSizeEncoding' using a decimal encoding with ASCII digits, padded
-- at the front.
--
-- >  toLazyByteString (E.encodeWithF (word64DecPadded '0' 1032) 12)
-- >= Chunk "0012" Empty
--
{-# INLINE word64DecPadded #-}
word64DecPadded :: Char             -- ^ Padding 'Char' (truncated to 8 bits)
                -> PaddedSizeEncoding
word64DecPadded paddingChar bound =
    fixedEncoding len 
                  (c_word64DecFixed pad (fromIntegral len) . fromIntegral)
  where
    len = decSize bound
    pad = fromIntegral (ord paddingChar)


-- | A 'PaddedSizeEncoding' using a hexadecimal encoding with lower-case ASCII
-- characters, padded at the front.
--
-- >  toLazyByteString (E.encodeWithF (word64HexPadded '0' 1032) 12)
-- >= Chunk "00c" Empty
--
{-# INLINE word64HexPadded #-}
word64HexPadded :: Char             -- ^ Padding 'Char' (truncated to 8 bits)
                -> PaddedSizeEncoding  
word64HexPadded paddingChar bound =
    fixedEncoding len 
                  (c_word64HexFixed pad (fromIntegral len) . fromIntegral)
  where
    len = hexSize bound
    pad = fromIntegral (ord paddingChar)

------------------------------------------------------------------------------
-- Chunked Encoding Transformer
------------------------------------------------------------------------------

-- | /Heavy inlining./ A 'Builder' transformer for implementing chunked
-- transfer encodings analogous to the HTTP chunked transfer encoding
-- (<http://en.wikipedia.org/wiki/Chunked_transfer_encoding>).
-- Chunked transfer encodings allow delimiting the size of
-- a sequence of bytes without completely buffering it.
-- They achieve this by encoding the sequence of bytes as a sequence of chunks
-- that are prefixed with their size. The chunk sizes are chosen such that
-- each chunk fits into the buffer being used for encoding.
-- A chunk of size zero is used to mark the end of a chunked transfer
-- encoding.
--
-- The 'encodeChunked' function transforms an inner 'Builder' such that its
-- chunks are prefixed with their size and suffixed with a chunk terminator.
-- For example, it can be used to implement the HTTP chunked transfer encoding
-- as follows.
--
-- @
--import           Data.Monoid (mappend)
--import qualified "Data.ByteString.Lazy.Builder"               as B
--import qualified "Data.ByteString.Lazy.Builder.BasicEncoding" as E
--
--(\<\>) :: Monoid m => m -> m -> m
--(\<\>) = 'mappend'
--
--{-\# NOINLINE httpChunkedTransfer \#-}
--httpChunkedTransfer :: Builder -> Builder
--httpChunkedTransfer body =
--    'encodeChunked' sizeEncoding chunkTerminator body \<\> endOfChunks
--  where
--    sizeEncoding bound = 
--      (\size -> (size, ())) '>$<' 'pairF' ('word64HexPadded' \'0\' bound) crlf
--    chunkTerminator   = E.'fromF' crlf
--    endOfChunks = B.char7 \'0\' \<\> E.'encodeWithF' crlf () \<\> E.'encodeWithF' crlf ()
--    crlf        = const (\'\r\',\'\n\') '>$<' 'pairF' E.'char7' E.'char7'
--    {-\# INLINE crlf \#-}  -- Always inline 'FixedEncoding's and 'BoundedEncoding's
-- @
--
-- The implementation uses a zero-copy algorithm:
-- it reserves space for encoding the size of the remaining free space of the
-- current buffer and suffixing the chunk terminator. Then it calls the inner
-- 'Builder'. Once, the inner 'Builder' requests a new buffer, the size of the
-- data written in the current buffer by the inner 'Builder' is encoded into
-- the reserved space and the chunk is suffixed with the chunk
-- terminator. The cases where the inner 'Builder' is finished or where it
-- wants to insert a 'S.ByteString' directly are handled analogously.
--
-- Note that the chunk sizes chosen by this algorithm depend on the sizes of
-- the buffers used for executing it, the 'Builder's executed before,
-- and the 'Builder's executed by it. Hence, the following does not hold in
-- general.
--
-- >   toLazyByteString (b1 <> httpChunkedTransfer b2)
-- >== (toLazyByteString b1) <> (toLazyByteString (httpChunkedTransfer b2))
--
-- We only have a monoid homomorphism /after/ removing the wrapping from the chunks.
--
{-# INLINE encodeChunked #-}
encodeChunked
    :: PaddedSizeEncoding
    -- ^ The padded size encoding for prefixing chunks with their size
    -> BoundedEncoding Word64
    -- ^ The encoding for terminating a chunk of the given size
    -> Builder
    -- ^ Inner Builder whose chunks must be size-prefixed
    -> Builder
    -- ^ Chunked 'Builder'
encodeChunked mkBeforeFE afterBE =
    fromPut . putChunked mkBeforeFE afterBE . putBuilder

-- | /Heavy inlining./
{-# INLINE putChunked #-}
putChunked
    :: PaddedSizeEncoding
    -- ^ Given a sizeBound on the maximal encodable size this function must return
    -- a fixed-size encoding for encoding all smaller size.
    -> BoundedEncoding Word64
    -- ^ Encoding a directly inserted chunk.
    -> Put a
    -- ^ Inner Put to transform
    -> Put a
    -- ^ 'Put' with chunked encoding.
putChunked mkBeforeFE afterBE p =
    put encodingStep
  where
    reservedAfter, maxReserved, minBufferSize :: Int

    -- reserved space must be computed for maximum buffer size to cover for all
    -- sizes of the actually returned buffer.
    reservedAfter = sizeBound afterBE
    maxReserved   = size (mkBeforeFE maxBound) + reservedAfter
    -- Our buffer must have at least maxReserved free bytes for the inner
    -- 'Builder' to amortize the chunking overhead in a one-to-one fashion.
    minBufferSize = 2 * maxReserved

    encodingStep k =
        fill (runPut p)
      where
        fill innerStep !(BufferRange op ope)
          | outRemaining < minBufferSize =
              evaluate $ bufferFull minBufferSize op (fill innerStep)
          | otherwise = do
              fillWithBuildStep innerStep doneH fullH insertChunksH brInner
          where
            outRemaining   = ope `minusPtr` op
            beforeFE       = mkBeforeFE $ fromIntegral outRemaining
            reservedBefore = size beforeFE

            opInner        = op  `plusPtr` reservedBefore
            opeInner       = ope `plusPtr` (-reservedAfter)
            brInner        = BufferRange opInner opeInner

            {-# INLINE wrapChunk #-}
            wrapChunk :: Ptr Word8 -> IO (Ptr Word8)
            wrapChunk !opInner'
              | innerSize == 0 = return op -- no data written => no chunk to wrap
              | otherwise      = do
                  runF beforeFE innerSize op
                  runB afterBE innerSize opInner'
              where
                innerSize = fromIntegral $ opInner' `minusPtr` opInner

            doneH opInner' x = do
                op' <- wrapChunk opInner'
                let !br' = BufferRange op' ope
                k x br'

            fullH opInner' minSize nextInnerStep = do
                op' <- wrapChunk opInner'
                evaluate $ bufferFull
                  (max minBufferSize (minSize + maxReserved))
                  op'
                  (fill nextInnerStep)

            insertChunksH opInner' chunks@(SizedChunks n _) Nothing nextInnerStep
              | n == 0 = do                    -- flush
                  op' <- wrapChunk opInner'
                  evaluate $ insertChunks op' chunks (fill nextInnerStep)

              | otherwise = do                 -- insert non-empty bytestring
                  op' <- wrapChunk opInner'
                  let !br' = BufferRange op' ope
                  runBuilderWith chunkB (fill nextInnerStep) br'
              where
                nWord64 = (fromIntegral n) :: Word64
                chunkB =
                  encodeWithF (mkBeforeFE nWord64) nWord64 `mappend`
                  sizedChunksInsert chunks Nothing         `mappend`
                  encodeWithB afterBE nWord64

            -- TODO: Think about exploiting this last partially filled buffer.
            insertChunksH opInner' chunks (Just buf) nextInnerStep = do
                let chunk = sizedChunk $ byteStringFromBuffer buf
                insertChunksH opInner' (chunks `mappend` chunk) Nothing nextInnerStep


-- | /Heavy inlining./ Prefix a 'Builder' with its size.
--
-- The implementation uses an almost zero-copy algorithm: it reserves space
-- for encoding the size of the remaining free space of the current buffer. Then it calls the inner
-- 'Builder'. If the inner 'Builder' finishes in the remaining free space of
-- the current buffer,
-- then the actual size is encoded in the reserved space, possibly padding it
-- to fill the whole reserved space. If the inner 'Builder' requires more
-- space, then its remainder is executed using the given 'AllocationStrategy'.
-- If the actual size does not fit into the reserved space, then the data
-- already written in the current buffer is moved.
-- This algorithm minimizes the number of chunk boundaries at the cost of a
-- slight encoding overhead due to padding.
--
-- A typical use-case for this 'Builder' transformer is the implementation of
-- Google's protocol buffer format
-- <http://code.google.com/apis/protocolbuffers/docs/encoding.html>.
-- This format uses a length-delimited encoding for strings and embedded
-- messages. Google's protocol buffer library encodes its messages in two
-- passes. It first computes all the lengths before performing the actual
-- encoding in a second pass.
-- At the cost of using a few bytes too many, we can implement this encoding
-- in a /single-pass/ and /independent/ of the structure of the inner
-- 'Builder' using the following 'Builder' transformer.
--
-- @
-- {-\# NOINLINE pbLengthDelimited \#-}
-- pbLengthDelimited :: Builder -> Builder
-- pbLengthDelimited body = 
--     'encodeSizePrefixed' strategy 'word64Base128LEPadded' body
--   where
--     strategy  = 'untrimmedStrategy' chunkSize chunkSize
--     -- a bit less than 2 ^ 14 bytes => at most 1 byte padding overhead
--     chunkSize = 16 * 1024 - 'chunkOverhead'
-- @
--
-- Note that the allocation @strategy@ is used to run the inner 'Builder' in
-- case it overflows the current buffer. It is therefore sensible to use 
-- the same @chunkSize@ for the all buffers filled using the inner 'Builder'.
--
-- When running a 'Builder' that uses 'encodeSizePrefixed', then the amount of
-- bytes spent for padding the encoded sizes depends on the buffer sizes used
-- for running the 'Builder' and the compactness of the padded size encoding.
-- When running the above 'pbLengthDelimited' function with a maximal buffer
-- size equal to @chunkSize@, then at most @1@ byte is spent on padding for
-- inner 'Builder's smaller than @127@ bytes. For larger 'Builder's, no
-- padding is used.
{-# INLINE encodeSizePrefixed #-}
encodeSizePrefixed
    ::
       AllocationStrategy
    -- ^ Allocation strategy for the inner 'Builder' in case it overflows the
    -- current buffer
    -> PaddedSizeEncoding
    -- ^ A padded encoding for sizes
    -> Builder
    -- ^ Inner 'Builder' to prefix with its size
    -> Builder
    -- ^ Size-prefixed 'Builder'
encodeSizePrefixed innerBufSize mkSizeFE =
    fromPut . putSizePrefixed innerBufSize mkSizeFE . putBuilder

{-# NOINLINE pbLengthDelimited #-}
pbLengthDelimited :: Builder -> Builder
pbLengthDelimited body = 
    encodeSizePrefixed strategy word64Base128LEPadded body
  where
    strategy  = untrimmedStrategy chunkSize chunkSize
    -- a bit less than 2 ^ 14 bytes => at most 1 byte padding overhead
    chunkSize = 16 * 1024 - chunkOverhead

-- | Prefix a 'Put' with the size of its written data.
{-# INLINE putSizePrefixed #-}
putSizePrefixed
    :: forall a.
       AllocationStrategy
    -- ^ Allocation strategy for the inner 'Put' in case it overflows the
    -- current buffer
    -> PaddedSizeEncoding
    -- ^ Encoding the size for the fallback case.
    -> Put a
    -- ^ 'Put' to prefix with the length of its sequence of bytes
    -> Put a
putSizePrefixed strategy mkSizeFE innerP =
    putBuilder (ensureFree minFree) >> put encodingStep
  where
    -- | The minimal free size is such that we can encode any size.
    minFree = size $ mkSizeFE maxBound

    encodingStep :: (forall r. (a -> BuildStep r) -> BuildStep r)
    encodingStep k =
        fill (runPut innerP)
      where
        fill :: BuildStep a -> BufferRange -> IO (BuildSignal r)
        fill innerStep !(BufferRange op ope) =
            fillWithBuildStep innerStep doneH fullH insertChunksH brInner
          where
            outRemaining   = ope `minusPtr` op
            sizeFE         = mkSizeFE $ fromIntegral outRemaining
            reservedBefore = size sizeFE
            reservedAfter  = minFree - reservedBefore

            -- leave enough free space such that all sizes can be encodded.
            startInner    = op  `plusPtr` reservedBefore
            opeInner      = ope `plusPtr` (negate reservedAfter)
            brInner       = BufferRange startInner opeInner

            -- fast path
            doneH :: Ptr Word8 -> a -> IO (BuildSignal r)
            doneH opInner' x = do
                op' <- fastPrefixSize opInner'
                let !br' = BufferRange op' ope
                k x br'

            fastPrefixSize :: Ptr Word8 -> IO (Ptr Word8)
            fastPrefixSize !opInner'
              | innerSize == 0 = do runB (toB $ mkSizeFE 0) 0         op
              | otherwise      = do runF (sizeFE)           innerSize op
                                    return opInner'
              where
                innerSize = fromIntegral $ opInner' `minusPtr` startInner

            -- slow path
            fullH :: Ptr Word8 -> Int -> BuildStep a -> IO (BuildSignal r)
            fullH opInner' minSize =
                slowPrefixSize opInner' (ensureFree minSize)

            insertChunksH :: Ptr Word8 -> SizedChunks -> Maybe Buffer
                          -> BuildStep a -> IO (BuildSignal r)
            insertChunksH opInner' chunks mayBuf =
                slowPrefixSize opInner' (sizedChunksInsert chunks mayBuf)

            slowPrefixSize :: Ptr Word8 -> Builder -> BuildStep a 
                           -> IO (BuildSignal r)
            slowPrefixSize opInner' bInner nextStep = do
                (x, chunks@(SizedChunks lenChunks _), bufLast) <-
                    runCIOSWithLength <=<
                    buildStepToCIOS strategy $ runBuilderWith bInner nextStep
{- At this point the situation is as follows:

|===============Current buffer range (BufferRange op ope)============| ... chunks ... |====Last buffer================|
^ reserved for length ^ inner data--- ^ unfilled ^ reserved for move ^ ---- inner data ----------------- ^ free space ^
|                     |> innerLenCur <|          |                   |> lenChunks    <|> innerLenLast   <|>  brLast  <|
op                startInner       opInner'   opeInner              ope           startLast            opLast    opeLast
                                                                                   fpLast

If the length of the inner builder data is such that its encoding takes more
space than what was reserved, then we move the inner builder data in the
current buffer range to make the encoded length fit exactly. If the encoded
length fits into the reserved space, then we just fill it. We also return the
last buffer, which is very likely partially filled, to our driver for further
filling.
-}
                let Buffer fpLast (BufferRange opLast _opeLast) = bufLast
                    innerLenCur  = opInner' `minusPtr` startInner
                    startLast    = unsafeForeignPtrToPtr fpLast
                    innerLenLast = opLast `minusPtr` startLast
                    innerLen     = fromIntegral innerLenCur +
                                   fromIntegral lenChunks +
                                   fromIntegral innerLenLast
                    -- encoder for length of inner builder data
                    sizeFE'      = mkSizeFE innerLen
                    -- start of inner builder data in current buffer with its
                    -- length encoded before
                    startInner'  = op `plusPtr` size sizeFE'

                -- Move inner builder data to fit encoded length.
                unless (startInner == startInner') $
                    moveBytes startInner' startInner innerLenCur
                -- Encode length of inner builder data
                runF sizeFE' innerLen op
                return $ insertChunksAndBuffer
                    (startInner' `plusPtr` innerLenCur) chunks bufLast (k x)


-- | Run a 'ChunkIOStream' and gather its results and their length.
runCIOSWithLength :: ChunkIOStream a -> IO (a, SizedChunks, Buffer)
runCIOSWithLength =
    go 0 id
  where
    go !l lbsC (Finished buf x) = return (x, SizedChunks l lbsC, buf)
    go !l lbsC (Yield1 bs io)   =
        io >>= go (l + fromIntegral (S.length bs)) (lbsC . L.Chunk bs)
    go !l lbsC (YieldChunks (SizedChunks n lbsC') io) = 
        io >>= go (l + n) (lbsC . lbsC')
