{-# LANGUAGE CPP, BangPatterns, MonoPatBinds, ScopedTypeVariables #-}
-- |
-- Module      : Data.ByteString.Lazy.Builder.BasicEncoding
-- Copyright   : (c) 2010-2011 Simon Meier
--             . (c) 2010      Jasper van der Jeugt
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- An /encoding/ is a conversion function of Haskell values to sequences of
-- bytes. A /bounded encoding/ is an encoding that never results in a sequence
-- longer than some fixed number of bytes. This number of bytes must be
-- independent of the value being encoded. Typical examples of bounded
-- encodings are the big-endian encoding of a 'Word64', which results always
-- in exactly 8 bytes, or the UTF-8 encoding of a 'Char', which results always
-- in less or equal to 4 bytes.
--
-- Typically, encodings are implemented efficiently by allocating a buffer (an
-- array of bytes) and repeatedly executing the following two steps: (1)
-- writing to the buffer until it is full and (2) handing over the filled part
-- to the consumer of the encoded value. Step (1) is where bounded encodings
-- are used. We must use a bounded encoding, as we must check that there is
-- enough free space /before/ actually writing to the buffer.
--
-- In term of expressivity, it would be sufficient to construct all encodings
-- from the single bounded encoding that encodes a 'Word8' as-is. However,
-- this is not sufficient in terms of efficiency. It results in unnecessary
-- buffer-full checks and it complicates the program-flow for writing to the
-- buffer, as buffer-full checks are interleaved with analyzing the value to be
-- encoded (e.g., think about the program-flow for UTF-8 encoding). This has a
-- significant effect on overall encoding performance, as encoding primitive
-- Haskell values such as 'Word8's or 'Char's lies at the heart of every
-- encoding implementation.
--
-- The bounded 'Encoding's provided by this module remove this performance
-- problem. Intuitively, they consist of a tuple of the bound on the maximal
-- number of bytes written and the actual implementation of the encoding as a
-- function that modifies a mutable buffer. Hence when executing a bounded
-- 'Encoding', the buffer-full check can be done once before the actual writing
-- to the buffer. The provided 'Encoding's also take care to implement the
-- actual writing to the buffer efficiently. Moreover, combinators are
-- provided to construct new bounded encodings from the provided ones. 
--
-- A typical example for using the combinators is a bounded 'Encoding' that
-- combines escaping the ' and \\ characters with UTF-8 encoding. More
-- precisely, the escaping to be done is the one implemented by the following
-- @escape@ function.
--
-- > escape :: Char -> [Char]
-- > escape '\'' = "\\'"
-- > escape '\\' = "\\\\"
-- > escape c    = [c]
--
-- The bounded 'Encoding' that combines this escaping with UTF-8 encoding is
-- the following.
--
-- > import Data.ByteString.Lazy.Builder.BasicEncoding.Utf8 (char)
-- >
-- > {-# INLINE escapeChar #-}
-- > escapeUtf8 :: BoundedEncoding Char
-- > escapeUtf8 = 
-- >     encodeIf ('\'' ==) (char <#> char #. const ('\\','\'')) $
-- >     encodeIf ('\\' ==) (char <#> char #. const ('\\','\\')) $
-- >     char
--
-- The definition of 'escapeUtf8' is more complicated than 'escape', because
-- the combinators ('encodeIf', 'encodePair', '#.', and 'char') used in
-- 'escapeChar' compute both the bound on the maximal number of bytes written
-- (8 for 'escapeUtf8') as well as the low-level buffer manipulation required
-- to implement the encoding. Bounded 'Encoding's should always be inlined.
-- Otherwise, the compiler cannot compute the bound on the maximal number of
-- bytes written at compile-time. Without inlinining, it would also fail to
-- optimize the constant encoding of the escape characters in the above
-- example. Functions that execute bounded 'Encoding's also perform
-- suboptimally, if the definition of the bounded 'Encoding' is not inlined.
-- Therefore we add an 'INLINE' pragma to 'escapeUtf8'.
--
-- Currently, the only library that executes bounded 'Encoding's is the
-- 'bytestring' library (<http://hackage.haskell.org/package/bytestring>). It
-- uses bounded 'Encoding's to implement most of its lazy bytestring builders.
-- Executing a bounded encoding should be done using the corresponding
-- functions in the lazy bytestring builder 'Extras' module.
--
-- *TODO: Merge with explanation/example below*
--
-- Bounded 'E.Encoding's abstract encodings of Haskell values that can be implemented by
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
-- > filter p = toLazyByteString . encodeLazyByteStringWithB write
-- >   where
-- >     write = E.encodeIf p E.word8 E.emptyEncoding
-- >
-- > map :: (Word8 -> Word8) -> ByteString -> ByteString
-- > map f = toLazyByteString . encodeLazyByteStringWithB (E.word8 E.#. f)
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
-- > import Data.ByteString.Lazy.Builder.Extras     -- assume these three
-- > import Codec.Bounded.Encoding                  -- imports are present
-- >        ( BoundedEncoding, encodeIf, (<#>), (#.) )
-- > import Data.ByteString.Lazy.Builder.BasicEncoding.Utf8 (char)  
-- > 
-- > renderString :: String -> Builder
-- > renderString cs = 
-- >     charUtf8 '"' <> encodeListWithB escapedUtf8 cs <> charUtf8 '"'
-- >   where
-- >     escapedUtf8 :: BoundedEncoding Char
-- >     escapedUtf8 = 
-- >       encodeIf (== '\\') (char <#> char #. const ('\\', '\\')) $
-- >       encodeIf (== '\"') (char <#> char #. const ('\\', '\"')) $
-- >       char
--
-- This 'Builder' considers a buffer with less than 8 free bytes as full. As
-- all functions are inlined, the compiler is able to optimize the constant
-- 'E.Encoding's as two sequential 'poke's. Compared to the first implementation of
-- 'renderString' this implementation is 1.7x faster.
--
module Data.ByteString.Lazy.Builder.BasicEncoding (

  -- * Fixed-size encodings
    FixedEncoding

  , encodeWithF 
  , encodeListWithF 
  , encodeUnfoldrWithF 

  , encodeByteStringWithF 
  , encodeLazyByteStringWithF 

  , toB

  -- ** Combinators
  , emptyF
  , pairF
  , (>*<)
  , contramapF
  , (>$<)

  -- * Bounded-size encodings

  , BoundedEncoding

  , encodeWithB
  , encodeListWithB
  , encodeUnfoldrWithB
  
  , encodeByteStringWithB
  , encodeLazyByteStringWithB

  -- ** Combinators
  , emptyB
  , pairB
  , eitherB
  , ifB
  , contramapB

  -- * Chunked Encoding
  , encodeChunked
  , encodeWithSize

  -- * Standard encodings of Haskell values

  -- ** Binary
  , module Data.ByteString.Lazy.Builder.BasicEncoding.Binary

  -- ** ASCII
  , module Data.ByteString.Lazy.Builder.BasicEncoding.ASCII

  -- ** UTF-8
  , charUtf8

  -- ** Latin-1
  , char8

  -- * Benchmarking
  -- , benchIntEncoding

  -- * Debugging
  -- | Note that the following two functions are intended for debugging use
  -- only. They are not efficient. Bounded encodings are efficently executed
  -- using the lazy bytestring builders provided in the
  -- 'Data.ByteString.Lazy.Builder.Extras' module of the 'bytestring' library.
  -- , evalEncoding
  -- , showEncoding

  , evalF
  , evalB

  , showF
  , showB

  ) where

import Data.ByteString.Lazy.Builder.Internal

import qualified Data.ByteString               as S
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy.Internal as L

import Data.Monoid
import Data.Char (chr, ord)
import Control.Monad ((<=<))

-- import Codec.Bounded.Encoding.Internal.Test
-- import Codec.Bounded.Encoding.Bench
import Data.ByteString.Lazy.Builder.BasicEncoding.Internal
import Data.ByteString.Lazy.Builder.BasicEncoding.Binary
import Data.ByteString.Lazy.Builder.BasicEncoding.ASCII

import Foreign


------------------------------------------------------------------------------
-- Creating Builders from bounded encodings
------------------------------------------------------------------------------

encodeWithF :: FixedEncoding a -> (a -> Builder)
encodeWithF = encodeWithB . toB

encodeListWithF :: FixedEncoding a -> ([a] -> Builder)
encodeListWithF = encodeListWithB . toB

encodeUnfoldrWithF :: FixedEncoding b -> (a -> Maybe (b, a)) -> a -> Builder
encodeUnfoldrWithF = encodeUnfoldrWithB . toB

encodeByteStringWithF :: FixedEncoding Word8 -> (S.ByteString -> Builder)
encodeByteStringWithF = encodeByteStringWithB . toB

encodeLazyByteStringWithF :: FixedEncoding Word8 -> (L.ByteString -> Builder)
encodeLazyByteStringWithF = encodeLazyByteStringWithB . toB

-- IMPLEMENTATION NOTE: Sadly, 'encodeListWith' cannot be used for foldr/build
-- fusion. Its performance relies on hoisting several variables out of the
-- inner loop.  That's not possible when writing 'encodeListWith' as a 'foldr'.
-- If we had stream fusion for lists, then we could fuse 'encodeListWith', as
-- 'encodeWithStream' can keep control over the execution.


-- | Create a 'Builder' that encodes values with the given 'Encoding'.
--
-- We rewrite consecutive uses of 'encodeWith' such that the bound-checks are
-- fused. For example,
--
-- > encodeWithB (word32 c1) `mappend` encodeWithB (word32 c2)
--
-- is rewritten such that the resulting 'Builder' checks only once, if ther are
-- at 8 free bytes, instead of checking twice, if there are 4 free bytes. This
-- optimization is not observationally equivalent in a strict sense, as it
-- influences the boundaries of the generated chunks. However, for a user of
-- this library it is observationally equivalent, as chunk boundaries of a lazy
-- 'L.ByteString' can only be observed through the internal interface.
-- Morevoer, we expect that all 'Encoding's write much fewer than 4kb (the
-- default short buffer size). Hence, it is safe to ignore the additional
-- memory spilled due to the more agressive buffer wrapping introduced by this
-- optimization.
--
{-# INLINE[1] encodeWithB #-}
encodeWithB :: BoundedEncoding a -> (a -> Builder)
encodeWithB w = 
    mkBuilder
  where
    bound = sizeBound w
    mkBuilder x = builder step
      where
        step k (BufferRange op ope)
          | op `plusPtr` bound <= ope = do
              op' <- runB w x op
              let !br' = BufferRange op' ope
              k br'
          | otherwise = return $ bufferFull bound op (step k)

{-# RULES 
   "append/encodeWith" forall w1 w2 x1 x2.
       append (encodeWithB w1 x1) (encodeWithB w2 x2) 
     = encodeWithB (pairB w1 w2) (x1, x2) 
  #-}

-- TODO: The same rules for 'putBuilder (..) >> putBuilder (..)'

-- | Create a 'Builder' that encodes a list of values consecutively using an
-- 'Encoding'. This function is more efficient than the canonical
--
-- > mconcat . map (encodeWithB w)
--
-- or
--
-- > foldMap (encodeWithB w)
--
-- because it moves several variables out of the inner loop. 
{-# INLINE encodeListWithB #-}
encodeListWithB :: BoundedEncoding a -> [a] -> Builder
encodeListWithB w = 
    makeBuilder
  where
    bound = sizeBound w
    makeBuilder xs0 = builder $ step xs0
      where
        step xs1 k !(BufferRange op0 ope0) = go xs1 op0
          where
            go [] !op = do
               let !br' = BufferRange op ope0
               k br'

            go xs@(x':xs') !op
              | op `plusPtr` bound <= ope0 = do
                  !op' <- runB w x' op
                  go xs' op'
             | otherwise = return $ bufferFull bound op (step xs k)

-- TODO: Add 'foldMap/encodeWith' its variants
-- TODO: Ensure rewriting 'encodeWithB w . f = encodeWithB (w #. f)'

-- | Create a 'Builder' that encodes a sequence generated from a seed value
-- using an 'Encoding'.
{-# INLINE encodeUnfoldrWithB #-}
encodeUnfoldrWithB :: BoundedEncoding b -> (a -> Maybe (b, a)) -> a -> Builder
encodeUnfoldrWithB w = 
    makeBuilder
  where
    bound = sizeBound w
    makeBuilder f x0 = builder $ step x0
      where
        step x1 !k = fill x1
          where
            fill x !(BufferRange pf0 pe0) = go (f x) pf0
              where
                go !Nothing        !pf = do
                    let !br' = BufferRange pf pe0
                    k br'
                go !(Just (y, x')) !pf
                  | pf `plusPtr` bound <= pe0 = do
                      !pf' <- runB w y pf
                      go (f x') pf'
                  | otherwise = return $ bufferFull bound pf $ 
                      \(BufferRange pfNew peNew) -> do 
                          !pfNew' <- runB w y pfNew
                          fill x' (BufferRange pfNew' peNew)

-- | Create a 'Builder' that encodes each 'Word8' of a strict 'S.ByteString'
-- using an 'Encoding'. For example, we can write a 'Builder' that filters
-- a strict 'S.ByteString' as follows.
--
-- > import Codec.Bounded.Encoding as E (encodeIf, word8, encodeNothing)
--
-- > filterBS p = E.encodeIf p E.word8 E.encodeNothing
--
{-# INLINE encodeByteStringWithB #-}
encodeByteStringWithB :: BoundedEncoding Word8 -> S.ByteString -> Builder
encodeByteStringWithB w =
    \bs -> builder $ step bs
  where
    bound = sizeBound w
    step (S.PS ifp ioff isize) !k = 
        goBS (unsafeForeignPtrToPtr ifp `plusPtr` ioff)
      where
        !ipe = unsafeForeignPtrToPtr ifp `plusPtr` (ioff + isize)
        goBS !ip0 !br@(BufferRange op0 ope)
          | ip0 >= ipe = do 
              touchForeignPtr ifp -- input buffer consumed
              k br

          | op0 `plusPtr` bound < ope = 
              goPartial (ip0 `plusPtr` min outRemaining inpRemaining)

          | otherwise  = return $ bufferFull bound op0 (goBS ip0) 
          where
            outRemaining = (ope `minusPtr` op0) `div` bound
            inpRemaining = ipe `minusPtr` ip0 

            goPartial !ipeTmp = go ip0 op0
              where
                go !ip !op
                  | ip < ipeTmp = do
                      x   <- peek ip
                      op' <- runB w x op
                      go (ip `plusPtr` 1) op'
                  | otherwise =
                      goBS ip (BufferRange op ope)

-- | Chunk-wise application of 'encodeByteStringWith'.
{-# INLINE encodeLazyByteStringWithB #-}
encodeLazyByteStringWithB :: BoundedEncoding Word8 -> L.ByteString -> Builder
encodeLazyByteStringWithB w = 
    L.foldrChunks (\x b -> encodeByteStringWithB w x `mappend` b) mempty

------------------------------------------------------------------------------
-- ASCII encoding
------------------------------------------------------------------------------

-- | Encode a 'Char' as its Unicode codepoint modulo 256. For codepoints less
-- than 128, this coincides with the ASCII encoding.
char8 :: FixedEncoding Char 
char8 = (fromIntegral . ord) >$< word8

------------------------------------------------------------------------------
-- Chunked Encoding Transformer
------------------------------------------------------------------------------

{-# INLINE encodeChunked #-}
encodeChunked
    :: Int                           -- ^ Minimal free-size
    -> (Int64 -> FixedEncoding Int64)   
    -- ^ Given a sizeBound on the maximal encodable size this function must return
    -- a fixed-size encoding for encoding all smaller size.
    -> (BoundedEncoding Int64)
    -- ^ An encoding for terminating a chunk of the given size.
    -> Builder
    -- ^ Inner Builder to transform
    -> Builder
    -- ^ 'Put' with chunked encoding.
encodeChunked minFree mkBeforeFE afterBE =
    fromPut . putChunked minFree mkBeforeFE afterBE . putBuilder

{-# INLINE putChunked #-}
putChunked
    :: Int                           -- ^ Minimal free-size
    -> (Int64 -> FixedEncoding Int64)   
    -- ^ Given a sizeBound on the maximal encodable size this function must return
    -- a fixed-size encoding for encoding all smaller size.
    -> (BoundedEncoding Int64)
    -- ^ Encoding a directly inserted chunk.
    -> Put a
    -- ^ Inner Put to transform
    -> Put a
    -- ^ 'Put' with chunked encoding.
putChunked minFree0 mkBeforeFE afterBE p =
    put encodingStep
  where
    minFree      = max 1 minFree0   -- sanitize

    encodingStep k = 
        fill (runPut p)
      where
        fill innerStep !(BufferRange op ope)
          | outRemaining < minBufferSize = 
              return $! bufferFull minBufferSize op (fill innerStep)
          | otherwise = do
              fillWithBuildStep innerStep doneH fullH insertChunksH brInner
          where
            outRemaining   = ope `minusPtr` op
            beforeFE       = mkBeforeFE $ fromIntegral outRemaining
            reservedBefore = size beforeFE
            reservedAfter  = sizeBound afterBE
            reserved       = reservedBefore + reservedAfter
            minBufferSize  = minFree + reserved
           
            opInner        = op  `plusPtr` reservedBefore
            opeInner       = ope `plusPtr` (-reservedAfter)
            brInner        = BufferRange opInner opeInner

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
                return $! bufferFull 
                  (max minBufferSize (minSize + reserved))
                  op'
                  (fill nextInnerStep)  

            insertChunksH opInner' n lbsC nextInnerStep
              | n == 0 = do                      -- flush
                  op' <- wrapChunk opInner'
                  return $! insertChunks op' 0 id (fill nextInnerStep)

              | otherwise = do                   -- insert non-empty bytestring
                  op' <- wrapChunk opInner'
                  let !br' = BufferRange op' ope
                  runBuilderWith chunkB (fill nextInnerStep) br'
              where
                chunkB =
                  encodeWithF (mkBeforeFE n) n `mappend`
                  lazyByteStringC n lbsC       `mappend`
                  encodeWithB afterBE n
  

-- | /Heavy inlining./ Prefix a 'Put a' with its length.
--
-- This function is optimized for streaming use. It tries to prefix the length
-- without copying the output. This is achieved by reserving space for the
-- maximum length to be encoded. This succeeds if the output is smaller than
-- the current free buffer size, which is guaranteed to be at least @8kb@.
--
-- If the output does not fit into the current free buffer size, 
-- the method falls back to encoding the data to a separate lazy bytestring,
-- computing the size, and encoding the size before inserting the chunks of
-- the separate lazy bytestring.
--
-- Note that the generated chunks might hold on to a lot of unused memory. If
-- you need to hold on to the generated chunks, then you should copy their
-- content.
{-# INLINE encodeWithSize #-}
encodeWithSize
    :: (Int64 -> FixedEncoding Int64)   
    -- ^ Given a bound on the maximal size to encode, this function must return
    -- a fixed-size encoding for all smaller sizes.
    -> Builder
    -- ^ 'Put' to prefix with the length of its sequence of bytes.
    -> Builder
encodeWithSize mkSizeFE = 
    fromPut . putWithSize mkSizeFE . putBuilder

-- | Prefix a 'Put' with the size of its written data.
{-# INLINE putWithSize #-}
putWithSize 
    :: forall a.
       (Int64 -> FixedEncoding Int64)   
    -- ^ Encoding the size for the fallback case.
    -> Put a
    -- ^ 'Put' to prefix with the length of its sequence of bytes.
    -> Put a
putWithSize mkSizeFE innerP =
    put $ encodingStep
  where
    -- | The minimal free-size must be at least the small chunk-size to ensure
    -- that the slow-mode inserts large enough chunks.
    minFree = L.smallChunkSize

    encodingStep :: (forall r. (a -> BuildStep r) -> BuildStep r)
    encodingStep k = 
        fill (runPut innerP)
      where
        fill :: BuildStep a -> BufferRange -> IO (BuildSignal r)
        fill innerStep !br@(BufferRange op ope)
          | outRemaining < minBufferSize = 
              return $! bufferFull minBufferSize op (fill innerStep)
          | otherwise = do
              fillWithBuildStep innerStep doneH fullH insertChunksH brInner
          where
            outRemaining  = ope `minusPtr` op
            sizeFE        = mkSizeFE $ fromIntegral outRemaining
            reserved      = size sizeFE
            minBufferSize = minFree + reserved
           
            opInner       = op  `plusPtr` reserved
            brInner       = BufferRange opInner ope

            fastPrefixSize :: Ptr Word8 -> IO (Ptr Word8)
            fastPrefixSize !opInner'
              | innerSize == 0 = do runB (toB $ mkSizeFE 0) 0          op
              | otherwise      = do runB (toB $ sizeFE)      innerSize op
              where
                innerSize = fromIntegral $ opInner' `minusPtr` opInner

            slowPrefixSize :: Ptr Word8 -> Builder -> BuildStep a -> IO (BuildSignal r)
            slowPrefixSize opInner' replaySignal nextStep = do
                (x, lbsC, l) <- toLBS $ runBuilderWith bInner nextStep
                -- TODO: We could get rid of one more chunk-boundary
                -- when encoding large messages by reserving space for the 
                -- size and modifying the returned bytestring afterwards.
                runBuilderWith (encodeWithF (mkSizeFE l) l `mappend` 
                                -- inserting the continuation is crucial to
                                -- avoid quadratic runtime when nesting
                                -- 'encodeWithSize'.
                                lazyByteStringC l lbsC)
                               (k x) br
              where
                toLBS  = runCIOSWithLength <=< buildStepToCIOSUntrimmed
                -- Note that we there's no 'unsafePerformIO' involved at this
                -- level. This is crucial to ensure a total ordering of all
                -- actions and, therefore, the execution of the 'bytesCopy'
                -- builder before returning a signal to the driver.
                bInner = bytesCopy (BufferRange opInner opInner') `mappend` 
                         replaySignal

            doneH :: Ptr Word8 -> a -> IO (BuildSignal r)
            doneH opInner' x = do
                op' <- fastPrefixSize opInner'
                let !br' = BufferRange op' ope
                k x br'

            fullH :: Ptr Word8 -> Int -> BuildStep a -> IO (BuildSignal r)
            fullH opInner' minSize nextInnerStep =
                slowPrefixSize opInner' (ensureFree minSize) nextInnerStep

            insertChunksH :: Ptr Word8 -> Int64 -> LazyByteStringC 
                          -> BuildStep a -> IO (BuildSignal r)
            insertChunksH opInner' n lbsC nextInnerStep =
                slowPrefixSize opInner' (lazyByteStringC n lbsC) nextInnerStep


-- | Run a 'ChunkIOStream' and gather its results and their length.
runCIOSWithLength :: ChunkIOStream a -> IO (a, LazyByteStringC, Int64)
runCIOSWithLength = 
    go 0 id
  where
    go !l lbsC (Finished x)        = return (x, lbsC, l)
    go !l lbsC (YieldC n lbsC' io) = io >>= go (l + n) (lbsC . lbsC')
    go !l lbsC (Yield1 bs io)      = 
        io >>= go (l + fromIntegral (S.length bs)) (lbsC . L.Chunk bs)

------------------------------------------------------------------------------
-- Debugging encodings
------------------------------------------------------------------------------

-- TODO: Port testing infrastructure.



-- | Encode a 'Char' using the UTF-8 encoding.
--
{-# INLINE charUtf8 #-}
charUtf8 :: BoundedEncoding Char
charUtf8 = boundedEncoding 4 (encodeCharUtf8 f1 f2 f3 f4)
  where
    pokeN n io op = io op >> return (op `plusPtr` n)

    f1 x1          = pokeN 1 $ \op -> do pokeByteOff op 0 x1

    f2 x1 x2       = pokeN 2 $ \op -> do pokeByteOff op 0 x1
                                         pokeByteOff op 1 x2
                   
    f3 x1 x2 x3    = pokeN 3 $ \op -> do pokeByteOff op 0 x1
                                         pokeByteOff op 1 x2
                                         pokeByteOff op 2 x3

    f4 x1 x2 x3 x4 = pokeN 4 $ \op -> do pokeByteOff op 0 x1
                                         pokeByteOff op 1 x2
                                         pokeByteOff op 2 x3
                                         pokeByteOff op 3 x4

-- | Encode a Unicode character to another datatype, using UTF-8. This function
-- acts as an abstract way of encoding characters, as it is unaware of what
-- needs to happen with the resulting bytes: you have to specify functions to
-- deal with those.
--
encodeCharUtf8 :: (Word8 -> a)                             -- ^ 1-byte UTF-8
               -> (Word8 -> Word8 -> a)                    -- ^ 2-byte UTF-8
               -> (Word8 -> Word8 -> Word8 -> a)           -- ^ 3-byte UTF-8
               -> (Word8 -> Word8 -> Word8 -> Word8 -> a)  -- ^ 4-byte UTF-8
               -> Char                                     -- ^ Input 'Char'
               -> a                                        -- ^ Result
encodeCharUtf8 f1 f2 f3 f4 c = case ord c of
    x | x <= 0x7F -> f1 $ fromIntegral x
      | x <= 0x07FF ->
           let x1 = fromIntegral $ (x `shiftR` 6) + 0xC0
               x2 = fromIntegral $ (x .&. 0x3F)   + 0x80
           in f2 x1 x2
      | x <= 0xFFFF ->
           let x1 = fromIntegral $ (x `shiftR` 12) + 0xE0
               x2 = fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80
               x3 = fromIntegral $ (x .&. 0x3F) + 0x80
           in f3 x1 x2 x3
      | otherwise ->
           let x1 = fromIntegral $ (x `shiftR` 18) + 0xF0
               x2 = fromIntegral $ ((x `shiftR` 12) .&. 0x3F) + 0x80
               x3 = fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80
               x4 = fromIntegral $ (x .&. 0x3F) + 0x80
           in f4 x1 x2 x3 x4
{-# INLINE encodeCharUtf8 #-}


------------------------------------------------------------------------------
-- Debugging encodings
------------------------------------------------------------------------------

evalF :: FixedEncoding a -> a -> [Word8]
evalF fe = S.unpack . S.unsafeCreate (size fe) . runF fe

evalB :: BoundedEncoding a -> a -> [Word8]
evalB be x = S.unpack $ unsafePerformIO $ 
    S.createAndTrim (sizeBound be) $ \op -> do
        op' <- runB be x op
        return (op' `minusPtr` op)

showF :: FixedEncoding a -> a -> [Char]
showF fe = map (chr . fromIntegral) . evalF fe

showB :: BoundedEncoding a -> a -> [Char]
showB be = map (chr . fromIntegral) . evalB be


