{-# LANGUAGE CPP, BangPatterns, MonoPatBinds, ScopedTypeVariables #-}
-- |
-- Module      : Data.ByteString.Lazy.Builder.BoundedEncoding
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
-- > import Data.ByteString.Lazy.Builder.BoundedEncoding.Utf8 (char)
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
-- > import Data.ByteString.Lazy.Builder.BoundedEncoding.Utf8 (char)  
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
module Data.ByteString.Lazy.Builder.BoundedEncoding (

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

  -- ** UTF-8 encoding
  --
  -- | UTF-8 encoding of 'Char's and numbers in decimal and hexadecimal formats
  -- is provided by the "Data.ByteString.Lazy.Builder.BoundedEncoding.Utf8" module.

  -- ** ASCII encoding
  , char8

  -- ** Binary encoding
  , int8
  , word8

  -- *** Big-endian
  , int16BE
  , int32BE
  , int64BE

  , word16BE
  , word32BE
  , word64BE

  , floatBE
  , doubleBE

  -- *** Little-endian
  , int16LE
  , int32LE
  , int64LE

  , word16LE
  , word32LE
  , word64LE

  , floatLE
  , doubleLE

  -- *** Base 128 Variable-length
  , int8Var
  , int16Var
  , int32Var
  , int64Var
  , intVar

  , word8Var
  , word16Var
  , word32Var
  , word64Var
  , wordVar

  , int8VarSigned
  , int16VarSigned
  , int32VarSigned
  , int64VarSigned
  , intVarSigned

  , intVarFixed

  -- *** Non-portable, host-dependent
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

  -- * Benchmarking
  -- , benchIntEncoding

  -- * Debugging
  -- | Note that the following two functions are intended for debugging use
  -- only. They are not efficient. Bounded encodings are efficently executed
  -- using the lazy bytestring builders provided in the
  -- 'Data.ByteString.Lazy.Builder.Extras' module of the 'bytestring' library.
  -- , evalEncoding
  -- , showEncoding

  ) where

import Data.ByteString.Lazy.Builder.Internal

import qualified Data.ByteString               as S
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy.Internal as L

import Data.Monoid
import Data.Char (ord)
import Control.Monad ((<=<))

import Codec.Bounded.Encoding.Internal
-- import Codec.Bounded.Encoding.Word
-- import Codec.Bounded.Encoding.Int
-- import Codec.Bounded.Encoding.Floating

-- import Codec.Bounded.Encoding.Internal.Test
-- import Codec.Bounded.Encoding.Bench
import Codec.Bounded.Encoding.Internal.UncheckedShifts
import Data.ByteString.Lazy.Builder.BoundedEncoding.Internal.Floating

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
-- Binary encoding
------------------------------------------------------------------------------

-- Word encodings
-----------------

-- | Encoding single unsigned bytes as-is.
--
{-# INLINE word8 #-}
word8 :: FixedEncoding Word8
word8 = storableToF

--
-- We rely on the fromIntegral to do the right masking for us.
-- The inlining here is critical, and can be worth 4x performance
--

-- | Encoding 'Word16's in big endian format.
{-# INLINE word16BE #-}
word16BE :: FixedEncoding Word16
#ifdef WORD_BIGENDIAN
word16BE = word16Host
#else
word16BE = fixedEncoding 2 $ \w p -> do
    poke p               (fromIntegral (shiftr_w16 w 8) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (w)              :: Word8)
#endif

-- | Encoding 'Word16's in little endian format.
{-# INLINE word16LE #-}
word16LE :: FixedEncoding Word16
#ifdef WORD_BIGENDIAN
word16LE = fixedEncoding 2 $ \w p -> do
    poke p               (fromIntegral (w)              :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w16 w 8) :: Word8)
#else
word16LE = word16Host
#endif

-- | Encoding 'Word32's in big endian format.
{-# INLINE word32BE #-}
word32BE :: FixedEncoding Word32
#ifdef WORD_BIGENDIAN
word32BE = word32Host
#else
word32BE = fixedEncoding 4 $ \w p -> do
    poke p               (fromIntegral (shiftr_w32 w 24) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (w)               :: Word8)
#endif

-- | Encoding 'Word32's in little endian format.
{-# INLINE word32LE #-}
word32LE :: FixedEncoding Word32
#ifdef WORD_BIGENDIAN
word32LE = fixedEncoding 4 $ \w p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w32 w 24) :: Word8)
#else
word32LE = word32Host
#endif

-- on a little endian machine:
-- word32LE w32 = fixedEncoding 4 (\w p -> poke (castPtr p) w32)

-- | Encoding 'Word64's in big endian format.
{-# INLINE word64BE #-}
word64BE :: FixedEncoding Word64
#ifdef WORD_BIGENDIAN
word64BE = word64Host
#else
#if WORD_SIZE_IN_BITS < 64
--
-- To avoid expensive 64 bit shifts on 32 bit machines, we cast to
-- Word32, and write that
--
word64BE =
    fixedEncoding 8 $ \w p -> do
        let a = fromIntegral (shiftr_w64 w 32) :: Word32
            b = fromIntegral w                 :: Word32
        poke p               (fromIntegral (shiftr_w32 a 24) :: Word8)
        poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 a 16) :: Word8)
        poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 a  8) :: Word8)
        poke (p `plusPtr` 3) (fromIntegral (a)               :: Word8)
        poke (p `plusPtr` 4) (fromIntegral (shiftr_w32 b 24) :: Word8)
        poke (p `plusPtr` 5) (fromIntegral (shiftr_w32 b 16) :: Word8)
        poke (p `plusPtr` 6) (fromIntegral (shiftr_w32 b  8) :: Word8)
        poke (p `plusPtr` 7) (fromIntegral (b)               :: Word8)
#else
word64BE = fixedEncoding 8 $ \w p -> do
    poke p               (fromIntegral (shiftr_w64 w 56) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (w)               :: Word8)
#endif
#endif

-- | Encoding 'Word64's in little endian format.
{-# INLINE word64LE #-}
word64LE :: FixedEncoding Word64
#ifdef WORD_BIGENDIAN
#if WORD_SIZE_IN_BITS < 64
word64LE =
    fixedEncoding 8 $ \w p -> do
        let b = fromIntegral (shiftr_w64 w 32) :: Word32
            a = fromIntegral w                 :: Word32
        poke (p)             (fromIntegral (a)               :: Word8)
        poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 a  8) :: Word8)
        poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 a 16) :: Word8)
        poke (p `plusPtr` 3) (fromIntegral (shiftr_w32 a 24) :: Word8)
        poke (p `plusPtr` 4) (fromIntegral (b)               :: Word8)
        poke (p `plusPtr` 5) (fromIntegral (shiftr_w32 b  8) :: Word8)
        poke (p `plusPtr` 6) (fromIntegral (shiftr_w32 b 16) :: Word8)
        poke (p `plusPtr` 7) (fromIntegral (shiftr_w32 b 24) :: Word8)
#else
word64LE = fixedEncoding 8 $ \w p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (shiftr_w64 w 56) :: Word8)
#endif
#else
word64LE = word64Host
#endif


-- | Encode a single native machine 'Word'. The 'Word's is encoded in host order,
-- host endian form, for the machine you are on. On a 64 bit machine the 'Word'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values encoded this way
-- are not portable to different endian or word sized machines, without
-- conversion.
--
{-# INLINE wordHost #-}
wordHost :: FixedEncoding Word
wordHost = storableToF

-- | Encoding 'Word16's in native host order and host endianness.
{-# INLINE word16Host #-}
word16Host :: FixedEncoding Word16
word16Host = storableToF

-- | Encoding 'Word32's in native host order and host endianness.
{-# INLINE word32Host #-}
word32Host :: FixedEncoding Word32
word32Host = storableToF

-- | Encoding 'Word64's in native host order and host endianness.
{-# INLINE word64Host #-}
word64Host :: FixedEncoding Word64
word64Host = storableToF


------------------------------------------------------------------------------
-- Int encodings
------------------------------------------------------------------------------
--
-- We rely on 'fromIntegral' to do a loss-less conversion to the corresponding
-- 'Word' type
-- 
------------------------------------------------------------------------------

-- | Encoding single signed bytes as-is.
--
{-# INLINE int8 #-}
int8 :: FixedEncoding Int8
int8 = fromIntegral >$< word8

-- | Encoding 'Int16's in big endian format.
{-# INLINE int16BE #-}
int16BE :: FixedEncoding Int16
int16BE = fromIntegral >$< word16BE

-- | Encoding 'Int16's in little endian format.
{-# INLINE int16LE #-}
int16LE :: FixedEncoding Int16
int16LE = fromIntegral >$< word16LE

-- | Encoding 'Int32's in big endian format.
{-# INLINE int32BE #-}
int32BE :: FixedEncoding Int32
int32BE = fromIntegral >$< word32BE

-- | Encoding 'Int32's in little endian format.
{-# INLINE int32LE #-}
int32LE :: FixedEncoding Int32
int32LE = fromIntegral >$< word32LE

-- | Encoding 'Int64's in big endian format.
{-# INLINE int64BE #-}
int64BE :: FixedEncoding Int64
int64BE = fromIntegral >$< word64BE

-- | Encoding 'Int64's in little endian format.
{-# INLINE int64LE #-}
int64LE :: FixedEncoding Int64
int64LE = fromIntegral >$< word64LE


-- TODO: Ensure that they are safe on architectures where an unaligned write is
-- an error.

-- | Encode a single native machine 'Int'. The 'Int's is encoded in host order,
-- host endian form, for the machine you are on. On a 64 bit machine the 'Int'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values encoded this way
-- are not portable to different endian or integer sized machines, without
-- conversion.
--
{-# INLINE intHost #-}
intHost :: FixedEncoding Int
intHost = storableToF

-- | Encoding 'Int16's in native host order and host endianness.
{-# INLINE int16Host #-}
int16Host :: FixedEncoding Int16
int16Host = storableToF

-- | Encoding 'Int32's in native host order and host endianness.
{-# INLINE int32Host #-}
int32Host :: FixedEncoding Int32
int32Host = storableToF

-- | Encoding 'Int64's in native host order and host endianness.
{-# INLINE int64Host #-}
int64Host :: FixedEncoding Int64
int64Host = storableToF

-- IEEE Floating Point Numbers
------------------------------

-- | Encode a 'Float' in big endian format.
{-# INLINE floatBE #-}
floatBE :: FixedEncoding Float
floatBE = coerceFloatToWord32 >$< word32BE 

-- | Encode a 'Float' in little endian format.
{-# INLINE floatLE #-}
floatLE :: FixedEncoding Float
floatLE = coerceFloatToWord32 >$< word32LE

-- | Encode a 'Double' in big endian format.
{-# INLINE doubleBE #-}
doubleBE :: FixedEncoding Double
doubleBE = coerceDoubleToWord64 >$< word64BE 

-- | Encode a 'Double' in little endian format.
{-# INLINE doubleLE #-}
doubleLE :: FixedEncoding Double
doubleLE = coerceDoubleToWord64 >$< word64LE 


-- | Encode a 'Float' in native host order and host endianness. Values written
-- this way are not portable to different endian machines, without conversion.
--
{-# INLINE floatHost #-}
floatHost :: FixedEncoding Float
floatHost = storableToF

-- | Encode a 'Double' in native host order and host endianness.
{-# INLINE doubleHost #-}
doubleHost :: FixedEncoding Double
doubleHost = storableToF



------------------------------------------------------------------------------
-- ASCII encoding
------------------------------------------------------------------------------

-- | Encode a 'Char' as its Unicode codepoint modulo 256. For codepoints less
-- than 128, this coincides with the ASCII encoding.
char8 :: FixedEncoding Char 
char8 = (fromIntegral . ord) >$< word8

------------------------------------------------------------------------------
-- Base-128 Variable-Length Encodings
------------------------------------------------------------------------------

encodeBase128 
    :: forall a b. (Integral a, Bits a, Storable b, Integral b, Num b) 
    => (a -> Int -> a) -> BoundedEncoding b
encodeBase128 shiftr = 
    -- We add 6 because we require the result of (`div` 7) to be rounded up.
    boundedEncoding ((8 * sizeOf (undefined :: b) + 6) `div` 7) (io . fromIntegral)
  where
    io !x !op
      | x' == 0   = do poke8 (x .&. 0x7f)
                       return $! op `plusPtr` 1
      | otherwise = do poke8 ((x .&. 0x7f) .|. 0x80)
                       io x' (op `plusPtr` 1)
      where
        x'    = x `shiftr` 7
        poke8 = poke op . fromIntegral

{-# INLINE word8Var #-}
word8Var :: BoundedEncoding Word8
word8Var = encodeBase128 shiftr_w

{-# INLINE word16Var #-}
word16Var :: BoundedEncoding Word16
word16Var = encodeBase128 shiftr_w

{-# INLINE word32Var #-}
word32Var :: BoundedEncoding Word32
word32Var = encodeBase128 shiftr_w32

{-# INLINE word64Var #-}
word64Var :: BoundedEncoding Word64
word64Var = encodeBase128 shiftr_w64

{-# INLINE wordVar #-}
wordVar :: BoundedEncoding Word
wordVar = encodeBase128 shiftr_w


{-# INLINE int8Var #-}
int8Var :: BoundedEncoding Int8
int8Var = fromIntegral >$< word8Var

{-# INLINE int16Var #-}
int16Var :: BoundedEncoding Int16
int16Var = fromIntegral >$< word16Var

{-# INLINE int32Var #-}
int32Var :: BoundedEncoding Int32
int32Var = fromIntegral >$< word32Var

{-# INLINE int64Var #-}
int64Var :: BoundedEncoding Int64
int64Var = fromIntegral >$< word64Var

{-# INLINE intVar #-}
intVar :: BoundedEncoding Int
intVar = fromIntegral >$< wordVar


zigZag :: (Storable a, Bits a) => a -> a
zigZag x = (x `shift` 1) `xor` (x `shift` (1 - 8 * sizeOf x))

{-# INLINE int8VarSigned #-}
int8VarSigned :: BoundedEncoding Int8
int8VarSigned = zigZag >$< int8Var

{-# INLINE int16VarSigned #-}
int16VarSigned :: BoundedEncoding Int16
int16VarSigned = zigZag >$< int16Var

{-# INLINE int32VarSigned #-}
int32VarSigned :: BoundedEncoding Int32
int32VarSigned = zigZag >$< int32Var

{-# INLINE int64VarSigned #-}
int64VarSigned :: BoundedEncoding Int64
int64VarSigned = zigZag >$< int64Var

{-# INLINE intVarSigned #-}
intVarSigned :: BoundedEncoding Int
intVarSigned = zigZag >$< intVar


-- Padded versions for streamed prefixing of output sizes
---------------------------------------------------------

{-# INLINE appsUntilZero #-}
appsUntilZero :: Num a => (a -> a) -> a -> Int
appsUntilZero f x0 = 
    count 0 x0
  where
    count !n 0 = n
    count !n x = count (succ n) (f x)
        

{-# INLINE wordVarFixed #-}
wordVarFixed :: Word -> FixedEncoding Word
wordVarFixed bound = 
    fixedEncoding n0 io
  where
    n0 = appsUntilZero (`shiftr_w` 7) bound

    io !x0 !op 
      | x0 > bound = error $ "genericVarFixed: value " ++ show x0 ++ 
                             " > bound " ++ show bound
      | otherwise  = loop 0 x0
      where
        loop !n !x
          | n0 <= n   = do poke8 (x .&. 0x7f)
          | otherwise = do poke8 ((x .&. 0x7f) .|. 0x80)
                           loop (n + 1) (x `shiftr_w` 7)
          where
            poke8 = pokeElemOff op n . fromIntegral

{-# INLINE intVarFixed #-}
intVarFixed :: Size -> FixedEncoding Size
intVarFixed bound = fromIntegral >$< wordVarFixed (fromIntegral bound)



------------------------------------------------------------------------------
-- Chunked Encoding Transformer
------------------------------------------------------------------------------

{-# INLINE encodeChunked #-}
encodeChunked
    :: Size                           -- ^ Minimal free-size
    -> (Size -> FixedEncoding Size)   
    -- ^ Given a sizeBound on the maximal encodable size this function must return
    -- a fixed-size encoding for encoding all smaller size.
    -> (BoundedEncoding Size)
    -- ^ An encoding for terminating a chunk of the given size.
    -> Builder
    -- ^ Inner Builder to transform
    -> Builder
    -- ^ 'Put' with chunked encoding.
encodeChunked minFree mkBeforeFE afterBE =
    fromPut . putChunked minFree mkBeforeFE afterBE chunkB . putBuilder
  where
    chunkB bs = 
        encodeWithF (mkBeforeFE l) l `mappend`
        byteStringInsert bs `mappend`
        encodeWithB afterBE l
      where
        l = S.length bs

{-# INLINE putChunked #-}
putChunked
    :: Size                           -- ^ Minimal free-size
    -> (Size -> FixedEncoding Size)   
    -- ^ Given a sizeBound on the maximal encodable size this function must return
    -- a fixed-size encoding for encoding all smaller size.
    -> (BoundedEncoding Size)
    -- ^ An encoding for terminating a chunk of the given size.
    -> (S.ByteString -> Builder)
    -- ^ Encoding a directly inserted chunk.
    -> Put a
    -- ^ Inner Put to transform
    -> Put a
    -- ^ 'Put' with chunked encoding.
putChunked minFree0 mkBeforeFE afterBE chunkB p =
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
              fillWithBuildStep innerStep doneH fullH insertChunkH brInner
          where
            outRemaining   = ope `minusPtr` op
            beforeFE       = mkBeforeFE outRemaining
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
                innerSize = opInner' `minusPtr` opInner

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

            insertChunkH opInner' bs nextInnerStep
              | S.null bs = do                      -- flush
                  op' <- wrapChunk opInner'
                  return $! insertChunk op' S.empty 
                    (fill nextInnerStep)

              | otherwise = do                      -- insert non-empty bytestring
                  op' <- wrapChunk opInner'
                  let !br' = BufferRange op' ope
                  runBuilderWith (chunkB bs) (fill nextInnerStep) br'

  

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
    :: (Size -> FixedEncoding Size)   
    -- ^ Given a bound on the maximal size to encode, this function must return
    -- a fixed-size encoding for all smaller sizes.
    -> BoundedEncoding Int64
    -- ^ Encoding the size for the fallback case.
    -> Builder
    -- ^ 'Put' to prefix with the length of its sequence of bytes.
    -> Builder
encodeWithSize mkSizeFE sizeBE = 
    fromPut . putWithSize mkSizeFE sizeBE . putBuilder

-- | Prefix a 'Put' with the size of its written data.
{-# INLINE putWithSize #-}
putWithSize 
    :: forall a.
       (Size -> FixedEncoding Size)   
    -- ^ Given a bound on the maximal size to encode, this function must return
    -- a fixed-size encoding for all smaller sizes.
    -> BoundedEncoding Int64
    -- ^ Encoding the size for the fallback case.
    -> Put a
    -- ^ 'Put' to prefix with the length of its sequence of bytes.
    -> Put a
putWithSize mkSizeFE sizeBE innerP =
    put encodingStep
  where
    -- The minimal free-size must be as large as '2 * smallChunkSize' to
    -- guarantee that when switching to the slow-mode, we can always just
    -- insert the generated chunks while guaranteeing the minimal average
    -- chunk-size.  We /must/ insert the chunks directly to avoid copying data
    -- multiple times when nesting 'encodeWithSize' transformers.
    minFree      = 2 * L.smallChunkSize

    encodingStep :: forall r. (a -> BuildStep r) -> BuildStep r
    encodingStep k = 
        fill (runPut innerP)
      where
        fill :: BuildStep a -> BufferRange -> IO (BuildSignal r)
        fill innerStep !br@(BufferRange op ope)
          | outRemaining < minBufferSize = 
              return $! bufferFull minBufferSize op (fill innerStep)
          | otherwise = do
              fillWithBuildStep innerStep doneH fullH insertChunkH brInner
          where
            outRemaining  = ope `minusPtr` op
            sizeFE        = mkSizeFE outRemaining
            reserved      = size sizeFE
            minBufferSize = minFree + reserved
           
            opInner       = op  `plusPtr` reserved
            brInner       = BufferRange opInner ope

            fastPrefixSize :: Ptr Word8 -> IO (Ptr Word8)
            fastPrefixSize !opInner'
              | innerSize == 0 = do runB sizeBE 0 op
              | otherwise      = do runF sizeFE innerSize op
                                    return opInner'
              where
                innerSize = opInner' `minusPtr` opInner

            slowPrefixSize :: Ptr Word8 -> Builder -> BuildStep a -> IO (BuildSignal r)
            slowPrefixSize opInner' replaySignal nextStep = do
                (x, lbs, l) <- toLBS $ runBuilderWith bInner nextStep
                runBuilderWith (encodeWithB sizeBE l `mappend` 
                                lazyByteStringInsert lbs)
                               (k x) br
              where
                toLBS = runCIOSWithLength <=< buildStepToCIOSUntrimmed
                -- FIXME: We are missing a signal to retrieve the foreign pointer
                -- of the buffer in order to avoid the copy below.
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

            insertChunkH :: Ptr Word8 -> S.ByteString -> BuildStep a -> IO (BuildSignal r)
            insertChunkH opInner' bs nextInnerStep =
                slowPrefixSize opInner' (byteStringInsert bs) nextInnerStep


-- | Run a 'ChunkIOStream' and gather its results and their length.
runCIOSWithLength :: ChunkIOStream a -> IO (a, L.ByteString, Int64)
runCIOSWithLength = 
    go 0 id
  where
    go !l lbsd (Finished x)  = return (x, lbsd L.Empty, l)
    go !l lbsd (Yield bs io) = 
        go (l + fromIntegral (S.length bs)) (L.Chunk bs . lbsd) =<< io

------------------------------------------------------------------------------
-- Debugging encodings
------------------------------------------------------------------------------

-- TODO: Port testing infrastructure.

