{-# LANGUAGE CPP, BangPatterns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{- | Copyright : (c) 2010-2012 Simon Meier
                   (c) 2010      Jasper van der Jeugt
License        : BSD3-style (see LICENSE)
Maintainer     : Simon Meier <iridcode@gmail.com>
Portability    : GHC

This module provides the types of fixed-size and bounded-size encodings,
  which are the basic building blocks for constructing 'Builder's.
They are used for application specific performance tuning of 'Builder's.
For example,
  libraries such as @blaze-html@
  or @aeson@ use the functions provided by this module to implement efficient
  encodings that combine escaping and character encoding.
We explain fixed-size and bounded-size encodings in three steps.
First, we define them formally.
Then, we explain how they can improve the performance of a 'Builder'.
Finally, we give two examples to illustrate their use.

/Fixed(-size) encodings/ are encodings that always result in a sequence of bytes
  of a predetermined, fixed length.
An example for a fixed encoding is the big-endian encoding of a 'Word64',
  which always results in exactly 8 bytes.
/Bounded(-size) encodings/ are encodings that always result in a sequence
  of bytes that is no larger than a predetermined bound.
An example for a bounded encoding is the UTF-8 encoding of a 'Char',
  which results always in less or equal to 4 bytes.

Note that every fixed encoding is also a bounded encoding.
This module does not expose functions that exploit the special
  properties of fixed-size encodings.
However,
  they are exploited in the functions 'encodeSizePrefixed' and 'encodeChunked'
  from "Data.ByteString.Lazy.Builder.Extras", which prefix a 'Builder' with
  its (chunk) size.
In the following,
  we therefore only refer to bounded encodings.

The goal of bounded encodings is to improve the performance of 'Builder's.
These improvements stem from making the two
  most common steps performed by a 'Builder' more efficient.

The first most common step is the concatentation of two 'Builder's.
Internally,
  concatentation corresponds to function composition.
(Note that 'Builder's can be seen as difference-lists
  of buffer-filling functions;
  cf.  <http://hackage.haskell.org/cgi-bin/hackage-scripts/package/dlist>.
)
Function composition is a fast /O(1)/ operation.
However,
  we can use bounded encodings to
  remove some of these function compositions altoghether,
  which is obviously more efficient.

The second most common step performed by a 'Builder' is to fill a buffer
  using a bounded encoding,
  which works as follows.
The 'Builder' checks whether there is enough space left to
  execute the bounded encoding.
If there is, then the 'Builder' executes the bounded encoding
  and calls the next 'Builder' with the updated buffer.
Otherwise,
  the 'Builder' signals its driver that it requires a new buffer.
This buffer must be at least as large as the bound of the encoding.
We can use bounded encodings to reduce the number of buffer-free
  checks by fusing the buffer-free checks of consecutive
  'Builder's.
We can also use bounded encodings to simplify the control flow
  for signalling that a buffer is full by
  ensuring that we check first that there is enough space left
  and only then decide on how to encode a given value.

Let us illustrate these improvements on the
  CSV-table rendering example from "Data.ByteString.Lazy.Builder".
Its \"hot code\" is the rendering of a table's cells,
  which we implement as follows using only the functions from the
  'Builder' API.

@
import           "Data.ByteString.Lazy.Builder"         as B
import           "Data.ByteString.Lazy.Builder.ASCII"   as B

renderCell :: Cell -> Builder
renderCell (StringC cs) = renderString cs
renderCell (IntC i)     = B.intDec i

renderString :: String -> Builder
renderString cs = B.charUtf8 \'\"\' \<\> foldMap escape cs \<\> B.charUtf8 \'\"\'
  where
    escape \'\\\\\' = B.charUtf8 \'\\\\\' \<\> B.charUtf8 \'\\\\\'
    escape \'\\\"\' = B.charUtf8 \'\\\\\' \<\> B.charUtf8 \'\\\"\'
    escape c    = B.charUtf8 c
@

Efficient encoding of 'Int's as decimal numbers is performed by @intDec@
  from "Data.ByteString.Lazy.Builder.ASCII".
Optimization potential exists for the escaping of 'String's.
The above implementation has two optimization opportunities.
First,
  we can use a single buffer free check for @4@ bytes before escaping a
  character and only then decide on how to escape the character.
This simplifies the control flow and allows to avoid a closure construction,
  which improves the performance of the 'Builder'.
Second,
  the concatenations performed by 'foldMap' can be eliminated.
The following implementation exploits these optimizations.

@
import qualified Data.ByteString.Lazy.Builder.BasicEncoding  as E
import           Data.ByteString.Lazy.Builder.BasicEncoding
                 ( 'ifB', 'fromF', 'pairF', ('>$<') )

renderString :: String -\> Builder
renderString cs =
    B.charUtf8 \'\"\' \<\> E.'encodeListWithB' escape cs \<\> B.charUtf8 \'\"\'
  where
    escape :: E.'BoundedEncoding' Char
    escape = E.'charUtf8AsciiWith' $
      'ifB' (== \'\\\\\') (fixed2 (\'\\\\\', \'\\\\\')) $
      'ifB' (== \'\\\"\') (fixed2 (\'\\\\\', \'\\\"\')) $
      ('fromF' E.'char8')
    &#160;
    {&#45;\# INLINE fixed2 \#&#45;}
    fixed2 x = 'fromF' $ const x '>$<' E.'char8' `pairF` E.'char8'
@

The code should be mostly self-explanatory.
The slightly awkward syntax is because the combinators
  are written such that the 'sizeBound' of the resulting 'BoundedEncoding'
  can be computed at compile time.
We also explicitly inline the 'fixed2' encoding,
  which encodes a fixed tuple of characters,
  to ensure that the bound compuation happens at compile time.
When encoding the following list of 'String's,
  the optimized implementation of 'renderString' is two times faster.

@
maxiStrings :: [String]
maxiStrings = take 1000 $ cycle [\"hello\", \"\\\"1\\\"\", \"&#955;-w&#246;rld\"]
@

Most of the performance gain stems from using 'encodeListWithB',
  which encodes a list of values from left-to-right with a
  'BoundedEncoding'.
It exploits the 'Builder' internals to avoid unnecessary function
  compositions (i.e., concatentations).
In the future,
  we would expect the compiler to perform the optimizations
  implemented in 'encodeListWithB' by himself.
However,
  it seems that the code is currently to complicated for the
  compiler to see through.
Therefore,
  we provide the 'BoundedEncoding' escape hatch,
  which allows data structures to provide very efficient encoding traversals,
  like 'encodeListWithB' for lists.

Note that 'BoundedEncoding's are a bit verbose, but quite versatile.
Here is an example of a 'BoundedEncoding' for combined HTML escaping and
  UTF-8 encoding.

@
{&#45;\# INLINE charUtf8HtmlEscaped \#&#45;}
charUtf8HtmlEscaped :: E.BoundedEncoding Char
charUtf8HtmlEscaped =
    E.'charUtf8AsciiWith' $
      'ifB' (== \'\<\' ) (fixed4 (\'&\',(\'l\',(\'t\',\';\')))) $        -- &lt;
      'ifB' (== \'\>\' ) (fixed4 (\'&\',(\'g\',(\'t\',\';\')))) $        -- &gt;
      'ifB' (== \'&\' ) (fixed5 (\'&\',(\'a\',(\'m\',(\'p\',\';\'))))) $  -- &amp;
      'ifB' (== \'\"\' ) (fixed5 (\'&\',(\'\#\',(\'3\',(\'4\',\';\'))))) $  -- &\#34;
      ('fromF' E.'char8')         -- unescaped ASCII characters
  where
    {&#45;\# INLINE fixed4 \#&#45;}
    fixed4 x = 'fromF' $ const x '>$<'
      E.char8 '>*<' E.char8 '>*<' E.char8 '>*<' E.char8
    &#160;
    {&#45;\# INLINE fixed5 \#&#45;}
    fixed5 x = 'fromF' $ const x '>$<'
      E.char8 '>*<' E.char8 '>*<' E.char8 '>*<' E.char8 '>*<' E.char8
@

Note that this HTML escaping is only suitable for HTML attribute values that
are /double-quoted/ and for HTML content.

-}
module Data.ByteString.Lazy.Builder.BasicEncoding (

  -- * Fixed-size encodings
    FixedEncoding
  , size

  -- ** Combinators
  -- | The combinators for 'FixedEncoding's are implemented such that the 'size'
  -- of the resulting 'FixedEncoding' is computed at compile time.
  , emptyF
  , pairF
  , contramapF

  -- ** Builder construction
  -- | In terms of expressivity, the function 'encodeWithF' would be sufficient
  -- for constructing 'Builder's from 'FixedEncoding's. The fused variants of
  -- this function are provided because they allow for more efficient
  -- implementations. Our compilers are just not smart enough yet; and for some
  -- of the employed optimizations (see the code of 'encodeByteStringWithF')
  -- they will very likely never be.
  --
  -- Note that functions marked with \"/Heavy inlining./\" are forced to be
  -- inlined because they must be specialized for concrete encodings,
  -- but are rather heavy in terms of code size. We recommend to define a
  -- top-level function for every concrete instantiation of such a function in
  -- order to share its code. A typical example is the function
  -- 'byteStringHexFixed' from "Data.ByteString.Lazy.Builder.ASCII", which is
  -- implemented as follows.
  --
  -- @
  -- {-\# NOINLINE byteStringHexFixed \#-}
  -- byteStringHexFixed :: S.ByteString -> Builder
  -- byteStringHexFixed = 'encodeByteStringWithF' 'word8HexFixed'
  -- @
  --
  , encodeWithF
  , encodeListWithF
  , encodeUnfoldrWithF

  , encodeByteStringWithF
  , encodeLazyByteStringWithF

  -- * Bounded-size encodings

  , BoundedEncoding
  , sizeBound

  -- ** Combinators
  -- | The combinators for 'BoundedEncoding's are implemented such that the
  -- 'sizeBound' of the resulting 'BoundedEncoding' is computed at compile time.
  , fromF
  , emptyB
  , pairB
  , eitherB
  , ifB
  , contramapB

  -- | We provide an overloaded operator for the 'contramapF' and 'contramapB'
  -- combinators to allow for a more convenient syntax. The operator is
  -- source-compatible with the one provided by the @contravariant@ library.
  -- Once this library is part of the Haskell platform, we will make
  -- 'FixedEncoding' and 'BoundedEncoding' instances of its 'Contravariant'
  -- type-class.
  , (>$<)

  -- ** Builder construction
  , encodeWithB
  , encodeListWithB
  , encodeUnfoldrWithB

  , encodeByteStringWithB
  , encodeLazyByteStringWithB

  -- * Standard encodings of Haskell values

  , module Data.ByteString.Lazy.Builder.BasicEncoding.Binary

  -- ** Character encodings
  , module Data.ByteString.Lazy.Builder.BasicEncoding.ASCII

  -- *** ISO/IEC 8859-1 (Char8)
  -- | The ISO/IEC 8859-1 encoding is an 8-bit encoding often known as Latin-1.
  -- The /Char8/ encoding implemented here works by truncating the Unicode
  -- codepoint to 8-bits and encoding them as a single byte. For the codepoints
  -- 0-255 this corresponds to the ISO/IEC 8859-1 encoding. Note that the
  -- Char8 encoding is equivalent to the ASCII encoding on the Unicode
  -- codepoints 0-127. Hence, functions such as 'intDec' can also be used for
  -- encoding 'Int's as a decimal number with Char8 encoded characters.
  , char8

  -- *** UTF-8
  -- | The UTF-8 encoding can encode all Unicode codepoints.
  -- It is equivalent to the ASCII encoding on the Unicode codepoints 0-127.
  -- Hence, functions such as 'intDec' can also be used for encoding 'Int's as
  -- a decimal number with UTF-8 encoded characters.
  , charUtf8
  , charUtf8AsciiWith

  -- * Testing support
  -- | The following four functions are intended for testing use
  -- only. They are /not/ efficient. 'FixedEncoding's and 'BoundedEncoding's
  -- are efficently executed by creating 'Builder's from them using the
  -- @encodeXXX@ functions explained at the top of this module.

  , evalF
  , evalB

  , showF
  , showB

  ) where

import           Data.ByteString.Lazy.Builder.Internal
import           Data.ByteString.Lazy.Builder.BasicEncoding.Internal.UncheckedShifts
import           Data.ByteString.Lazy.Builder.BasicEncoding.Internal.Base16 (lowerTable, encode4_as_8)

import qualified Data.ByteString               as S
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy.Internal as L

import           Data.Monoid
import           Data.Foldable (foldMap) -- HADDOCK ONLY
import           Data.List (unfoldr)  -- HADDOCK ONLY
import           Data.Char (chr, ord)
import           Control.Monad ((<=<), unless)

import           Data.ByteString.Lazy.Builder.BasicEncoding.Internal
import           Data.ByteString.Lazy.Builder.BasicEncoding.Binary
import           Data.ByteString.Lazy.Builder.BasicEncoding.ASCII

#if MIN_VERSION_base(4,4,0)
import           Foreign hiding (unsafePerformIO, unsafeForeignPtrToPtr)
import           Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import           System.IO.Unsafe (unsafePerformIO)
#else
import           Foreign
#endif

------------------------------------------------------------------------------
-- Creating Builders from bounded encodings
------------------------------------------------------------------------------

-- | Encode a value with a 'FixedEncoding'.
{-# INLINE encodeWithF #-}
encodeWithF :: FixedEncoding a -> (a -> Builder)
encodeWithF = encodeWithB . fromF

-- | Encode a list of values from left-to-right with a 'FixedEncoding'.
{-# INLINE encodeListWithF #-}
encodeListWithF :: FixedEncoding a -> ([a] -> Builder)
encodeListWithF = encodeListWithB . fromF

-- | Encode a list of values represented as an 'unfoldr' with a 'FixedEncoding'.
{-# INLINE encodeUnfoldrWithF #-}
encodeUnfoldrWithF :: FixedEncoding b -> (a -> Maybe (b, a)) -> a -> Builder
encodeUnfoldrWithF = encodeUnfoldrWithB . fromF

-- | /Heavy inlining./ Encode all bytes of a strict 'S.ByteString' from
-- left-to-right with a 'FixedEncoding'. This function is quite versatile. For
-- example, we can use it to construct a 'Builder' that maps every byte before
-- copying it to the buffer to be filled.
--
-- @
--mapToBuilder :: (Word8 -> Word8) -> S.ByteString -> Builder
--mapToBuilder f = 'encodeByteStringWithF' ('contramapF' f 'word8')
-- @
--
-- We can also use it to hex-encode a strict 'S.ByteString' as shown in the
-- 'byteStringHexFixed' example above.
{-# INLINE encodeByteStringWithF #-}
encodeByteStringWithF :: FixedEncoding Word8 -> (S.ByteString -> Builder)
encodeByteStringWithF = encodeByteStringWithB . fromF

-- | /Heavy inlining./ Encode all bytes of a lazy 'L.ByteString' from
-- left-to-right with a 'FixedEncoding'.
{-# INLINE encodeLazyByteStringWithF #-}
encodeLazyByteStringWithF :: FixedEncoding Word8 -> (L.ByteString -> Builder)
encodeLazyByteStringWithF = encodeLazyByteStringWithB . fromF

-- IMPLEMENTATION NOTE: Sadly, 'encodeListWith' cannot be used for foldr/build
-- fusion. Its performance relies on hoisting several variables out of the
-- inner loop.  That's not possible when writing 'encodeListWith' as a 'foldr'.
-- If we had stream fusion for lists, then we could fuse 'encodeListWith', as
-- 'encodeWithStream' can keep control over the execution.


-- | Encode a value with a 'BoundedEncoding'.
--
-- Note that consecutive uses of 'encodeWithB' and 'encodeWithF' are rewritten
-- such that their bounds-checks are fused; i.e., we rewrite using the rules
--
-- >  encodeWithF 
-- >= encodeWithB . fromF
--
-- >  encodeWithB be1 x1 `mappend` (encodeWithB be2 x2)
-- >= encodeWithB (pairB be1 be2) (x1, x2)
--
-- For example,
--
-- >encodeWithB (word32 x1) `mappend` encodeWithB (word32 x2)
--
-- is rewritten such that the resulting 'Builder' checks only once, if ther are
-- at least 8 free bytes, instead of checking twice, if there are 4 free bytes.
-- This rewrite rule is not observationally equivalent, as it may change the
-- boundaries of the generated chunks. We deem this acceptable, as for all
-- use-cases of 'Builder's known to us the precise location of chunk
-- boundaries does not matter.
--
{-# INLINE[1] encodeWithB #-}
encodeWithB :: BoundedEncoding a -> (a -> Builder)
encodeWithB w x =
    -- It is important to avoid recursive 'BuildStep's where possible, as
    -- their closure allocation is expensive. Using 'ensureFree' allows the
    -- 'step' to assume that at least 'sizeBound w' free space is available.
    ensureFree (sizeBound w) `mappend` builder step
  where
    step k (BufferRange op ope) = do
        op' <- runB w x op
        let !br' = BufferRange op' ope
        k br'

-- Fuse bounds-checks of concatenated 'BoundedEncoding's.
{-# RULES
"append/encodeWithB" forall w1 w2 x1 x2.
       append (encodeWithB w1 x1) (encodeWithB w2 x2)
     = encodeWithB (pairB w1 w2) (x1, x2)

"append/encodeWithB/assoc_r" forall w1 w2 x1 x2 b.
       append (encodeWithB w1 x1) (append (encodeWithB w2 x2) b)
     = append (encodeWithB (pairB w1 w2) (x1, x2)) b

"append/encodeWithB/assoc_l" forall w1 w2 x1 x2 b.
       append (append b (encodeWithB w1 x1)) (encodeWithB w2 x2)
     = append b (encodeWithB (pairB w1 w2) (x1, x2))
  #-}

-- | Encodes a list of values from left-to-right using a 'BoundedEncoding'.
{-# INLINE encodeListWithB #-}
encodeListWithB :: BoundedEncoding a -> [a] -> Builder
encodeListWithB w xs0 =
    builder $ step xs0
  where
    step xs1 k (BufferRange op0 ope0) = 
        go xs1 op0
      where
        go []          !op             = k (BufferRange op ope0)
        go xs@(x':xs') !op
          | op `plusPtr` bound <= ope0 = runB w x' op >>= go xs'
          | otherwise                  = 
             return $ bufferFull bound op (step xs k)

    bound = sizeBound w

-- TODO: Think about adding 'foldMap/encodeWith' fusion its variants
-- TODO: Think about rewriting 'encodeWithB w . f = encodeWithB (w #. f)'

-- | Create a 'Builder' that encodes a list of values represented as an
-- 'unfoldr' with a 'BoundedEncoding'.
{-# INLINE encodeUnfoldrWithB #-}
encodeUnfoldrWithB :: BoundedEncoding b -> (a -> Maybe (b, a)) -> a -> Builder
encodeUnfoldrWithB w f x0 =
    builder $ fillWith x0
  where
    fillWith x k !(BufferRange op0 ope0) =
        go (f x) op0
      where
        go !Nothing        !op         = do let !br' = BufferRange op ope0
                                            k br'
        go !(Just (y, x')) !op         
          | op `plusPtr` bound <= ope0 = runB w y op >>= go (f x')
          | otherwise                  = return $ bufferFull bound op $
              \(BufferRange opNew opeNew) -> do
                  !opNew' <- runB w y opNew
                  fillWith x' k (BufferRange opNew' opeNew)
    bound = sizeBound w

-- | /Heavy inlining./ Encode all bytes of a strict 'S.ByteString' from
-- left-to-right with a 'BoundedEncoding'.
--
-- For example, we can use this function to construct a 'Builder' that filters
-- every byte before copying it to the buffer to be filled.
--
-- @
--filterToBuilder :: (Word8 -> Bool) -> S.ByteString -> Builder
--filterToBuilder p =
--  'encodeByteStringWithB' ('ifB' p ('fromF' 'word8') 'emptyB')
-- @
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

-- | /Heavy inlining./ Encode all bytes of a lazy 'L.ByteString' from
-- left-to-right with a 'BoundedEncoding'.
{-# INLINE encodeLazyByteStringWithB #-}
encodeLazyByteStringWithB :: BoundedEncoding Word8 -> L.ByteString -> Builder
encodeLazyByteStringWithB w =
    L.foldrChunks (\x b -> encodeByteStringWithB w x `mappend` b) mempty


------------------------------------------------------------------------------
-- Char8 encoding
------------------------------------------------------------------------------

-- | Char8 encode a 'Char'.
{-# INLINE char8 #-}
char8 :: FixedEncoding Char
char8 = (fromIntegral . ord) >$< word8


------------------------------------------------------------------------------
-- UTF-8 encoding
------------------------------------------------------------------------------

-- | UTF-8 encode a 'Char'.
{-# INLINE charUtf8 #-}
charUtf8 :: BoundedEncoding Char
charUtf8 = charUtf8AsciiWith (fromF char8)

-- | UTF-8 encode all 'Char's with codepoints greater or equal to 128 and
-- use a special encoding for the ASCII characters.
--
-- This function is typically used to implement UTF-8 encoding combined with
-- escaping. For example, escaping the \" and the \\ characters as in
-- Haskell 'String's works follows.
--
-- @
--{&#45;\# INLINE charUtf8Escaped \#&#45;}
--charUtf8Escaped :: 'BoundedEncoding' Char
--charUtf8Escaped = 'charUtf8AsciiWith' $
--    'ifB' (== \'\\\\\') (fixed2 (\'\\\\\', \'\\\\\')) $
--    'ifB' (== \'\\\"\') (fixed2 (\'\\\\\', \'\\\"\')) $
--    ('fromF' 'char8')
--  where
--    {&#45;\# INLINE fixed2 \#&#45;}
--    fixed2 x = 'fromF' $ const x '>$<' 'char8' \`pairF\` 'char8'
-- @
--
-- The following function would then escape 'String's.
--
-- @
--escapeString :: String -> 'Builder'
--escapeString = 'encodeListWithB' charUtf8Escaped
-- @
--
-- For example, 
-- @toLazyByteString (escapeString \"\\\"&#955;-w&#246;rld\\\"\") == \"\\\"\\206\\187-w\\195\\182rld\\\"\"@.
--
{-# INLINE charUtf8AsciiWith #-}
charUtf8AsciiWith
  :: BoundedEncoding Char 
     -- ^ Encoding for the ASCII characters. It is guaranteed
     -- to be called only with 'Char's with codepoint less than 128.
  -> BoundedEncoding Char
     -- ^ Resulting 'BoundedEncoding' that combines UTF-8 encoding with
     -- the special encoding for the ASCII characters.
charUtf8AsciiWith ascii = 
    ifB (<= '\x7F') ascii $ (ord >$<) $
    ifB (<= 0x07FF)
      (fromF (
        (\x -> 
          ( fromIntegral $ (x `shiftR` 6) + 0xC0
          , fromIntegral $ (x .&. 0x3F)   + 0x80 )
        ) >$< (word8 `pairF` word8) 
      ) ) $
    ifB (<= 0xFFFF) 
      (fromF $ (
        (\x -> 
          ( ( 
            fromIntegral $ (x `shiftR` 12) + 0xE0
          , fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80 )
          , fromIntegral $ (x .&. 0x3F) + 0x80              )
        ) >$< (word8 `pairF` word8 `pairF` word8)
      ) ) $
      (fromF $ (
        (\x ->
          ( ( ( 
            fromIntegral $ (x `shiftR` 18) + 0xF0
          , fromIntegral $ ((x `shiftR` 12) .&. 0x3F) + 0x80  )
          , fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80   )
          , fromIntegral $ (x .&. 0x3F) + 0x80                )
        ) >$< (word8 `pairF` word8 `pairF` word8 `pairF` word8)
      ) )

------------------------------------------------------------------------------
-- Testing encodings
------------------------------------------------------------------------------

-- | /For testing use only./ Evaluate a 'FixedEncoding' on a given value.
evalF :: FixedEncoding a -> a -> [Word8]
evalF fe = S.unpack . S.unsafeCreate (size fe) . runF fe

-- | /For testing use only./ Evaluate a 'BoundedEncoding' on a given value.
evalB :: BoundedEncoding a -> a -> [Word8]
evalB be x = S.unpack $ unsafePerformIO $
    S.createAndTrim (sizeBound be) $ \op -> do
        op' <- runB be x op
        return (op' `minusPtr` op)

-- | /For testing use only./ Show the result of a 'FixedEncoding' of a given
-- value as a 'String' by interpreting the resulting bytes as Unicode
-- codepoints.
showF :: FixedEncoding a -> a -> String
showF fe = map (chr . fromIntegral) . evalF fe

-- | /For testing use only./ Show the result of a 'BoundedEncoding' of a given
-- value as a 'String' by interpreting the resulting bytes as Unicode
-- codepoints.
showB :: BoundedEncoding a -> a -> String
showB be = map (chr . fromIntegral) . evalB be


