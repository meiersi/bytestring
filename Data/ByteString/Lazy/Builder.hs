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
-- The 'Builder' type provides a representation of a stream of bytes
-- (i.e, a possibly infinite sequence of bytes) which is efficient in
-- the following sense:
--
--   1. The stream of bytes represented by a 'Builder' can be accessed as a
--      lazy 'L.ByteString' whose chunks are large on average.
--
--   2. Concatenating two 'Builder's is in /O(1)/.
--
--   3. Encoding values of standard Haskell types like 'Int32' or 'Char' as a
--      sequence of bytes represented as a 'Builder' is efficient in terms of
--      CPU cycles spent.
--
-- Property (1) guarantees that processing the resulting lazy 'L.ByteString'
-- chunk-wise is efficient, as the work on the chunk boundaries can be
-- amortized by a significant amount of actual work spent on the chunk content.
-- For example, when writing the resulting lazy 'L.ByteString' to a file or to a
-- network socket, the large chunk size guarantees that the cost of a system
-- call is distributed over enough bytes being written. Property (2)
-- guarantees that 'Builder's can be assembled efficiently from smaller
-- 'Builder's. This makes 'Builder's well-suited for declaratively describing a
-- stream of bytes. Property (2) is also is a significant difference to strict
-- and lazy bytestrings, whose concatenation is in /O(n)/. Property (3)
-- guarantees that creating the primitive 'Builder's, from which we assemble
-- our 'Builder's of interest, is efficent in terms of wall time.
--
-- Typically, 'Builder's are used to encode application internal data as a
-- sequence of bytes in an external format like JSON, HTML, or a HTTP response.
-- As a simple example, we'll show how to encode the following representation
-- of mixed-data tables as a Comma-Separated-Values (CSV) file. (Note that the
-- whole example is distributed with the source of this library.)
--
-- > data Cell = StringC String
-- >           | IntC Int
-- >           deriving( Eq, Ord, Show )
-- > 
-- > type Row   = [Cell]
-- > type Table = [Row]
--
-- We use the following imports and abbreviate 'mappend' to simplify reading.
--
-- > import qualified Data.ByteString.Lazy         as L
-- > import qualified Data.ByteString.Lazy.Builder as B
-- > import           Data.Monoid
-- > import           Data.List                 (intersperse)
-- > 
-- > infixr 4 <>
-- > (<>) :: Monoid m => m -> m -> m
-- > (<>) = mappend
--
-- CSV is a character-based representation of tables. For maximal modularity,
-- we'd first render 'Table's as 'String's and then encode this 'String' to a
-- file using some fixed character encoding. However, this sacrifices performance
-- due to the intermediate 'String' representation being built and thrown away
-- right afterwards. We get rid of this intermediate 'String' representation by
-- fixing the character encoding to UTF-8 and using 'Builder's to convert
-- 'Table's directly to UTF-8 encoded CSV tables represented as lazy
-- 'L.ByteString's.
--
-- > encodeUtf8CSV :: Table -> L.ByteString
-- > encodeUtf8CSV = B.toLazyByteString . renderTable
-- > 
-- > renderTable :: Table -> B.Builder
-- > renderTable rs = mconcat [renderRow r <> B.utf8 '\n' | r <- rs]
-- >            
-- > renderRow :: Row -> B.Builder
-- > renderRow []     = mempty
-- > renderRow (c:cs) = 
-- >     renderCell c <> mconcat [ B.utf8 ',' <> renderCell c' | c' <- cs ]
-- > 
-- > renderCell :: Cell -> B.Builder
-- > renderCell (StringC cs) = renderString cs
-- > renderCell (IntC i)     = B.utf8 $ show i
-- > 
-- > renderString :: String -> B.Builder
-- > renderString cs = B.utf8 '"' <> B.foldMap escape cs <> B.utf8 '"'
-- >   where
-- >     escape '\\' = B.utf8 '\\' <> B.utf8 '\\'
-- >     escape '\"' = B.utf8 '\\' <> B.utf8 '\"'
-- >     escape c    = B.utf8 c
--
-- We use our UTF-8 CSV encoding function on the following table. /Note that/
-- /Haddock currently doesn't handle the Unicode lambda-character and the/
-- /o-umlaut character in the last string "<lambda>-w<o-umlaut>rld". They are/
-- /present in the source, but not output by Haddock./
--
-- > strings :: [String]
-- > strings =  ["hello", "\"1\"", "λ-wörld"]
-- > 
-- > table :: Table
-- > table = [map StringC strings, map IntC [-3..3]]
--
-- The expression @encodeUtf8CSV table@ results in the following lazy
-- 'L.ByteString'.
--
-- > Chunk "\"hello\",\"\\\"1\\\"\",\"\206\187-w\195\182rld\"\n-3,-2,-1,0,1,2,3\n" Empty
--
-- We can clearly see that we are converting to a /binary/ format. The \'λ\'
-- (lambda-character) and \'ö\' (o-umlaut) characters, which have a Unicode
-- codepoint above 127, are expanded to their corresponding UTF-8 multi-byte
-- representation.
--
-- We use the criterion library to benchmark the efficiency of our encoding
-- function on the following table.
--
-- > import Criterion.Main     -- add this import to the ones above
-- >
-- > maxiTable :: Table
-- > maxiTable = take 1000 $ cycle table
-- >
-- > main :: IO ()
-- > main = defaultMain
-- >   [ bench "encodeUtf8CSV maxiTable (original)" $ 
-- >       whnf (L.length . encodeUtf8CSV) maxiTable
-- >   ]
--
-- On a Core2 Duo 2.20GHz on a 32-bit Linux, the above code takes 1.1ms to
-- generate the 22'500 bytes long lazy 'L.ByteString'. Looking again at
-- the definitions above, we see that we took care to avoid intermediate
-- data structures, as otherwise we would sacrifice performance.
-- For example, the following (arguably simpler) definition of 'renderRow' is
-- about 20% slower.
--
-- > renderRow :: Row -> B.Builder
-- > renderRow  = mconcat . intersperse (B.utf8 ',') . map renderCell
--
-- Similarly, using /O(n)/ concatentations like '(++)' should be avoided. The
-- following definition of 'renderString' is also about 20% slower.
--
-- > renderString :: String -> B.Builder
-- > renderString cs = B.utf8 $ "\"" ++ concatMap escape cs ++ "\"" 
-- >   where
-- >     escape '\\' = "\\"
-- >     escape '\"' = "\\\""
-- >     escape c    = return c
--
-- Note that on the above example 'Builder's are not much faster than using
-- difference-lists to achieve efficient 'String' concatenation. Rendering the
-- table first as a 'String' using a difference-list and UTF-8 encoding it
-- afterwards takes 1.3ms. However, this performance difference becomes more
-- pronounced once longer snippets of static-data are involved. 'Builder's can
-- exploit low-level memory copy and write operations, while 'String's always
-- have to follow one pointer per 'Char' and process this 'Char' individually.
--
-- Apart from removing intermediate data-structures, 'Builder's can be
-- optimized further by exploiting knowledge about their implementation.
-- Internally, 'Builder's are buffer-fill operations that are
-- given a continuation buffer-fill operation and a buffer-range to be filled.
-- A 'Builder' first checks if the buffer-range is large enough. If that's
-- the case, the 'Builder' writes the sequences of bytes to the buffer and
-- calls its continuation.  Otherwise, it returns a signal that it requires a
-- new buffer together with a continuation to be called on this new buffer.
-- Ignoring the rare case of a full buffer-range, the execution cost of a
-- 'Builder' consists of three parts: 
--
--   1. The time taken to read the parameters; i.e., the buffer-fill
--      operation to call after the 'Builder' is done and the buffer-range to
--      fill. 
--
--   2. The time taken to check for the size of the buffer-range.
--
--   3. The time taken for the actual encoding.
--
-- We can reduce cost (1) by ensuring that fewer buffer-fill function calls are
-- required. We can reduce cost (2) by fusing buffer-size checks of sequential
-- writes. For example, when escaping a 'String' using 'renderString', it would
-- be sufficient to check before encoding a character that at least 8 bytes are
-- free. We can reduce cost (3) by implementing better primitive 'Builder's.
-- For example, 'renderCell' builds an intermediate list containing the decimal
-- representation of an 'Int'. Implementing a direct decimal encoding of 'Int's
-- to memory would be more efficient, as it requires fewer buffer-size checks
-- and less allocation. It is also a planned extension of this library.
--
-- The first two cost reductions are supported for user code through functions
-- in "Data.ByteString.Lazy.Builder.Write". There, we continue the above example
-- and drop the generation time to 0.9ms by implementing 'renderString' more
-- cleverly. The third reduction requires meddling with the internals of
-- 'Builder's and is not recomended in code outside of this library. However,
-- patches to this library are very welcome. 
--
-----------------------------------------------------------------------------
module Data.ByteString.Lazy.Builder
    ( 
      -- * The Builder type
      Builder
      -- | Converting a list to a 'Builder' often works by converting each
      -- element and concatenating the resulting 'Builder's. The higher-order
      -- function abstracting this pattern is 'foldMap' from the
      -- "Data.Foldable" module.

      -- * Executing Builders
    , toLazyByteString

      -- * Creating Builders
    , flush
    , byteString
    , lazyByteString

      -- ** Encoding characters
    , module Data.ByteString.Lazy.Builder.Char.Utf8

      -- ** Encoding signed integers
    , int8

    , int16BE 
    , int32BE 
    , int64BE 

    , int16LE
    , int32LE
    , int64LE
      
      -- ** Encoding unsigned integers
    , word8

    , word16BE 
    , word32BE 
    , word64BE 

    , word16LE
    , word32LE
    , word64LE
      
    ) where

import Data.ByteString.Lazy.Builder.Internal
import Data.ByteString.Lazy.Builder.Extras
import Data.ByteString.Lazy.Builder.Word
import Data.ByteString.Lazy.Builder.Int
import Data.ByteString.Lazy.Builder.Char.Utf8

import qualified Data.ByteString               as S
import qualified Data.ByteString.Lazy.Internal as L


------------------------------------------------------------------------------
-- Default conversion of strict and lazy ByteStrings to Builders
------------------------------------------------------------------------------

-- | Create a 'Builder' denoting the same sequence of bytes as a strict
-- 'S.ByteString'.
--
-- The 'Builder' copies short 'S.ByteString's and inserts long 
-- (>= 2 * 'L.smallChunkSize')
-- 'S.ByteString's directly. This way the 'Builder' ensures that on average
-- chunks are large enough (>= 'L.smallChunkSize'), which is important for the
-- efficiency of consumers of the generated chunks. If you have a special
-- application that requires more control over chunk handling, then see the
-- module "Data.ByteString.Lazy.Builder.ByteString".
--
{-# INLINE byteString #-}
byteString :: S.ByteString -> Builder
byteString = byteStringWith defaultMaximalCopySize

-- | Chunk-wise application of 'byteString' to a lazy 'L.ByteString'.
--
{-# INLINE lazyByteString #-}
lazyByteString :: L.ByteString -> Builder
lazyByteString = lazyByteStringWith defaultMaximalCopySize

-- | The maxiamal size of a bytestring that is copied. 
-- @2 * 'L.smallChunkSize'@ to guarantee that on average a chunk is of
-- 'L.smallChunkSize'.
defaultMaximalCopySize :: Int
defaultMaximalCopySize = 2 * L.smallChunkSize

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

