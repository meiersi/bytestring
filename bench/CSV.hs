{-# LANGUAGE PackageImports #-}
-- |
-- Copyright   : (c) 2010-2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Running example for documentation of Data.ByteString.Lazy.Builder
--
module CSV where

-- **************************************************************************
-- CamHac 2011: An introduction to Data.ByteString.Lazy.Builder
-- **************************************************************************


{- The Encoding Problem
 ----------------------

 Encoding: Conversion from a Haskell value to a sequence of bytes.


 Efficient encoding implementation:

   1. represent sequence of bytes as a list of byte arrays (chunks)
   2. generate chunks that are large on average
   3. avoid intermediate copies/datastructures

 Compositionality:

   4. support fast append


 Problem: Provide a library for defining compositional, efficient encodings.

-}



{- Data.ByteString.Lazy.Builder
 ------------------------------
 
 A solution to the "Encoding Problem"  (based on the code of blaze-builder).

 Builder creation:

   word8   :: Word8 -> Builder
   int64LE :: Int64 -> Builder
   floatBE :: Float -> Builder
   ....


 Builder composition via its Monoid instance:

   word8 10 `mappend` floatBE 1.4


 Builder execution by converting it to a lazy bytestring:

   toLazyByteString :: Builder -> L.ByteString

-}


{- Typical users of Builders
 ---------------------------

 binary, text, aeson, blaze-html, blaze-textual, warp, snap-server, ...
 
 => they want support for maximal performance!
 => use of Builders is rather local: in rendering/encoding functions.

-}



{- Notable properties
 --------------------

 * Built-in UTF-8 support: very hard to get efficient otherwise.

     Utf8.string :: String -> Builder
     Utf8.intDec :: Int -> Builder
     Utf8.intHex :: Int -> Builder

 * Fine-grained control over when to copy/reference existing bytestrings

 * EDSL for defining low-level Encodings of bounded values (e.g., Int, Char)
   to improve speed of escaping and similar operations.
 
 * If used together with iteratee-style IO: no 'unsafePerformIO' required

-}


{- An example problem:
 ---------------------

 Rendering a table in comma-separated-value (CSV) format using UTF-8 encoded
 Unicode characters.

 * We are willing to fuse table-rendering with UTF8-encoding to achieve better
   performance.

-}


import qualified "new-bytestring" Data.ByteString                     as S
import qualified "new-bytestring" Data.ByteString.Lazy                as L

import           Data.ByteString.Lazy.Builder                         as B
import           Data.ByteString.Lazy.Builder.Utf8                    as B

import Data.Monoid
import Data.Foldable (foldMap)
       
import Criterion.Main
import Control.DeepSeq


-- To be used in a later optimization
import           Data.ByteString.Lazy.Builder.BoundedEncoding ( (<#>), (#.) )
import qualified Data.ByteString.Lazy.Builder.BoundedEncoding         as E
import qualified Data.ByteString.Lazy.Builder.BoundedEncoding.Utf8    as E

-- To be used in a later comparison
import qualified Data.DList                                           as D
import qualified Codec.Binary.UTF8.Light                         as Utf8Light
import qualified Data.String.UTF8                                as Utf8String


------------------------------------------------------------------------------
-- Simplife CSV Tables
------------------------------------------------------------------------------

data Cell = StringC String
          | IntC Int
          deriving( Eq, Ord, Show )

type Row   = [Cell]
type Table = [Row]

-- Example data
strings :: [String]
strings =  ["hello", "\"1\"", "λ-wörld"]

table :: Table
table = [map StringC strings, map IntC [-3..3]]


-- | The rendered 'table':
--
-- > "hello","\"1\"","λ-wörld"
-- > -3,-2,-1,0,1,2,3
--

 
-- | A bigger table for benchmarking our encoding functions.
maxiTable :: Table
maxiTable = take 1000 $ cycle table


------------------------------------------------------------------------------
-- String based rendering
------------------------------------------------------------------------------

renderString :: String -> String
renderString cs = "\"" ++ concatMap escape cs ++ "\"" 
  where
    escape '\\' = "\\"
    escape '\"' = "\\\""
    escape c    = return c

renderCell :: Cell -> String
renderCell (StringC cs) = renderString cs
renderCell (IntC i)     = show i
           
renderRow :: Row -> String
renderRow []     = ""
renderRow (c:cs) = renderCell c ++ concat [',' : renderCell c' | c' <- cs]

renderTable :: Table -> String
renderTable rs = concat [renderRow r ++ "\n" | r <- rs]

-- 1.53 ms
benchString :: Benchmark
benchString = bench "renderTable maxiTable" $ nf renderTable maxiTable

-- 1.45 ms
benchStringUtf8 :: Benchmark
benchStringUtf8 = bench "utf8 + renderTable maxiTable" $ 
  nf (L.length . B.toLazyByteString . B.string . renderTable) maxiTable


-- using difference lists:  0.92 ms
--
--  (++) is a performance-grinch!


------------------------------------------------------------------------------
-- Builder based rendering
------------------------------------------------------------------------------

-- better syntax for `mappend`
infixr 4 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

-- As a reminder:
--
-- import  Data.ByteString.Lazy.Builder       as B
-- import  Data.ByteString.Lazy.Builder.Utf8  as B

renderStringB :: String -> Builder
renderStringB cs = B.char '"' <> foldMap escape cs <> B.char '"'
  where
    escape '\\' = B.string "\\\\"
    escape '\"' = B.string "\\\\"
    escape c    = B.char c

renderCellB :: Cell -> Builder
renderCellB (StringC cs) = renderStringB cs
renderCellB (IntC i)     = B.intDec i
           
renderRowB :: Row -> Builder
renderRowB []     = mempty
renderRowB (c:cs) = 
    renderCellB c <> mconcat [ B.char ',' <> renderCellB c' | c' <- cs ]

renderTableB :: Table -> Builder
renderTableB rs = mconcat [renderRowB r <> B.char '\n' | r <- rs]

-- 0.81ms
benchBuilderUtf8 :: Benchmark
benchBuilderUtf8 = bench "utf8 + renderTableB maxiTable" $ 
  nf (L.length . B.toLazyByteString . renderTableB) maxiTable

-- 1.14x  faster than DList

-- However: touching the whole table 'nf maxiTable' takes  0.27ms

-- 1.20x  faster than DList on the code path other than touching all data  
--        (0.92 - 0.27) / (0.81 - 0.27)


------------------------------------------------------------------------------
-- Baseline: Touching all data
------------------------------------------------------------------------------

instance NFData Cell where
  rnf (StringC cs) = rnf cs
  rnf (IntC i)     = rnf i

-- 0.27 ms
benchNF :: Benchmark
benchNF = bench "nf maxiTable" $ nf id maxiTable


------------------------------------------------------------------------------
-- Exploiting bounded encodings 
------------------------------------------------------------------------------

{- Why 'Bounded Encodings'?
 --------------------------

 Hot code of encoding implementations:

 * Appending Builders: Optimized already.

 * Encoding primitive Haskell values: room for optimization:

     - reduce buffer-free checks
     - remove jumps/function calls
     - hoist constant values out of inner-loops 
       (e.g., the loop for encoding the elements of a list)

 * Bounded encoding: 
     an encoding that never takes more than a fixed number of bytes.

     - intuitively: (Int,     Ptr Word8 -> IO (Ptr Word8))
                     ^bound   ^ low-level encoding function

     - compositional: coalesce buffer-checks, ...

       E.encodeIf :: (a -> Bool) -> Encoding a -> Encoding a -> Encoding a
       E.char     :: Encoding Char
       (<#>)      :: Encoding a -> Encoding b -> Encoding (a, b)

       (#.)       :: Encoding a -> (b -> a) -> Encoding b

       ^ Encodings are cofunctors; like most data-sinks


     - Implementation relies heavily on inlining to compute bounds and
       low-level encoding code during compilation.
-}

renderStringBE :: String -> Builder
renderStringBE cs = 
    B.char '"' <> E.encodeListWith escape cs <> B.char '"'
  where
    escape :: E.Encoding Char
    escape = 
      E.encodeIf (== '\\') (E.char <#> E.char #. const ('\\', '\\')) $
      E.encodeIf (== '\"') (E.char <#> E.char #. const ('\\', '\"')) $
      E.char

renderCellBE :: Cell -> Builder
renderCellBE (StringC cs) = renderStringBE cs
renderCellBE (IntC i)     = B.intDec i
           
renderRowBE :: Row -> Builder
renderRowBE []     = mempty
renderRowBE (c:cs) = 
    renderCellBE c <> mconcat [ B.char ',' <> renderCellBE c' | c' <- cs ]

renderTableBE :: Table -> Builder
renderTableBE rs = mconcat [renderRowBE r <> B.char '\n' | r <- rs]

-- 0.68 ms
benchBuilderEncodingUtf8 :: Benchmark
benchBuilderEncodingUtf8 = bench "utf8 + renderTableBE maxiTable" $ 
  nf (L.length . B.toLazyByteString . renderTableBE) maxiTable


-- 1.35x faster than DList based

-- 1.58x faster than DList based on code other than touching all data


------------------------------------------------------------------------------
-- Difference-list based rendering
------------------------------------------------------------------------------

type DString = D.DList Char

renderStringD :: String -> DString
renderStringD cs = return '"' <> foldMap escape cs <> return '"'
  where
    escape '\\' = D.fromList "\\\\"
    escape '\"' = D.fromList "\\\\"
    escape c    = return c

renderCellD :: Cell -> DString
renderCellD (StringC cs) = renderStringD cs
renderCellD (IntC i)     = D.fromList $ show i
           
renderRowD :: Row -> DString
renderRowD []     = mempty
renderRowD (c:cs) = 
    renderCellD c <> mconcat [ return ',' <> renderCellD c' | c' <- cs ]

renderTableD :: Table -> DString
renderTableD rs = mconcat [renderRowD r <> return '\n' | r <- rs]

benchDListUtf8 :: Benchmark
benchDListUtf8 = bench "utf8 + renderTableD maxiTable" $ 
  nf (L.length . B.toLazyByteString . B.string . D.toList . renderTableD) maxiTable


------------------------------------------------------------------------------
-- utf8-string and utf8-light
------------------------------------------------------------------------------

-- 3.96 ms
benchDListUtf8Light :: Benchmark
benchDListUtf8Light = bench "utf8-light + renderTable maxiTable" $ 
  whnf (Utf8Light.encode . D.toList . renderTableD) maxiTable

{- Couldn't get utf8-string to work :-(
 
benchDListUtf8String :: Benchmark
benchDListUtf8String = bench "utf8-light + renderTable maxiTable" $ 
  whnf (Utf8String.toRep . encode .
        D.toList . renderTableD) maxiTable
  where
    encode :: String -> Utf8String.UTF8 S.ByteString
    encode = Utf8String.fromString 
-}


------------------------------------------------------------------------------
-- Benchmarking
------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "Encoding the maxiTable"
    putStrLn $ "Total length: " ++
        (show $ L.length $ encodeUtf8CSV maxiTable)
    putStrLn $ "Chunk lengths: " ++ 
        (show $ map S.length $ L.toChunks $ encodeUtf8CSV maxiTable)
    print ""
    defaultMain
      [ benchNF
      , benchString
      , benchStringUtf8
      , benchDListUtf8
      , benchDListUtf8Light
      , benchBuilderUtf8
      , benchBuilderEncodingUtf8     
      ]
  where
    encodeUtf8CSV = B.toLazyByteString . renderTableBE


{- On a Core 2 Duo 2.2 GHz running a 32-bit Linux:


touching all data:                 0.27 ms
string rendering:                  1.52 ms
string rendering + utf8 encoding:  1.46 ms   (less allocation?)
DList rendering  + utf8 encoding:  0.94 ms
builder rendering (incl. utf8):    0.84 ms
builder + faster escaping:         0.71 ms

How to improve further? 
  - Use packed formats for string literals
    => fast memcpy  (that's what blaze-html does for tags)


results from criterion:
 
  benchmarking nf maxiTable
  mean: 271.5343 us, lb 266.8739 us, ub 276.1637 us, ci 0.950
  std dev: 23.59385 us, lb 22.20160 us, ub 24.89147 us, ci 0.950

  benchmarking renderTable maxiTable
  mean: 1.522055 ms, lb 1.516985 ms, ub 1.533372 ms, ci 0.950
  std dev: 36.68640 us, lb 20.47489 us, ub 71.29054 us, ci 0.950

  benchmarking utf8 + renderTable maxiTable
  mean: 1.466305 ms, lb 1.461600 ms, ub 1.474856 ms, ci 0.950
  std dev: 31.92463 us, lb 20.90142 us, ub 59.03556 us, ci 0.950

  benchmarking utf8 + renderTableD maxiTable
  mean: 939.0503 us, lb 936.5558 us, ub 942.5777 us, ci 0.950
  std dev: 14.94975 us, lb 11.44815 us, ub 23.73155 us, ci 0.950

  benchmarking utf8-light + renderTable maxiTable
  mean: 3.949076 ms, lb 3.935238 ms, ub 3.970824 ms, ci 0.950
  std dev: 87.53537 us, lb 62.32593 us, ub 134.6882 us, ci 0.950

  benchmarking utf8 + renderTableB maxiTable
  mean: 842.8139 us, lb 840.6189 us, ub 846.0228 us, ci 0.950
  std dev: 13.53231 us, lb 10.06590 us, ub 18.86145 us, ci 0.950

  benchmarking utf8 + renderTableBE maxiTable
  mean: 710.0430 us, lb 708.3533 us, ub 713.2298 us, ci 0.950
  std dev: 11.39598 us, lb 7.380518 us, ub 20.79146 us, ci 0.950

-}



{- Conclusion:
 -------------

 * Whenever generating a sequence of bytes: use the 'Builder' type

   => chunks can always be kept large; impossible when exporting only
      a strict/lazy bytestring interface.

   => filtering/mapping lazy bytestrings now automatically defragments
      the output and guarantees a large chunk size.


 * Status of work: API complete, documentation needs more reviewing.


 * Bounded encodings: safely exploiting low-level optimizations

   => a performance advantage on other outputstream-libraries?
 

                           ---------------
                           - Questions ? -
                           ---------------

-}




{- Implementation outline:
 ------------------------

data BufferRange = BufferRange {-# UNPACK #-} !(Ptr Word8)  -- First byte of range
                               {-# UNPACK #-} !(Ptr Word8)  -- First byte /after/ range

newtype BuildStep a =  
    BuildStep { runBuildStep :: BufferRange -> IO (BuildSignal a) }

data BuildSignal a =
    Done             !(Ptr Word8)    -- next free byte in current buffer
                     a               -- return value
  | BufferFull
                     !Int            -- minimal size of next buffer
                     !(Ptr Word8)    -- next free byte in current buffer
                     !(BuildStep a)  -- continuation to call on next buffer
  | InsertByteString
                     !(Ptr Word8)    -- next free byte in current buffer
                     !S.ByteString   -- bytestring to insert directly
                     !(BuildStep a)  -- continuation to call on next buffer


-- | A "difference list" of build-steps.
newtype Builder = Builder (forall r. BuildStep r -> BuildStep r)


-- | The corresponding "Writer" monad.
newtype Put a = Put { unPut :: forall r. (a -> BuildStep r) -> BuildStep r }


-}
