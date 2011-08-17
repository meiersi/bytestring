{-# LANGUAGE PackageImports, ScopedTypeVariables, BangPatterns #-}
-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Benchmark all Encodings implemented directly.
module BenchAll where

import Prelude hiding (words)
import Criterion.Main
import Data.Foldable (foldMap)

import qualified "new-bytestring" Data.ByteString                  as S
import qualified "new-bytestring" Data.ByteString.Lazy             as L

import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Builder.Utf8  
import           Data.ByteString.Lazy.Builder.BoundedEncoding         ( Encoding, (#.) )
import qualified Data.ByteString.Lazy.Builder.BoundedEncoding      as E
import qualified Data.ByteString.Lazy.Builder.BoundedEncoding.Utf8 as Utf8

------------------------------------------------------------------------------
-- Benchmark support
------------------------------------------------------------------------------

countToZero :: Int -> Maybe (Int, Int)
countToZero 0 = Nothing
countToZero n = Just (n, n - 1)


------------------------------------------------------------------------------
-- Benchmark
------------------------------------------------------------------------------

-- input data (NOINLINE to ensure memoization)
----------------------------------------------

-- | Few-enough repetitions to avoid making GC too expensive.
nRepl :: Int
nRepl = 10000

{-# NOINLINE intData #-}
intData :: [Int]
intData = [1..nRepl]

-- Half of the integers inside the range of an Int and half of them outside.
{-# NOINLINE integerData #-}
integerData :: [Integer]
integerData = map (\x -> fromIntegral x + fromIntegral (maxBound - nRepl `div` 2)) intData

{-# NOINLINE floatData #-}
floatData :: [Float]
floatData = map (\x -> (3.14159 * fromIntegral x) ^ (3 :: Int)) intData

{-# NOINLINE doubleData #-}
doubleData :: [Double]
doubleData = map (\x -> (3.14159 * fromIntegral x) ^ (3 :: Int)) intData

{-# NOINLINE byteStringData #-}
byteStringData :: S.ByteString
byteStringData = S.pack $ map fromIntegral intData

{-# NOINLINE lazyByteStringData #-}
lazyByteStringData :: L.ByteString
lazyByteStringData = case S.splitAt (nRepl `div` 2) byteStringData of
    (bs1, bs2) -> L.fromChunks [bs1, bs2]


-- benchmark wrappers
---------------------

{-# INLINE benchB #-}
benchB :: String -> a -> (a -> Builder) -> Benchmark
benchB name x b = 
    bench (name ++" (" ++ show nRepl ++ ")") $
        whnf (L.length . toLazyByteString . b) x

{-# INLINE benchBInts #-}
benchBInts :: String -> ([Int] -> Builder) -> Benchmark
benchBInts name = benchB name intData

-- | Benchmark an encoding. Full inlining to enable specialization.
{-# INLINE benchE #-}
benchE :: String -> Encoding Int -> Benchmark
benchE name e = 
  bench (name ++" (" ++ show nRepl ++ ")") $ E.benchIntEncoding nRepl e



-- benchmarks
-------------

sanityCheckInfo :: [String]
sanityCheckInfo =
  [ "Sanity checks:"
  , " lengths of input data: " ++ show 
      [ length intData, length floatData, length doubleData, length integerData
      , S.length byteStringData, fromIntegral (L.length lazyByteStringData)
      ]
  ]

main :: IO ()
main = do
  mapM_ putStrLn sanityCheckInfo
  putStrLn ""
  Criterion.Main.defaultMain 
    [ bgroup "Data.ByteString.Lazy.Builder"
      [ bgroup "Encoding wrappers"
        [ benchBInts "foldMap word8"                 $ foldMap (word8 . fromIntegral)
        , benchBInts "encodeListWith word8"          $ E.encodeListWith (E.word8 #. fromIntegral)
        , benchB     "encodeUnfoldrWith word8" nRepl $ 
            E.encodeUnfoldrWith (E.word8 #. fromIntegral) countToZero
        , benchB     "encodeByteStringWith word8" byteStringData $ 
            E.encodeByteStringWith E.word8 
        , benchB     "encodeLazyByteStringWith word8" lazyByteStringData $ 
            E.encodeLazyByteStringWith E.word8 
        ]
      , bgroup "Non-bounded encodings"
        [ benchB "foldMap floatDec"        floatData          $ foldMap floatDec
        , benchB "foldMap doubleDec"       doubleData         $ foldMap doubleDec
        , benchB "foldMap integerDec"      integerData        $ foldMap integerDec
        , benchB "byteStringHexFixed"      byteStringData     $ byteStringHexFixed
        , benchB "lazyByteStringHexFixed"  lazyByteStringData $ lazyByteStringHexFixed
        ]
      ]

    , bgroup "Data.ByteString.Lazy.Builder.BoundedEncoding"
      [ benchE "char8"      $ E.char8      #. toEnum

      -- binary encoding
      , benchE "int8"       $ E.int8       #. fromIntegral
      , benchE "word8"      $ E.word8      #. fromIntegral

      -- big-endian
      , benchE "int16BE"    $ E.int16BE    #. fromIntegral
      , benchE "int32BE"    $ E.int32BE    #. fromIntegral
      , benchE "int64BE"    $ E.int64BE    #. fromIntegral

      , benchE "word16BE"   $ E.word16BE   #. fromIntegral
      , benchE "word32BE"   $ E.word32BE   #. fromIntegral
      , benchE "word64BE"   $ E.word64BE   #. fromIntegral

      , benchE "floatBE"    $ E.floatBE    #. fromIntegral
      , benchE "doubleBE"   $ E.doubleBE   #. fromIntegral

      -- little-endian
      , benchE "int16LE"    $ E.int16LE    #. fromIntegral
      , benchE "int32LE"    $ E.int32LE    #. fromIntegral
      , benchE "int64LE"    $ E.int64LE    #. fromIntegral

      , benchE "word16LE"   $ E.word16LE   #. fromIntegral
      , benchE "word32LE"   $ E.word32LE   #. fromIntegral
      , benchE "word64LE"   $ E.word64LE   #. fromIntegral

      , benchE "floatLE"    $ E.floatLE    #. fromIntegral
      , benchE "doubleLE"   $ E.doubleLE   #. fromIntegral

      -- host-dependent
      , benchE "int16Host"  $ E.int16Host  #. fromIntegral
      , benchE "int32Host"  $ E.int32Host  #. fromIntegral
      , benchE "int64Host"  $ E.int64Host  #. fromIntegral
      , benchE "intHost"    $ E.intHost    #. fromIntegral

      , benchE "word16Host" $ E.word16Host #. fromIntegral
      , benchE "word32Host" $ E.word32Host #. fromIntegral
      , benchE "word64Host" $ E.word64Host #. fromIntegral
      , benchE "wordHost"   $ E.wordHost   #. fromIntegral

      , benchE "floatHost"  $ E.floatHost  #. fromIntegral
      , benchE "doubleHost" $ E.doubleHost #. fromIntegral
      ]

    , bgroup "Data.ByteString.Lazy.Builder.BoundedEncoding.Utf8"
      [ benchE "char"        $ Utf8.char        #. toEnum
      -- decimal number
      , benchE "int8Dec"     $ Utf8.int8Dec     #. fromIntegral
      , benchE "int16Dec"    $ Utf8.int16Dec    #. fromIntegral
      , benchE "int32Dec"    $ Utf8.int32Dec    #. fromIntegral
      , benchE "int64Dec"    $ Utf8.int64Dec    #. fromIntegral
      , benchE "intDec"      $ Utf8.intDec      #. fromIntegral

      , benchE "word8Dec"    $ Utf8.word8Dec    #. fromIntegral
      , benchE "word16Dec"   $ Utf8.word16Dec   #. fromIntegral
      , benchE "word32Dec"   $ Utf8.word32Dec   #. fromIntegral
      , benchE "word64Dec"   $ Utf8.word64Dec   #. fromIntegral
      , benchE "wordDec"     $ Utf8.wordDec     #. fromIntegral

      -- decimal number
      , benchE "word8Hex"    $ Utf8.word8Hex    #. fromIntegral
      , benchE "word16Hex"   $ Utf8.word16Hex   #. fromIntegral
      , benchE "word32Hex"   $ Utf8.word32Hex   #. fromIntegral
      , benchE "word64Hex"   $ Utf8.word64Hex   #. fromIntegral
      , benchE "wordHex"     $ Utf8.wordHex     #. fromIntegral

      -- fixed-width hexadecimal numbers
      , benchE "int8HexFixed"     $ Utf8.int8HexFixed     #. fromIntegral
      , benchE "int16HexFixed"    $ Utf8.int16HexFixed    #. fromIntegral
      , benchE "int32HexFixed"    $ Utf8.int32HexFixed    #. fromIntegral
      , benchE "int64HexFixed"    $ Utf8.int64HexFixed    #. fromIntegral

      , benchE "word8HexFixed"    $ Utf8.word8HexFixed    #. fromIntegral
      , benchE "word16HexFixed"   $ Utf8.word16HexFixed   #. fromIntegral
      , benchE "word32HexFixed"   $ Utf8.word32HexFixed   #. fromIntegral
      , benchE "word64HexFixed"   $ Utf8.word64HexFixed   #. fromIntegral

      , benchE "floatHexFixed"    $ Utf8.floatHexFixed    #. fromIntegral
      , benchE "doubleHexFixed"   $ Utf8.doubleHexFixed   #. fromIntegral
      ]
    ]
