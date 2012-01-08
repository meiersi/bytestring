{-# LANGUAGE PackageImports, ScopedTypeVariables, BangPatterns #-}
-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Benchmark all 'Builder' functions.
module Main (main) where

import           Prelude hiding (words)
import           Criterion.Main
import           Data.Foldable (foldMap)
import           Data.Monoid

import qualified Data.ByteString                  as S
import qualified Data.ByteString.Lazy             as L

import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Builder.Internal (ensureFree)
import           Data.ByteString.Lazy.Builder.ASCII
import           Data.ByteString.Lazy.Builder.Extras
import           Data.ByteString.Lazy.Builder.BasicEncoding
                   ( FixedEncoding, BoundedEncoding, (>$<), pairF, fromF )
import qualified Data.ByteString.Lazy.Builder.BasicEncoding          as E
import qualified Data.ByteString.Lazy.Builder.BasicEncoding.Internal as EI

import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.Text               as Blaze
import qualified "bytestring" Data.ByteString.Lazy as OldL

import Foreign

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
intData = [start..start +nRepl]
  where start = 0

{-# NOINLINE smallIntegerData #-}
smallIntegerData :: [Integer]
smallIntegerData = map fromIntegral intData

{-# NOINLINE largeIntegerData #-}
largeIntegerData :: [Integer]
largeIntegerData = map (* (10 ^ (100 :: Integer))) smallIntegerData

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

{-# NOINLINE byteStringChunksData #-}
byteStringChunksData :: [S.ByteString]
byteStringChunksData = map (S.pack . replicate 500 . fromIntegral) intData


-- benchmark wrappers
---------------------

{-# INLINE benchBlaze #-}
benchBlaze :: String -> a -> (a -> Blaze.Builder) -> Benchmark
benchBlaze name x b =
    bench (name ++" (" ++ show nRepl ++ ")") $
        whnf (OldL.length . Blaze.toLazyByteString . b) x

{-# INLINE benchB' #-}
benchB' :: String -> a -> (a -> Builder) -> Benchmark
benchB' name x b = bench name $ whnf (L.length . toLazyByteString . b) x

{-# INLINE benchB #-}
benchB :: String -> a -> (a -> Builder) -> Benchmark
benchB name = benchB' $ name ++" (" ++ show nRepl ++ ")"

{-# INLINE benchBInts #-}
benchBInts :: String -> ([Int] -> Builder) -> Benchmark
benchBInts name = benchB name intData

-- | Benchmark a 'FixedEncoding'. Full inlining to enable specialization.
{-# INLINE benchFE #-}
benchFE :: String -> FixedEncoding Int -> Benchmark
benchFE name = benchBE name . E.fromF

-- | Benchmark a 'BoundedEncoding'. Full inlining to enable specialization.
{-# INLINE benchBE #-}
benchBE :: String -> BoundedEncoding Int -> Benchmark
benchBE name e =
    bench (name ++" (" ++ show nRepl ++ ")") $ benchIntEncodingB nRepl e


-- We use this construction of just looping through @n,n-1,..,1@ to ensure that
-- we measure the speed of the encoding and not the speed of generating the
-- values to be encoded.
{-# INLINE benchIntEncodingB #-}
benchIntEncodingB :: Int                  -- ^ Maximal 'Int' to write
                  -> BoundedEncoding Int  -- ^ 'BoundedEncoding' to execute
                  -> IO ()                -- ^ 'IO' action to benchmark
benchIntEncodingB n0 w
  | n0 <= 0   = return ()
  | otherwise = do
      fpbuf <- mallocForeignPtrBytes (n0 * EI.sizeBound w)
      withForeignPtr fpbuf (loop n0) >> return ()
  where
    loop !n !op
      | n <= 0    = return op
      | otherwise = EI.runB w n op >>= loop (n - 1)

-- | Benchmark a 'PaddedEncoding'. Full inlining to enable specialization.
{-# INLINE benchPE #-}
benchPE :: String -> PaddedSizeEncoding-> Benchmark
benchPE name mkEncoding =
    bench (name ++" (" ++ show nRepl ++ ")") $ io
  where
    maxSize = EI.size $ mkEncoding maxBound

    io | nRepl < 0 = error "benchPE: negative number of replications"
       | otherwise = do
           fpbuf <- mallocForeignPtrBytes (nRepl * maxSize)
           withForeignPtr fpbuf (loop (fromIntegral nRepl)) >> return ()

    loop !n !op
      | n <= 0    = return op
      | otherwise = EI.runB (fromF (mkEncoding n)) n op >>= loop (n - 1)


-- benchmarks
-------------

sanityCheckInfo :: [String]
sanityCheckInfo =
  [ "Sanity checks:"
  , " lengths of input data: " ++ show
      [ length intData, length floatData, length doubleData
      , length smallIntegerData, length largeIntegerData
      , S.length byteStringData, fromIntegral (L.length lazyByteStringData)
      ]
  ]

{-# NOINLINE encodeChunkedBase128LE #-}
encodeChunkedBase128LE :: Builder -> Builder
encodeChunkedBase128LE = encodeChunked word64Base128LEPadded E.emptyB

{-# NOINLINE encodeSizePrefixedBase128LE #-}
encodeSizePrefixedBase128LE :: Builder -> Builder
encodeSizePrefixedBase128LE =
    encodeSizePrefixed defaultUntrimmedStrategy word64Base128LEPadded

{-# NOINLINE encodeChunkedHex #-}
encodeChunkedHex :: Builder -> Builder
encodeChunkedHex = encodeChunked (word64HexPadded ' ') E.emptyB

{-# NOINLINE encodeSizePrefixedHex #-}
encodeSizePrefixedHex :: Builder -> Builder
encodeSizePrefixedHex =
    encodeSizePrefixed defaultUntrimmedStrategy (word64HexPadded ' ')

{-# NOINLINE encodeChunkedDec #-}
encodeChunkedDec :: Builder -> Builder
encodeChunkedDec = encodeChunked (word64DecPadded ' ') E.emptyB

{-# NOINLINE encodeSizePrefixedDec #-}
encodeSizePrefixedDec :: Builder -> Builder
encodeSizePrefixedDec =
    encodeSizePrefixed defaultUntrimmedStrategy (word64DecPadded ' ')

{-# NOINLINE encodeChunkedIntHost #-}
encodeChunkedIntHost :: Builder -> Builder
encodeChunkedIntHost = encodeChunked (const E.word64Host) E.emptyB

{-# NOINLINE encodeSizePrefixedIntHost #-}
encodeSizePrefixedIntHost :: Builder -> Builder
encodeSizePrefixedIntHost =
    encodeSizePrefixed defaultUntrimmedStrategy (const E.word64Host)

{-# NOINLINE toLazyByteStringUntrimmed #-}
toLazyByteStringUntrimmed :: Builder -> L.ByteString
toLazyByteStringUntrimmed = toLazyByteStringWith defaultUntrimmedStrategy L.empty

defaultUntrimmedStrategy = untrimmedStrategy smallChunkSize defaultChunkSize

main :: IO ()
main = do
  mapM_ putStrLn sanityCheckInfo
  putStrLn ""
  Criterion.Main.defaultMain
    [ bgroup "Builder"
      [ bgroup "Small payload"
        [ benchB' "mempty"        ()  (const mempty)
        , benchB' "ensureFree 8"  ()  (const (ensureFree 8))
        , benchB' "intHost 1"     1   intHost

        , benchB' "encodeChunkedIntHost . intHost $ 1" 1 
            (encodeChunkedIntHost . intHost)
        , benchB' "encodeSizePrefixed . intHost $ 1" 1 
            (encodeSizePrefixedIntHost . intHost)

        , benchB' "encodeChunkedBase128LE intHost $ 1" 1 
            (encodeChunkedBase128LE . intHost)
        , benchB' "encodeSizePrefixedBase128LE intHost $ 1" 1 
            (encodeSizePrefixedBase128LE . intHost)

        , benchB' "encodeChunkedDec intHost $ 1" 1 
            (encodeChunkedDec . intHost)
        , benchB' "encodeSizePrefixedDec intHost $ 1" 1 
            (encodeSizePrefixedDec . intHost)

        , benchB' "encodeChunkedHex intHost $ 1" 1 
            (encodeChunkedHex . intHost)
        , benchB' "encodeSizePrefixedHex intHost $ 1" 1 
            (encodeSizePrefixedHex . intHost)
        ]

      , bgroup "Encoding wrappers"
        [ benchBInts "foldMap word8" $
            foldMap (word8 . fromIntegral)
        , benchBInts "encodeListWithF word8" $
            E.encodeListWithF (fromIntegral >$< E.word8)
        , benchB     "encodeUnfoldrWithF word8" nRepl $
            E.encodeUnfoldrWithF (fromIntegral >$< E.word8) countToZero
        , benchB     "encodeByteStringWithF word8" byteStringData $
            E.encodeByteStringWithF E.word8
        , benchB     "encodeLazyByteStringWithF word8" lazyByteStringData $
            E.encodeLazyByteStringWithF E.word8

        , benchBInts "foldMap (encodeChunkedIntHost . intHost)"
            (foldMap (encodeChunkedIntHost . intHost))
        , benchBInts "foldMap (encodeSizePrefixedIntHost . intHost)"
            (foldMap (encodeSizePrefixedIntHost . intHost))

        , benchBInts "foldMap (encodeChunkedBase128LE . intHost)"
            (foldMap (encodeChunkedBase128LE . intHost))
        , benchBInts "foldMap (encodeSizePrefixedBase128LE . intHost)"
            (foldMap (encodeSizePrefixedBase128LE . intHost))
         
        , benchBInts "foldMap (encodeChunkedHex . intHost)"
            (foldMap (encodeChunkedHex . intHost))
        , benchBInts "foldMap (encodeSizePrefixedHex . intHost)"
            (foldMap (encodeSizePrefixedHex . intHost))

        , benchBInts "foldMap (encodeChunkedDec . intHost)"
            (foldMap (encodeChunkedDec . intHost))
        , benchBInts "foldMap (encodeSizePrefixedDec . intHost)"
            (foldMap (encodeSizePrefixedDec . intHost))
        ]

      , bgroup "ByteString insertion" $
          let dataName = " byteStringChunks" ++ 
                         show (S.length (head byteStringChunksData)) ++ "Data"
          in
            [ benchB ("foldMap byteString" ++ dataName) byteStringChunksData
                (foldMap byteString)
            , benchB ("foldMap byteStringCopy" ++ dataName) byteStringChunksData
                (foldMap byteStringCopy)
            , benchB ("foldMap byteStringInsert" ++ dataName) byteStringChunksData
                (foldMap byteStringInsert)
            ]

      , bgroup "Non-bounded encodings"
        [ benchB "foldMap intDec"                                 intData            $ foldMap intDec
          -- Note that the small data corresponds to the intData pre-converted
          -- to Integer.
        , benchB "foldMap integerDec (small)"                     smallIntegerData        $ foldMap integerDec
        , benchB "foldMap integerDec (large)"                     largeIntegerData        $ foldMap integerDec
        , benchBlaze "foldMap integerDec (small) (blaze-textual)" smallIntegerData        $ foldMap Blaze.integral
        , benchBlaze "foldMap integerDec (large) (blaze-textual)" largeIntegerData        $ foldMap Blaze.integral
        , benchB "foldMap floatDec"        floatData          $ foldMap floatDec
        , benchB "foldMap doubleDec"       doubleData         $ foldMap doubleDec
        , benchBlaze "foldMap floatDec (blaze-textual)"       floatData          $ foldMap Blaze.float
        , benchBlaze "foldMap doubleDec (blaze-textual)"      doubleData         $ foldMap Blaze.double
        , benchB "byteStringHexFixed"      byteStringData     $ byteStringHexFixed
        , benchB "lazyByteStringHexFixed"  lazyByteStringData $ lazyByteStringHexFixed

        ]
      ]

    , bgroup "BasicEncoding"
      [ benchFE "char7"      $ toEnum       >$< E.char7
      , benchFE "char8"      $ toEnum       >$< E.char8
      , benchBE "charUtf8"   $ toEnum       >$< E.charUtf8

      -- binary encoding
      , benchFE "int8"       $ fromIntegral >$< E.int8
      , benchFE "word8"      $ fromIntegral >$< E.word8

      -- big-endian
      , benchFE "int16BE"    $ fromIntegral >$< E.int16BE
      , benchFE "int32BE"    $ fromIntegral >$< E.int32BE
      , benchFE "int64BE"    $ fromIntegral >$< E.int64BE

      , benchFE "word16BE"   $ fromIntegral >$< E.word16BE
      , benchFE "word32BE"   $ fromIntegral >$< E.word32BE
      , benchFE "word64BE"   $ fromIntegral >$< E.word64BE

      , benchFE "floatBE"    $ fromIntegral >$< E.floatBE
      , benchFE "doubleBE"   $ fromIntegral >$< E.doubleBE

      -- little-endian
      , benchFE "int16LE"    $ fromIntegral >$< E.int16LE
      , benchFE "int32LE"    $ fromIntegral >$< E.int32LE
      , benchFE "int64LE"    $ fromIntegral >$< E.int64LE

      , benchFE "word16LE"   $ fromIntegral >$< E.word16LE
      , benchFE "word32LE"   $ fromIntegral >$< E.word32LE
      , benchFE "word64LE"   $ fromIntegral >$< E.word64LE

      , benchFE "floatLE"    $ fromIntegral >$< E.floatLE
      , benchFE "doubleLE"   $ fromIntegral >$< E.doubleLE

      -- host-dependent
      , benchFE "int16Host"  $ fromIntegral >$< E.int16Host
      , benchFE "int32Host"  $ fromIntegral >$< E.int32Host
      , benchFE "int64Host"  $ fromIntegral >$< E.int64Host
      , benchFE "intHost"    $ fromIntegral >$< E.intHost

      , benchFE "word16Host" $ fromIntegral >$< E.word16Host
      , benchFE "word32Host" $ fromIntegral >$< E.word32Host
      , benchFE "word64Host" $ fromIntegral >$< E.word64Host
      , benchFE "wordHost"   $ fromIntegral >$< E.wordHost

      , benchFE "floatHost"  $ fromIntegral >$< E.floatHost
      , benchFE "doubleHost" $ fromIntegral >$< E.doubleHost

      -- variable-length, little-endian, base-128
      , benchBE "word8Base128LE"  (fromIntegral >$< E.word8Base128LE)
      , benchBE "word16Base128LE" (fromIntegral >$< E.word16Base128LE)
      , benchBE "word32Base128LE" (fromIntegral >$< E.word32Base128LE)
      , benchBE "word64Base128LE" (fromIntegral >$< E.word64Base128LE)
      , benchBE "wordBase128LE"   (fromIntegral >$< E.wordBase128LE)

        -- Strange: in this benchmark it is faster than 'word64Base128LE'.
        -- However, when serializing a list, then it is as much slower as
        -- expected. Perhaps, some unboxing works here that doesn't otherwise.
      , benchPE "word64Base128LEPadded"   word64Base128LEPadded

      , benchBE "int8ZigZagBase128LE"  (fromIntegral >$< E.int8ZigZagBase128LE)
      , benchBE "int16ZigZagBase128LE" (fromIntegral >$< E.int16ZigZagBase128LE)
      , benchBE "int32ZigZagBase128LE" (fromIntegral >$< E.int32ZigZagBase128LE)
      , benchBE "int64ZigZagBase128LE" (fromIntegral >$< E.int64ZigZagBase128LE)
      , benchBE "intZigZagBase128LE"   (fromIntegral >$< E.intZigZagBase128LE)
      ]

    , bgroup "Data.ByteString.Lazy.Builder.BoundedEncoding.ASCII"
      [
      -- decimal number
        benchBE "int8Dec"     $ fromIntegral >$< E.int8Dec
      , benchBE "int16Dec"    $ fromIntegral >$< E.int16Dec
      , benchBE "int32Dec"    $ fromIntegral >$< E.int32Dec
      , benchBE "int64Dec"    $ fromIntegral >$< E.int64Dec
      , benchBE "intDec"      $ fromIntegral >$< E.intDec

      , benchBE "word8Dec"    $ fromIntegral >$< E.word8Dec
      , benchBE "word16Dec"   $ fromIntegral >$< E.word16Dec
      , benchBE "word32Dec"   $ fromIntegral >$< E.word32Dec
      , benchBE "word64Dec"   $ fromIntegral >$< E.word64Dec
      , benchBE "wordDec"     $ fromIntegral >$< E.wordDec

      , benchPE "word64DecPadded"         (word64DecPadded '0')

      -- hexadecimal number
      , benchBE "word8Hex"    $ fromIntegral >$< E.word8Hex
      , benchBE "word16Hex"   $ fromIntegral >$< E.word16Hex
      , benchBE "word32Hex"   $ fromIntegral >$< E.word32Hex
      , benchBE "word64Hex"   $ fromIntegral >$< E.word64Hex
      , benchBE "wordHex"     $ fromIntegral >$< E.wordHex

      , benchPE "word64HexPadded"         (word64HexPadded '0')

      -- fixed-width hexadecimal numbers
      , benchFE "int8HexFixed"     $ fromIntegral >$< E.int8HexFixed
      , benchFE "int16HexFixed"    $ fromIntegral >$< E.int16HexFixed
      , benchFE "int32HexFixed"    $ fromIntegral >$< E.int32HexFixed
      , benchFE "int64HexFixed"    $ fromIntegral >$< E.int64HexFixed

      , benchFE "word8HexFixed"    $ fromIntegral >$< E.word8HexFixed
      , benchFE "word16HexFixed"   $ fromIntegral >$< E.word16HexFixed
      , benchFE "word32HexFixed"   $ fromIntegral >$< E.word32HexFixed
      , benchFE "word64HexFixed"   $ fromIntegral >$< E.word64HexFixed

      , benchFE "floatHexFixed"    $ fromIntegral >$< E.floatHexFixed
      , benchFE "doubleHexFixed"   $ fromIntegral >$< E.doubleHexFixed
      ]
    ]
