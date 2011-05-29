{-# LANGUAGE PackageImports #-}
-- |
-- Copyright   : (c) 2010-2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Performance and compatibility regression tests for the Builder.
--
module Regression where

import qualified "blaze-builder" Blaze.ByteString.Builder as Blaze

import qualified "new-bytestring" Data.ByteString.Lazy    as NewL
import qualified "new-bytestring" Data.ByteString.Builder as NewL
import qualified "new-bytestring" Data.ByteString.Builder.Char.Utf8 as NewL

import qualified "bytestring" Data.ByteString.Lazy as OldL
import qualified Data.ByteString.Base16.Lazy       as OldBase16

import Foreign
import Criterion.Main

------------------------------------------------------------------------------
-- Benchmark Data
------------------------------------------------------------------------------

n :: Int
n = 10000

nShort :: Int
nShort = 64

{-# NOINLINE word8Input #-}
word8Input :: [(String, [Word8])]
word8Input = 
  [ (show $ length word8s,      word8s)
  , (show $ length shortWord8s, shortWord8s)
  ]

word8s, shortWord8s :: [Word8]
word8s      = take n $ map fromIntegral $ [(0::Int)..]
shortWord8s = take nShort word8s

{-# NOINLINE lbsInput #-}
lbsInput :: [(String, (OldL.ByteString, NewL.ByteString))]
lbsInput = 
  [ (show n,      longLBS)
  , (show nShort, shortLBS)
  ]
  where
    longLBS  = (OldL.pack word8s, NewL.pack word8s)
    shortLBS = (OldL.pack shortWord8s, NewL.pack shortWord8s)

{-# NOINLINE intInput #-}
intInput :: [(String, Int)]
intInput = [ (show n, n), (show nShort, nShort) ]

countToZero :: Int -> Maybe (Word8, Int)
countToZero 0 = Nothing
countToZero i = Just (fromIntegral i, i - 1)

{-# NOINLINE splitAtInput #-}
splitAtInput :: [(String, ((Int64, OldL.ByteString), (Int64, NewL.ByteString)))]
splitAtInput = 
  [ (show n,      ((n',      oldLBS), (n',      newLBS)))
  , (show nShort, ((nShort', oldLBS), (nShort', newLBS)))
  ]
  where
    n'      = fromIntegral n
    nShort' = fromIntegral nShort
    newLBS  = NewL.cycle $ NewL.pack [0..100]
    oldLBS  = OldL.cycle $ OldL.pack [0..100]


------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

newBS :: ([Char], NewL.ByteString -> Int64, NewL.ByteString -> [Word8])
newBS = ("new-bytestring",  NewL.length, NewL.unpack)

oldBS :: ([Char], OldL.ByteString -> Int64, OldL.ByteString -> [Word8])
oldBS = ("old-bytestring",  OldL.length, OldL.unpack)

base16BS :: ([Char], OldL.ByteString -> Int64, OldL.ByteString -> [Word8])
base16BS = ("base16-bytestring",  OldL.length, OldL.unpack)

blaze :: ([Char], Blaze.Builder -> Int64, Blaze.Builder -> [Word8])
blaze = ( "blaze"
        , OldL.length . Blaze.toLazyByteString
        , OldL.unpack . Blaze.toLazyByteString )

benchmarks :: [Benchmark]
testResults :: [String]
(benchmarks, testResults) = unzip $ 
    [ comparison "splitAt on LBS with chunksize 100" $
        [ impl newBS (fst . uncurry NewL.splitAt . snd) splitAtInput
        , impl oldBS (uncurry OldL.take . fst) splitAtInput
        ]
    , comparison "unfoldr countToZero starting from" $
        [ impl newBS (NewL.unfoldr countToZero) intInput
        , impl oldBS (OldL.unfoldr countToZero) intInput
        ]
     , comparison "base16 encoding of a LBS" $
        [ impl newBS (NewL.toLazyByteString . NewL.hexLower . snd) lbsInput
        , impl base16BS (OldBase16.encode . fst) lbsInput
        ]
    , comparison "filter ((0 ==) . (`mod` 2)) on a LBS"
        [ impl newBS (NewL.filter ((0 ==) . (`mod` 2)) . snd) lbsInput
        , impl oldBS (OldL.filter ((0 ==) . (`mod` 2)) . fst) lbsInput
        ]
    , comparison "map (+1) on a LBS"
        [ impl newBS (NewL.map (+1) . snd) lbsInput
        , impl oldBS (OldL.map (+1) . fst) lbsInput
        ]
    , comparison "pack [Word8]"
        [ impl newBS NewL.pack word8Input
        , impl oldBS OldL.pack word8Input
        , impl blaze Blaze.fromWord8s word8Input
        ]
    ]
  where
    comparison :: Eq d => String -> [([Benchmark], [d])] -> (Benchmark, String)
    comparison name xs =
        ( bgroup name $ concat bs
        , (if success then "OK" else "FAIL" ) ++ ". " ++ name
        )
      where
        (bs, ts) = unzip xs
        success  = and $ zipWith (==) ts $ tail ts

    impl :: (String, b -> c, b -> d) -> (a -> b) -> [(String, a)] 
          -> ([Benchmark], [d])
    impl (name, fBench, fTest) f inputs = 
      ( do (inputName, x) <- inputs
           return $ bgroup name [bench inputName $ whnf (fBench . f) x]
      , map (fTest . f . snd) inputs
      )


main :: IO ()
main = do
  putStrLn ""
  putStrLn "Comparing results of different implementations:"
  putStrLn $ unlines $ map ("  " ++) testResults
  putStrLn "Benchmarking:"
  defaultMain benchmarks

