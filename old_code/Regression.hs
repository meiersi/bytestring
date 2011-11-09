{-# LANGUAGE PackageImports #-}
-- |
-- Copyright   : (c) 2010-2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Performance and compatibility regression tests
-- for the Builder and changes to Data.ByteString.Lazy.
--
module Regression where

-- NOTE: The regression testing does not work currrently, as different versions
-- of the same package (in this case bytestring-0.9.3 and bytestring-0.9.2) cannot
-- be loaded at the same time. The code is kept for later use/modification.

import qualified "blaze-builder" Blaze.ByteString.Builder as Blaze

import qualified "new-bytestring" Data.ByteString.Lazy                       as NewL
import qualified "new-bytestring" Data.ByteString.Lazy.Builder               as NewL
import qualified "new-bytestring" Data.ByteString.Lazy.Builder.ASCII         as NewL
import qualified "new-bytestring" Data.ByteString.Lazy.Builder.Extras        as NewL
import qualified "new-bytestring" Data.ByteString.Lazy.Builder.BasicEncoding as E

import qualified "bytestring" Data.ByteString.Lazy as OldL
import qualified Data.ByteString.Base16.Lazy       as OldBase16


import Foreign
import Criterion.Main
import Data.Monoid

------------------------------------------------------------------------------
-- Benchmark Data
------------------------------------------------------------------------------

n :: Int
n = 10000

-- | Note that 'blaze-builder' is twice as fast for short bytestrings, because
-- it uses 'inlinePerformIO', while the 'bytestring' builder uses
-- 'unsafePerformIO'. The latter is the right approach, as it guarantees that
-- buildsteps will be called at most once and can therefore make destructive
-- use of ressources. Moreover, the design of the 'bytestring' builder
-- execution is such that it works without any 'unsafePerformIO', if the
-- builder is provided with an IO context.
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
    newLBS  = NewL.take (n' + nShort') $ NewL.cycle $ NewL.pack [0..100]
    oldLBS  = OldL.take (n' + nShort') $ OldL.cycle $ OldL.pack [0..100]


------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

newBS :: ([Char], NewL.ByteString -> Int64, NewL.ByteString -> [Word8])
newBS = ("new-bytestring",  NewL.length, NewL.unpack)

newBuilder :: ([Char], NewL.Builder -> Int64, NewL.Builder -> [Word8])
newBuilder = ( "new-builder"
        , NewL.length . NewL.toLazyByteString
        , NewL.unpack . NewL.toLazyByteString )


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
    [ comparison "encodeWith `mappend` encodeWith" $
        [ impl newBuilder (mconcat . map (\x -> NewL.word8 x `mappend` NewL.word8 x)) word8Input
        , impl blaze      (mconcat . map (\x -> Blaze.fromWord8 x `mappend` Blaze.fromWord8 x)) word8Input
        ]
    , comparison "drop on LBS with chunksize 100" $
        [ impl newBS (uncurry NewL.drop . snd) splitAtInput
        , impl oldBS (uncurry OldL.drop . fst) splitAtInput
        ]
    , comparison "take on LBS with chunksize 100" $
        [ impl newBS (uncurry NewL.take . snd) splitAtInput
        , impl oldBS (uncurry OldL.take . fst) splitAtInput
        ]
    , comparison "splitAt on LBS with chunksize 100" $
        [ impl newBS (fst . uncurry NewL.splitAt . snd) splitAtInput
        , impl oldBS (fst . uncurry OldL.splitAt . fst) splitAtInput
        ]
    , comparison "unfoldr countToZero starting from" $
        [ impl newBS (NewL.unfoldr countToZero) intInput
        , impl oldBS (OldL.unfoldr countToZero) intInput
        ]
     , comparison "base16 encoding of a LBS" $
        [ impl newBS (NewL.toLazyByteString . NewL.lazyByteStringHexFixed . snd) lbsInput
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

