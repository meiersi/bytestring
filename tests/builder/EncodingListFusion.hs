{-# LANGUAGE PackageImports #-}
-- |
-- Copyright   : (c) 2010-2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Test on what rewrite rule is required to enable the user to ignore
-- 'encodeListWith'.
--
module EncodingListFusion where

import qualified "new-bytestring" Data.ByteString.Lazy as L

import Data.ByteString.Lazy.Builder
import Data.ByteString.Lazy.Builder.Internal
import Data.ByteString.Lazy.Builder.Extras

import qualified Codec.Bounded.Encoding as E

import Foreign
import Criterion.Main
import Data.Monoid

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


------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

newBuilder :: ([Char], Builder -> Int64, Builder -> [Word8])
newBuilder = ( "new-builder"
        , L.length . toLazyByteString
        , L.unpack . toLazyByteString )


{-# NOINLINE test1 #-}
test1 :: [Word8] -> Builder
-- test1 = mconcat . map word8
test1 = foldr (\x b -> append (word8 x) b) empty

{-# NOINLINE test2 #-}
test2 :: [Word8] -> Builder
test2 = encodeListWith E.word8

{-# RULES 
"foldr/encodeWith" forall w.
    foldr (\x b -> append (encodeWith w x) b) empty = encodeListWith w

"encodeWith/." forall (w::E.Encoding a) (f::b -> a).
    encodeWith w . f = encodeWith (w E.#. f)

 #-}

benchmarks :: [Benchmark]
testResults :: [String]
(benchmarks, testResults) = unzip $ 
    [ comparison "encodeListWith E.word8" $
        [ impl newBuilder test1 word8Input
        , impl newBuilder test2 word8Input
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
