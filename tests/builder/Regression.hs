{-# LANGUAGE PackageImports, CPP, BangPatterns, MonoPatBinds #-}
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

import qualified "bytestring" Data.ByteString.Lazy as OldL

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


------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

newBS, oldBS, blaze :: String
newBS = "new-bytestring"
oldBS = "old-bytestring"
blaze = "blaze"

main :: IO ()
main = defaultMain
    [ benchmark "filter ((0 ==) . (`mod` 2)) on a LBS"
        [ (newBS, NewL.length . NewL.filter ((0 ==) . (`mod` 2)) . snd)
        , (oldBS, OldL.length . OldL.filter ((0 ==) . (`mod` 2)) . fst)
        ]
        lbsInput
    , benchmark "map (+1) on a LBS"
        [ (newBS, NewL.length . NewL.map (+1) . snd)
        , (oldBS, OldL.length . OldL.map (+1) . fst)
        ]
        lbsInput
    , benchmark ("packing [Word8] " ++ show n)
        [ (newBS, NewL.length . NewL.pack)
        , (oldBS, OldL.length . OldL.pack)
        , (blaze, OldL.length . Blaze.toLazyByteString . Blaze.fromWord8s)
        ]
        word8Input
    ]
  where
    benchmark gname tasks xs =
        bgroup gname [ bgroup xname $ map (mkBench x) tasks | (xname, x) <- xs ]
      where
        mkBench x (name, f) = bench name $ whnf f x

