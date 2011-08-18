{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Testing all encodings provided by this library.
module TestAll (main) where

import Data.Bits
import Data.Char (ord)

import Control.Monad

import Foreign

import Numeric (showHex)

import           Data.ByteString.Lazy.Builder.BoundedEncoding (Encoding)
import qualified Data.ByteString.Lazy.Builder.BoundedEncoding      as E
import qualified Data.ByteString.Lazy.Builder.BoundedEncoding.Utf8 as EUtf8
import Codec.Bounded.Encoding.Internal.Test (cmpEncodingErr)

import System.ByteOrder  -- from byteorder-1.0.3

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit.Lang (assertFailure)
import Test.QuickCheck (Arbitrary)

import Unsafe.Coerce (unsafeCoerce)


main :: IO ()
main = Test.Framework.defaultMain $ return $ testAll

------------------------------------------------------------------------------
-- Additional testing infrastructure
------------------------------------------------------------------------------

-- | Quickcheck test that includes a check that the property holds on the
-- bounds of a bounded value.
testBoundedProperty :: forall a. (Arbitrary a, Show a, Bounded a) 
                    => String -> (a -> Bool) -> Test
testBoundedProperty name p = testGroup name
  [ testProperty "arbitrary" p
  , testCase "bounds" $ do
      unless (p (minBound :: a)) $ assertFailure "minBound"
      unless (p (maxBound :: a)) $ assertFailure "maxBound"
  ]

------------------------------------------------------------------------------
-- Test-Cases
------------------------------------------------------------------------------

testAll :: Test
testAll = testGroup "bytestring"
  [ testBuilder
  , testBoundedEncoding
  , testBoundedEncodingUtf8
  ]

-- Builder
----------

testBuilder :: Test
testBuilder = testGroup "Data.ByteString.Lazy.Builder" []

-- BoundedEncoding
------------------

testBoundedEncoding :: Test
testBoundedEncoding = testGroup "Data.ByteString.Lazy.Builder.BoundedEncoding"
  [ 
  -- ascii
    testBoundedProperty "char8"     $ prop_char8        E.char8
  
  -- binary
  , testBoundedProperty "word8"     $ prop_bigEndian    E.word8
  , testBoundedProperty "int8"      $ prop_bigEndian    E.int8

  --  big-endian
  , testBoundedProperty "int16BE"   $ prop_bigEndian    E.int16BE
  , testBoundedProperty "int32BE"   $ prop_bigEndian    E.int32BE
  , testBoundedProperty "int64BE"   $ prop_bigEndian    E.int64BE
                              
  , testBoundedProperty "word16BE"   $ prop_bigEndian    E.word16BE
  , testBoundedProperty "word32BE"   $ prop_bigEndian    E.word32BE
  , testBoundedProperty "word64BE"   $ prop_bigEndian    E.word64BE

  , testProperty "floatLE"    $ prop_Float  littleEndianList E.floatLE
  , testProperty "doubleLE"   $ prop_Double littleEndianList E.doubleLE

  --  little-endian
  , testBoundedProperty "int16LE"   $ prop_littleEndian E.int16LE
  , testBoundedProperty "int32LE"   $ prop_littleEndian E.int32LE
  , testBoundedProperty "int64LE"   $ prop_littleEndian E.int64LE
                              
  , testBoundedProperty "word16LE"   $ prop_littleEndian E.word16LE
  , testBoundedProperty "word32LE"   $ prop_littleEndian E.word32LE
  , testBoundedProperty "word64LE"   $ prop_littleEndian E.word64LE

  , testProperty "floatBE"    $ prop_Float bigEndianList    E.floatBE
  , testProperty "doubleBE"   $ prop_Double bigEndianList   E.doubleBE

  --  host dependent
  , testBoundedProperty "int16Host" $ prop_hostEndian  E.int16Host
  , testBoundedProperty "int32Host" $ prop_hostEndian  E.int32Host
  , testBoundedProperty "int64Host" $ prop_hostEndian  E.int64Host
  , testBoundedProperty "intHost"   $ prop_hostEndian  E.intHost
                              
  , testBoundedProperty "word16Host" $ prop_hostEndian  E.word16Host
  , testBoundedProperty "word32Host" $ prop_hostEndian  E.word32Host
  , testBoundedProperty "word64Host" $ prop_hostEndian  E.word64Host
  , testBoundedProperty "wordHost"   $ prop_hostEndian  E.wordHost

  , testProperty "floatHost"   $ prop_Float  hostEndianList   E.floatHost
  , testProperty "doubleHost"  $ prop_Double hostEndianList   E.doubleHost
  ]

prop_char8 :: Encoding Char -> Char -> Bool
prop_char8 = cmpEncodingErr (return . fromIntegral . ord)

prop_bigEndian :: (Storable a, Bits a, Integral a) => Encoding a -> a -> Bool
prop_bigEndian = cmpEncodingErr bigEndianList

prop_littleEndian :: (Storable a, Bits a, Integral a) => Encoding a -> a -> Bool
prop_littleEndian = cmpEncodingErr littleEndianList

prop_hostEndian :: (Storable a, Bits a, Integral a) => Encoding a -> a -> Bool
prop_hostEndian = cmpEncodingErr hostEndianList

bigEndianList :: (Storable a, Bits a, Integral a) => a -> [Word8]
bigEndianList = reverse . littleEndianList

littleEndianList :: (Storable a, Bits a, Integral a) => a -> [Word8]
littleEndianList x = 
    map (fromIntegral . (x `shiftR`) . (8*)) $ [0..sizeOf x - 1]

hostEndianList :: (Storable a, Bits a, Integral a) => a -> [Word8]
hostEndianList = case byteOrder of
    LittleEndian -> littleEndianList
    BigEndian    -> bigEndianList
    _            -> error $ 
        "bounded-encoding: unsupported byteorder '" ++ show byteOrder ++ "'"


prop_Float :: (Word32 -> [Word8]) -> Encoding Float -> Float -> Bool
prop_Float f  = cmpEncodingErr (f . coerceFloatToWord32)

prop_Double :: (Word64 -> [Word8]) -> Encoding Double -> Double -> Bool
prop_Double f = cmpEncodingErr (f . coerceDoubleToWord64)

-- Note that the following use of unsafeCoerce is not guaranteed to be 
-- safe on GHC 7.0 and less. The reason is probably the following ticket:
--
--   http://hackage.haskell.org/trac/ghc/ticket/4092
--
-- However, that only applies if the value is loaded in a register. We
-- avoid this by coercing only boxed values and ensuring that they
-- remain boxed using a NOINLINE pragma.
-- 

-- | Coerce a 'Float' to a 'Word32'.
{-# NOINLINE coerceFloatToWord32 #-}
coerceFloatToWord32 :: Float -> Word32
coerceFloatToWord32 = unsafeCoerce

-- | Coerce a 'Double' to a 'Word64'.
{-# NOINLINE coerceDoubleToWord64 #-}
coerceDoubleToWord64 :: Double -> Word64
coerceDoubleToWord64 = unsafeCoerce

-- BoundedEncoding.Utf8
-----------------------

testBoundedEncodingUtf8 :: Test
testBoundedEncodingUtf8 = 
  testGroup "Data.ByteString.Lazy.Builder.BoundedEncoding.Utf8"
  [ testBoundedProperty "char" (cmpEncodingErr (encodeUtf8 . return) EUtf8.char)

  , testBoundedProperty "int8Dec"  $ prop_dec EUtf8.int8Dec
  , testBoundedProperty "int16Dec" $ prop_dec EUtf8.int16Dec
  , testBoundedProperty "int32Dec" $ prop_dec EUtf8.int32Dec
  , testBoundedProperty "int64Dec" $ prop_dec EUtf8.int64Dec
  , testBoundedProperty "intDec"   $ prop_dec EUtf8.intDec

  , testBoundedProperty "word8Dec"  $ prop_dec EUtf8.word8Dec
  , testBoundedProperty "word16Dec" $ prop_dec EUtf8.word16Dec
  , testBoundedProperty "word32Dec" $ prop_dec EUtf8.word32Dec
  , testBoundedProperty "word64Dec" $ prop_dec EUtf8.word64Dec
  , testBoundedProperty "wordDec"   $ prop_dec EUtf8.wordDec

  , testBoundedProperty "word8Hex"  $ prop_hex EUtf8.word8Hex
  , testBoundedProperty "word16Hex" $ prop_hex EUtf8.word16Hex
  , testBoundedProperty "word32Hex" $ prop_hex EUtf8.word32Hex
  , testBoundedProperty "word64Hex" $ prop_hex EUtf8.word64Hex
  , testBoundedProperty "wordHex"   $ prop_hex EUtf8.wordHex

  , testBoundedProperty "word8HexFixed"  $ prop_hexFixed EUtf8.word8HexFixed
  , testBoundedProperty "word16HexFixed" $ prop_hexFixed EUtf8.word16HexFixed
  , testBoundedProperty "word32HexFixed" $ prop_hexFixed EUtf8.word32HexFixed
  , testBoundedProperty "word64HexFixed" $ prop_hexFixed EUtf8.word64HexFixed
  -- TODO:
  -- , testProperty "floatHexFixed"  $ prop_hexFixed EUtf8.word64HexFixed
  -- , testProperty "doubleHexFixed" $ prop_hexFixed EUtf8.word64HexFixed
  ]


-- TODO: Test boundaries of decimal encoding.

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
--
-- Copied from 'utf8-string-0.3.6' to make tests self-contained. 
-- Copyright (c) 2007, Galois Inc. All rights reserved.
--
encodeUtf8 :: String -> [Word8]
encodeUtf8 = concatMap (map fromIntegral . go . ord)
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]

prop_dec :: Show a => Encoding a -> a -> Bool
prop_dec = cmpEncodingErr (encodeUtf8 . show)

prop_hex :: (Integral a, Show a) => Encoding a -> a -> Bool
prop_hex = cmpEncodingErr (encodeUtf8 . (\x -> showHex x ""))

prop_hexFixed :: (Storable a, Integral a, Show a) => Encoding a -> a -> Bool
prop_hexFixed = cmpEncodingErr f
  where
    f x      = encodeUtf8 $ pad (2 * sizeOf x) $ showHex x ""
    pad n cs = replicate (n - length cs) '0' ++ cs

