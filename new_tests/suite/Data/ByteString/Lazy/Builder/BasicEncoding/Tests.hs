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

module Data.ByteString.Lazy.Builder.BasicEncoding.Tests (tests) where

import           Data.Char (ord)

import           Control.Monad

import qualified Data.ByteString.Lazy.Builder.BasicEncoding      as BE

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.Providers.HUnit
import           Test.HUnit.Lang (assertFailure)
import           Test.QuickCheck (Arbitrary(..))


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

tests :: [Test]
tests = 
  [ 
  -- ascii
    testBoundedProperty "char8"      prop_char8
  ]

-- BasicEncoding
------------------


prop_char8 :: Char -> Bool
prop_char8 c = BE.evalF BE.char8 c == [fromIntegral $ ord c]

{-
import Data.Bits
import Data.Char (ord)
import Data.Monoid
import Data.Foldable (asum, foldMap)

import Control.Monad
import Control.Applicative

import Numeric (showHex)

import Foreign

import qualified Data.DList      as D

import qualified "new-bytestring" Data.ByteString      as S
import qualified "new-bytestring" Data.ByteString.Lazy as L

import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Builder.Extras
import qualified Data.ByteString.Lazy.Builder.Utf8                 as Utf8
import           Data.ByteString.Lazy.Builder.basicEncoding (Encoding)
import qualified Data.ByteString.Lazy.Builder.basicEncoding      as E
import qualified Data.ByteString.Lazy.Builder.basicEncoding.Utf8 as EUtf8
import Codec.Bounded.Encoding.Internal.Test (cmpEncodingErr)

import System.ByteOrder 

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit.Lang (assertFailure)
import Test.QuickCheck (Arbitrary(..), oneof, choose, listOf, elements)
import Test.QuickCheck.Property (printTestCase)

import Unsafe.Coerce (unsafeCoerce)


main :: IO ()
main = Test.Framework.defaultMain $ return $ testAll


------------------------------------------------------------------------------
-- Test-Cases
------------------------------------------------------------------------------

testAll :: Test
testAll = testGroup "bytestring"
  [ testbasicEncoding
  , testbasicEncodingUtf8
  , testBuilder
  ]

-- basicEncoding
------------------

testbasicEncoding :: Test
testbasicEncoding = testGroup "Data.ByteString.Lazy.Builder.basicEncoding"
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

-- basicEncoding.Utf8
-----------------------

testbasicEncodingUtf8 :: Test
testbasicEncodingUtf8 = 
  testGroup "Data.ByteString.Lazy.Builder.basicEncoding.Utf8"
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
prop_hexFixed = cmpEncodingErr hexFixedList
  where

hexFixedList :: (Storable a, Integral a, Show a) => a -> [Word8]
hexFixedList x =
   encodeUtf8 $ pad (2 * sizeOf x) $ showHex x ""
 where
   pad n cs = replicate (n - length cs) '0' ++ cs


------------------------------------------------------------------------------
-- Testing chunk-handling.
------------------------------------------------------------------------------

testBuilder :: Test
testBuilder = 
    testProperty "Data.ByteString.Lazy.Builder" $ testRecipe <$> arbitrary
  where
    testRecipe r = 
        printTestCase msg $ x1 == x2
      where
        x1 = renderRecipe r
        x2 = buildRecipe r
        toString = show -- map (chr . fromIntegral)
        msg = unlines 
          [ "recipe: " ++ show r
          , "render: " ++ toString x1 
          , "build : " ++ toString x2
          , "diff  : " ++ show (dropWhile (uncurry (==)) $ zip x1 x2)
          ]

-- Recipes with which to test the builder functions
---------------------------------------------------

data Mode = 
       Threshold Int
     | Insert 
     | Copy
     | Hex
     deriving( Eq, Ord, Show )

data Action = 
       SBS Mode S.ByteString
     | LBS Mode L.ByteString
     | W8  Word8
     | W8S [Word8]
     | IDec Integer
     | FDec Float
     | DDec Double
     | Flush
     deriving( Eq, Ord, Show )

data Strategy = Safe | Untrimmed
     deriving( Eq, Ord, Show )

data Recipe = Recipe Strategy Int Int L.ByteString [Action]
     deriving( Eq, Ord, Show )

renderRecipe :: Recipe -> [Word8]
renderRecipe (Recipe _ _ _ cont as) =
    D.toList $ foldMap renderAction as `mappend` renderLBS cont
  where
    renderAction (SBS Hex bs)  = foldMap hexWord8 $ S.unpack bs
    renderAction (SBS _ bs)    = D.fromList $ S.unpack bs
    renderAction (LBS Hex lbs) = foldMap hexWord8 $ L.unpack lbs
    renderAction (LBS _ lbs)   = renderLBS lbs
    renderAction (W8 w)        = return w
    renderAction (W8S ws)      = D.fromList ws
    renderAction Flush         = mempty
    renderAction (IDec i)      = D.fromList $ encodeUtf8 $ show i
    renderAction (FDec f)      = D.fromList $ encodeUtf8 $ show f
    renderAction (DDec d)      = D.fromList $ encodeUtf8 $ show d

    renderLBS = D.fromList . L.unpack
    hexWord8  = D.fromList . hexFixedList


buildRecipe :: Recipe -> [Word8]
buildRecipe (Recipe how firstSize otherSize cont as) =
    L.unpack $ toLBS $ foldMap buildAction as
  where
    buildAction (SBS Hex bs)            = Utf8.byteStringHexFixed bs
    buildAction (SBS Copy bs)           = byteStringCopy bs
    buildAction (SBS Insert bs)         = byteStringInsert bs
    buildAction (SBS (Threshold i) bs)  = byteStringThreshold i bs
    buildAction (LBS Hex lbs)           = Utf8.lazyByteStringHexFixed lbs
    buildAction (LBS Copy lbs)          = lazyByteStringCopy lbs
    buildAction (LBS Insert lbs)        = lazyByteStringInsert lbs
    buildAction (LBS (Threshold i) lbs) = lazyByteStringThreshold i lbs
    buildAction (W8 w)                  = word8 w
    buildAction (W8S ws)                = E.encodeListWith E.word8 ws
    buildAction (IDec i)                = Utf8.integerDec i
    buildAction (FDec f)                = Utf8.floatDec f
    buildAction (DDec d)                = Utf8.doubleDec d
    buildAction Flush                   = flush

    toLBS = toLazyByteStringWith (strategy how firstSize otherSize) cont
      where
        strategy Safe      = safeStrategy
        strategy Untrimmed = untrimmedStrategy


-- 'Arbitary' instances
-----------------------

instance Arbitrary L.ByteString where
    arbitrary = L.fromChunks <$> listOf arbitrary
    shrink lbs
      | L.null lbs = []
      | otherwise = pure $ L.take (L.length lbs `div` 2) lbs

instance Arbitrary S.ByteString where
    arbitrary =
        trim S.drop =<< trim S.take =<< S.pack <$> listOf arbitrary
      where
        trim f bs = oneof [pure bs, f <$> choose (0, S.length bs) <*> pure bs]

    shrink bs
      | S.null bs = []
      | otherwise = pure $ S.take (S.length bs `div` 2) bs

instance Arbitrary Mode where
    arbitrary = oneof [Threshold <$> arbitrary, pure Insert, pure Copy, pure Hex]

    shrink (Threshold i) = Threshold <$> shrink i
    shrink _             = []

instance Arbitrary Action where
    arbitrary = oneof
      [ SBS <$> arbitrary <*> arbitrary
      , LBS <$> arbitrary <*> arbitrary
      , W8  <$> arbitrary
      , W8S <$> listOf arbitrary
      , pure Flush
      , IDec <$> arbitrary
      , FDec <$> arbitrary
      , DDec <$> arbitrary
      ]
      where

    shrink (SBS m bs) = 
      (SBS <$> shrink m <*> pure bs) <|>
      (SBS <$> pure m   <*> shrink bs)
    shrink (LBS m lbs) = 
      (LBS <$> shrink m <*> pure lbs) <|>
      (LBS <$> pure m   <*> shrink lbs)
    shrink (W8 w)   = W8 <$> shrink w
    shrink (W8S ws) = W8S <$> shrink ws
    shrink Flush    = []
    shrink (IDec i) = IDec <$> shrink i
    shrink (FDec f) = FDec <$> shrink f
    shrink (DDec d) = DDec <$> shrink d

instance Arbitrary Strategy where
    arbitrary = elements [Safe, Untrimmed]
    shrink _  = []

instance Arbitrary Recipe where
    arbitrary = 
        Recipe <$> arbitrary 
               <*> ((`mod` 33333) <$> arbitrary)  -- bound max chunk-sizes
               <*> ((`mod` 33337) <$> arbitrary)
               <*> arbitrary 
               <*> listOf arbitrary

    -- shrinking the actions first is desirable
    shrink (Recipe a b c d e) = asum
      [ (\x -> Recipe a b c d x) <$> shrink e
      , (\x -> Recipe a b c x e) <$> shrink d
      , (\x -> Recipe a b x d e) <$> shrink c
      , (\x -> Recipe a x c d e) <$> shrink b
      , (\x -> Recipe x b c d e) <$> shrink a
      ] 


-}
