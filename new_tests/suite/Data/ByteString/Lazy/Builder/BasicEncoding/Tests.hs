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

import           Control.Arrow (first)
import           Data.Char (ord)
import           Numeric (showHex)
import           Foreign
import           System.ByteOrder 
import           Test.Framework
import           Test.QuickCheck (Arbitrary)
import           Unsafe.Coerce (unsafeCoerce)

import qualified Data.ByteString.Lazy.Builder.BasicEncoding           as BE
import           Data.ByteString.Lazy.Builder.BasicEncoding.TestUtils


tests :: [Test]
tests = concat [testsBinary, testsASCII, testsChar8, testsUtf8]


------------------------------------------------------------------------------
-- Binary
------------------------------------------------------------------------------

testsBinary :: [Test]
testsBinary =
  [ testBoundedF "word8"     bigEndian_list    BE.word8
  , testBoundedF "int8"      bigEndian_list    BE.int8

  --  big-endian
  , testBoundedF "int16BE"   bigEndian_list    BE.int16BE
  , testBoundedF "int32BE"   bigEndian_list    BE.int32BE
  , testBoundedF "int64BE"   bigEndian_list    BE.int64BE
                              
  , testBoundedF "word16BE"  bigEndian_list    BE.word16BE
  , testBoundedF "word32BE"  bigEndian_list    BE.word32BE
  , testBoundedF "word64BE"  bigEndian_list    BE.word64BE

  , testF "floatLE"     (float_list  littleEndian_list) BE.floatLE
  , testF "doubleLE"    (double_list littleEndian_list) BE.doubleLE

  --  little-endian
  , testBoundedF "int16LE"   littleEndian_list BE.int16LE
  , testBoundedF "int32LE"   littleEndian_list BE.int32LE
  , testBoundedF "int64LE"   littleEndian_list BE.int64LE
                             
  , testBoundedF "word16LE"  littleEndian_list BE.word16LE
  , testBoundedF "word32LE"  littleEndian_list BE.word32LE
  , testBoundedF "word64LE"  littleEndian_list BE.word64LE

  , testF "floatBE"     (float_list  bigEndian_list)   BE.floatBE
  , testF "doubleBE"    (double_list bigEndian_list)   BE.doubleBE

  --  host dependent
  , testBoundedF "int16Host"   hostEndian_list  BE.int16Host
  , testBoundedF "int32Host"   hostEndian_list  BE.int32Host
  , testBoundedF "int64Host"   hostEndian_list  BE.int64Host
  , testBoundedF "intHost"     hostEndian_list  BE.intHost
                              
  , testBoundedF "word16Host"  hostEndian_list  BE.word16Host
  , testBoundedF "word32Host"  hostEndian_list  BE.word32Host
  , testBoundedF "word64Host"  hostEndian_list  BE.word64Host
  , testBoundedF "wordHost"    hostEndian_list  BE.wordHost

  , testF "floatHost"   (float_list  hostEndian_list)   BE.floatHost
  , testF "doubleHost"  (double_list hostEndian_list)   BE.doubleHost

  , testBoundedB "word8Var"     genVar_list  BE.word8Var
  , testBoundedB "word16Var"    genVar_list  BE.word16Var
  , testBoundedB "word32Var"    genVar_list  BE.word32Var
  , testBoundedB "word64Var"    genVar_list  BE.word64Var
  , testBoundedB "wordVar"      genVar_list  BE.wordVar

  , testBoundedB "int8Var"     int8Var_list   BE.int8Var
  , testBoundedB "int16Var"    int16Var_list  BE.int16Var
  , testBoundedB "int32Var"    int32Var_list  BE.int32Var
  , testBoundedB "int64Var"    int64Var_list  BE.int64Var
  , testBoundedB "intVar"      intVar_list    BE.intVar

  , testBoundedB "int8VarSigned"     (int8Var_list  . zigZag)  BE.int8VarSigned
  , testBoundedB "int16VarSigned"    (int16Var_list . zigZag)  BE.int16VarSigned
  , testBoundedB "int32VarSigned"    (int32Var_list . zigZag)  BE.int32VarSigned
  , testBoundedB "int64VarSigned"    (int64Var_list . zigZag)  BE.int64VarSigned
  , testBoundedB "intVarSigned"      (intVar_list   . zigZag)  BE.intVarSigned

  , testGroup "parseable" 
    [ prop_zigZag_parseable  "int8VarSigned"   unZigZagInt8  BE.int8VarSigned
    , prop_zigZag_parseable  "int16VarSigned"  unZigZagInt16 BE.int16VarSigned
    , prop_zigZag_parseable  "int32VarSigned"  unZigZagInt32 BE.int32VarSigned
    , prop_zigZag_parseable  "int64VarSigned"  unZigZagInt64 BE.int64VarSigned
    , prop_zigZag_parseable  "intVarSigned"    unZigZagInt   BE.intVarSigned
    ]

  , testFixedBoundF "wordVarFixed"   wordVarFixed_list    BE.wordVarFixed
  , testFixedBoundF "word64VarFixed" word64VarFixed_list  BE.word64VarFixed

  ]

bigEndian_list :: (Storable a, Bits a, Integral a) => a -> [Word8]
bigEndian_list = reverse . littleEndian_list

littleEndian_list :: (Storable a, Bits a, Integral a) => a -> [Word8]
littleEndian_list x = 
    map (fromIntegral . (x `shiftR`) . (8*)) $ [0..sizeOf x - 1]

hostEndian_list :: (Storable a, Bits a, Integral a) => a -> [Word8]
hostEndian_list = case byteOrder of
    LittleEndian -> littleEndian_list
    BigEndian    -> bigEndian_list
    _            -> error $ 
        "bounded-encoding: unsupported byteorder '" ++ show byteOrder ++ "'"


float_list :: (Word32 -> [Word8]) -> Float -> [Word8]
float_list f  = f . coerceFloatToWord32

double_list :: (Word64 -> [Word8]) -> Double -> [Word8]
double_list f = f . coerceDoubleToWord64

-- Note that the following use of unsafeCoerce is not guaranteed to be 
-- safe on GHC 7.0 and less. The reason is probably the following ticket:
--
--   http://hackage.haskell.org/trac/ghc/ticket/4092
--
-- However, that only applies if the value is loaded in a register. We
-- avoid this by coercing only boxed values and ensuring that they
-- remain boxed using a NOINLINE pragma.
-- 

-- | Super unsafe coerce a 'Float' to a 'Word32'. We have to explicitly mask
-- out the higher bits in case we are working on a 64-bit machine.
{-# NOINLINE coerceFloatToWord32 #-}
coerceFloatToWord32 :: Float -> Word32
coerceFloatToWord32 = (.&. maxBound) . unsafeCoerce

-- | Super unsafe coerce a 'Double' to a 'Word64'. Currently, there are no
-- > 64 bit machines supported by GHC. But we just play it safe.
{-# NOINLINE coerceDoubleToWord64 #-}
coerceDoubleToWord64 :: Double -> Word64
coerceDoubleToWord64 = (.&. maxBound) . unsafeCoerce


-- Variable length encodings
----------------------------

-- | Variable length encoding.
genVar_list :: (Ord a, Num a, Bits a, Integral a) => a -> [Word8]
genVar_list x
  | x <= 0x7f = sevenBits            : []
  | otherwise = (sevenBits .|. 0x80) : genVar_list (x `shiftR` 7)
  where
    sevenBits = fromIntegral x .&. 0x7f

int8Var_list :: Int8 -> [Word8]
int8Var_list  = genVar_list . (fromIntegral :: Int8 -> Word8)

int16Var_list :: Int16 -> [Word8]
int16Var_list = genVar_list . (fromIntegral :: Int16 -> Word16)

int32Var_list :: Int32 -> [Word8]
int32Var_list = genVar_list . (fromIntegral :: Int32 -> Word32)

int64Var_list :: Int64 -> [Word8]
int64Var_list = genVar_list . (fromIntegral :: Int64 -> Word64)

intVar_list :: Int -> [Word8]
intVar_list = genVar_list . (fromIntegral :: Int -> Word)

-- | Parse a variable length encoding
parseVar :: (Num a, Bits a) => [Word8] -> (a, [Word8])
parseVar = 
    go 
  where
    go []    = error "parseVar: unterminated variable length int"
    go (w:ws) 
      | w .&. 0x80 == 0 = (fromIntegral w, ws)
      | otherwise       = first add (go ws)
      where
        add x = (x `shiftL` 7) .|. (fromIntegral w .&. 0x7f)


-- | The so-called \"zig-zag\" encoding from Google's protocol buffers.
-- It maps integers of small magnitude to naturals of small
-- magnitude by encoding negative integers as odd naturals and positive
-- integers as even naturals.
--
-- For example: @0 -> 0,  -1 -> 1, 1 -> 2, -2 -> 3, 2 -> 4, ...@
--
-- PRE: 'a' must be a signed integer type.
zigZag :: (Storable a, Bits a) => a -> a
zigZag x = (x `shiftL` 1) `xor` (x `shiftR` (8 * sizeOf x - 1))


-- | Reversing the zigZag encoding.
--
-- PRE: 'a' must be an unsigned integer type.
--
-- forall x. fromIntegral x == 
--           unZigZag ((fromIntegral :: IntX -> WordX) (zigZag x))
--
unZigZag :: (Storable a, Bits a) => a -> a
unZigZag x = (x `shiftR` 1) `xor` negate (x .&. 1)

unZigZagInt8 :: Int8 -> Int8
unZigZagInt8 = (fromIntegral :: Word8 -> Int8) . unZigZag . fromIntegral

unZigZagInt16 :: Int16 -> Int16
unZigZagInt16 = (fromIntegral :: Word16 -> Int16) . unZigZag . fromIntegral

unZigZagInt32 :: Int32 -> Int32
unZigZagInt32 = (fromIntegral :: Word32 -> Int32) . unZigZag . fromIntegral

unZigZagInt64 :: Int64 -> Int64
unZigZagInt64 = (fromIntegral :: Word64 -> Int64) . unZigZag . fromIntegral

unZigZagInt :: Int -> Int
unZigZagInt = (fromIntegral :: Word -> Int) . unZigZag . fromIntegral

-- | Check that the 'intVarSigned' encodings are parseable.
prop_zigZag_parseable :: (Arbitrary t, Bits b, Show t, Eq t) 
    => String -> (b -> t) -> BE.BoundedEncoding t -> Test
prop_zigZag_parseable name unZig be = 
  compareImpls name (\x -> (x, [])) (first unZig . parseVar . BE.evalB be)
   
-- | Variable length encoding to a fixed number of bytes (pad / truncate).
genVarFixed_list :: (Ord a, Num a, Bits a, Integral a) 
                 => Int 
                 -> a -> [Word8]
genVarFixed_list n x
  | n <= 1    = sevenBits            : []
  | otherwise = (sevenBits .|. 0x80) : genVarFixed_list (n - 1) (x `shiftR` 7)
  where
    sevenBits = fromIntegral x .&. 0x7f

wordVarFixed_list :: Word -> Word -> [Word8]
wordVarFixed_list bound = genVarFixed_list (length $ genVar_list bound)

word64VarFixed_list :: Word64 -> Word64 -> [Word8]
word64VarFixed_list bound = genVarFixed_list (length $ genVar_list bound)

-- Somehow this function doesn't really make sense, as the bound must be
-- greater when interpreted as an unsigned integer.
--
-- intVarFixed_list :: Int -> Int -> [Word8]
-- intVarFixed_list bound = wordVarFixed_list (fromIntegral bound) . fromIntegral


------------------------------------------------------------------------------
-- Latin-1  aka  Char8
------------------------------------------------------------------------------

testsChar8 :: [Test]
testsChar8 = 
  [ testBoundedF "char8"     char8_list        BE.char8  ]

char8_list :: Char -> [Word8]
char8_list = return . fromIntegral . ord


------------------------------------------------------------------------------
-- ASCII
------------------------------------------------------------------------------

testsASCII :: [Test]
testsASCII = 
  [ testBoundedF "charASCII" charASCII_list BE.charASCII

  , testBoundedB "int8Dec"   dec_list BE.int8Dec
  , testBoundedB "int16Dec"  dec_list BE.int16Dec
  , testBoundedB "int32Dec"  dec_list BE.int32Dec
  , testBoundedB "int64Dec"  dec_list BE.int64Dec
  , testBoundedB "intDec"    dec_list BE.intDec

  , testBoundedB "word8Dec"  dec_list BE.word8Dec
  , testBoundedB "word16Dec" dec_list BE.word16Dec
  , testBoundedB "word32Dec" dec_list BE.word32Dec
  , testBoundedB "word64Dec" dec_list BE.word64Dec
  , testBoundedB "wordDec"   dec_list BE.wordDec

  , testBoundedB "word8Hex"  hex_list BE.word8Hex
  , testBoundedB "word16Hex" hex_list BE.word16Hex
  , testBoundedB "word32Hex" hex_list BE.word32Hex
  , testBoundedB "word64Hex" hex_list BE.word64Hex
  , testBoundedB "wordHex"   hex_list BE.wordHex

  , testBoundedF "word8HexFixed"  wordHexFixed_list BE.word8HexFixed
  , testBoundedF "word16HexFixed" wordHexFixed_list BE.word16HexFixed
  , testBoundedF "word32HexFixed" wordHexFixed_list BE.word32HexFixed
  , testBoundedF "word64HexFixed" wordHexFixed_list BE.word64HexFixed

  , testBoundedF "int8HexFixed"  int8HexFixed_list  BE.int8HexFixed
  , testBoundedF "int16HexFixed" int16HexFixed_list BE.int16HexFixed
  , testBoundedF "int32HexFixed" int32HexFixed_list BE.int32HexFixed
  , testBoundedF "int64HexFixed" int64HexFixed_list BE.int64HexFixed

  , testF "floatHexFixed"  floatHexFixed_list  BE.floatHexFixed
  , testF "doubleHexFixed" doubleHexFixed_list BE.doubleHexFixed

  , testFixedBoundF "wordDecFixedBound" 
      (genDecFixedBound_list 'x') (BE.wordDecFixedBound 'x')

  , testFixedBoundF "word64DecFixedBound" 
      (genDecFixedBound_list 'x') (BE.word64DecFixedBound 'x')

  , testFixedBoundF "wordHexFixedBound" 
      (genHexFixedBound_list 'x') (BE.wordHexFixedBound 'x')

  , testFixedBoundF "word64HexFixedBound" 
      (genHexFixedBound_list 'x') (BE.word64HexFixedBound 'x')
  ]

encodeASCII :: String -> [Word8]
encodeASCII = 
    map (encode . ord)
  where
    encode c 
      | c < 0x7f  = fromIntegral c
      | otherwise = error $ "encodeASCII: non-ASCII codepoint " ++ show c
    
encodeForcedASCII :: String -> [Word8]
encodeForcedASCII = map ((.&. 0x7f) . fromIntegral . ord)

charASCII_list :: Char -> [Word8]
charASCII_list = encodeForcedASCII . return

dec_list :: Show a =>  a -> [Word8]
dec_list = encodeASCII . show

hex_list :: (Integral a, Show a) => a -> [Word8]
hex_list = encodeASCII . (\x -> showHex x "")

wordHexFixed_list :: (Storable a, Integral a, Show a) => a -> [Word8]
wordHexFixed_list x =
   encodeASCII $ pad (2 * sizeOf x) $ showHex x ""
 where
   pad n cs = replicate (n - length cs) '0' ++ cs

int8HexFixed_list :: Int8 -> [Word8]
int8HexFixed_list  = wordHexFixed_list . (fromIntegral :: Int8  -> Word8 )

int16HexFixed_list :: Int16 -> [Word8]
int16HexFixed_list = wordHexFixed_list . (fromIntegral :: Int16 -> Word16)

int32HexFixed_list :: Int32 -> [Word8]
int32HexFixed_list = wordHexFixed_list . (fromIntegral :: Int32 -> Word32)

int64HexFixed_list :: Int64 -> [Word8]
int64HexFixed_list = wordHexFixed_list . (fromIntegral :: Int64 -> Word64)

floatHexFixed_list :: Float -> [Word8]
floatHexFixed_list  = float_list wordHexFixed_list

doubleHexFixed_list :: Double -> [Word8]
doubleHexFixed_list = double_list wordHexFixed_list

-- | PRE: positive bound and value.
genDecFixedBound_list :: (Show a, Integral a)
                      => Char    -- ^ Padding character.
                      -> a       -- ^ Max value to be encoded.
                      -> a       -- ^ Value to encode.
                      -> [Word8]
genDecFixedBound_list padChar bound = 
    encodeASCII . pad . show
  where
    n      = length $ show bound
    pad cs = replicate (n - length cs) padChar ++ cs

-- | PRE: positive bound and value.
genHexFixedBound_list :: (Show a, Integral a)
                      => Char    -- ^ Padding character.
                      -> a       -- ^ Max value to be encoded.
                      -> a       -- ^ Value to encode.
                      -> [Word8]
genHexFixedBound_list padChar bound = 
    encodeASCII . pad . (`showHex` "")
  where
    n      = length $ (`showHex` "") bound
    pad cs = replicate (n - length cs) padChar ++ cs


------------------------------------------------------------------------------
-- UTF-8
------------------------------------------------------------------------------

testsUtf8 :: [Test]
testsUtf8 = 
  [ testBoundedB "charUtf8"  charUtf8_list  BE.charUtf8 ]

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
--
-- Copied from 'utf8-string-0.3.6' to make tests self-contained. 
-- Copyright (c) 2007, Galois Inc. All rights reserved.
--
charUtf8_list :: Char -> [Word8]
charUtf8_list =
    map fromIntegral . encode . ord
  where
    encode oc
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

