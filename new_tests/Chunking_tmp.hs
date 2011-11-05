{-# LANGUAGE PackageImports,ScopedTypeVariables #-}

import           Control.Arrow (first)
import           Data.Char (ord)
import           Numeric (showHex)
import           Foreign
import           System.ByteOrder 
import           Test.Framework
import           Test.QuickCheck (Arbitrary)
import           Unsafe.Coerce (unsafeCoerce)

import qualified "new-bytestring" Data.ByteString.Lazy                                 as L
import qualified "new-bytestring" Data.ByteString.Lazy.Builder.BasicEncoding           as BE
import           "new-bytestring" Data.ByteString.Lazy.Builder
import           "new-bytestring" Data.ByteString.Lazy.Builder.Extras




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

{-

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
genVarFixedBound_list :: (Ord a, Num a, Bits a, Integral a) 
                 => Int 
                 -> a -> [Word8]
genVarFixedBound_list n x
  | n <= 1    = sevenBits            : []
  | otherwise = (sevenBits .|. 0x80) : genVarFixedBound_list (n - 1) (x `shiftR` 7)
  where
    sevenBits = fromIntegral x .&. 0x7f

wordVarFixedBound_list :: Word -> Word -> [Word8]
wordVarFixedBound_list bound = genVarFixedBound_list (length $ genVar_list bound)

word64VarFixedBound_list :: Word64 -> Word64 -> [Word8]
word64VarFixedBound_list bound = genVarFixedBound_list (length $ genVar_list bound)

-- Somehow this function doesn't really make sense, as the bound must be
-- greater when interpreted as an unsigned integer.
--
-- intVarFixedBound_list :: Int -> Int -> [Word8]
-- intVarFixedBound_list bound = wordVarFixedBound_list (fromIntegral bound) . fromIntegral
-}



------------------------------------------------------------------------------
-- Creating Builders from basic encodings
------------------------------------------------------------------------------

{-
testsChunked :: [Test]
testsChunked =
  [ test_encodeChunked ]

test_encodeChunked :: Test
test_encodeChunked = 
    compareImpls "encodeChunked . stringUtf8" 
        (runBuilder id) (parseChunks . runBuilder encodeVar)
  where
    -}
encodeVar = BE.encodeChunked 16 BE.word64VarFixedBound BE.emptyB

runBuilder f cs = 
    L.unpack $ toLazyByteStringWith strategy L.empty $ f $ stringUtf8 cs
  where
    bufSize      = length cs `div` 5
    firstBufSize = bufSize `div` 3
    strategy     = safeStrategy firstBufSize bufSize

parseChunks ws
  | chunkLen == 0 && null ws' = []
  | chunkLen == 0             = error $ "trailing bytes: " ++ show ws
  | chunkLen <= length ws'    = chunk ++ parseChunks rest
  | otherwise                 = error $ "too few bytes: " ++ show ws
  where
    (chunkLen, ws') = parseVar ws
    (chunk, rest)   = splitAt chunkLen ws'

        
test1 = runBuilder id "hello simon"
test2 = runBuilder encodeVar "hello simon"
         



