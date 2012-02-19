{-# LANGUAGE ScopedTypeVariables, CPP, ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
-- | Copyright : (c) 2010 - 2011 Simon Meier
--               (c) 2011 MailRank, Inc.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Portability : GHC
--
-- Constructing 'Builder's using ASCII-based encodings.
--
module Data.ByteString.Lazy.Builder.ASCII
    (
      -- * Decimal numbers
      -- | Decimal encoding of numbers using ASCII encoded characters.
      int8Dec
    , int16Dec
    , int32Dec
    , int64Dec
    , intDec
    , integerDec

    , word8Dec
    , word16Dec
    , word32Dec
    , word64Dec
    , wordDec

    , floatDec
    , doubleDec

      -- * Hexadecimal numbers

      -- | Encoding positive integers as hexadecimal numbers using lower-case
      -- ASCII characters. The shortest
      -- possible representation is used. For example,
      --
      -- >>> toLazyByteString (word16Hex 0x0a10)
      -- "a10"
      --
      -- Note that there is no support for using upper-case characters. Please
      -- contact the maintainer, if your application cannot work without
      -- hexadecimal encodings that use upper-case characters.
      --
    , word8Hex
    , word16Hex
    , word32Hex
    , word64Hex
    , wordHex

      -- * Fixed-width hexadecimal numbers

      -- | Encoding integers as hexadecimal numbers using lower-case
      -- ASCII characters. The width of the output is fixed and corresponds to
      -- the bit-width of the input-type divided by 4. Zero-padding is used,
      -- if required. For example:
      --
      -- >>> toLazyByteString (word16Hex 0xa10)
      -- "0a10"
      --
    , int8HexFixed
    , int16HexFixed
    , int32HexFixed
    , int64HexFixed
    , word8HexFixed
    , word16HexFixed
    , word32HexFixed
    , word64HexFixed

    , floatHexFixed
    , doubleHexFixed

    , byteStringHexFixed
    , lazyByteStringHexFixed

    ) where

import           Data.ByteString                                      as S
import           Data.ByteString.Lazy.Internal                        as L
import           Data.ByteString.Lazy.Builder.Internal (Builder)      
import qualified Data.ByteString.Lazy.Builder.BasicEncoding           as E
import qualified Data.ByteString.Lazy.Builder.BasicEncoding.Internal  as E
import           Data.ByteString.Lazy.Builder.BasicEncoding.Internal.UncheckedShifts
                   ( caseWordSize_32_64 )
import           Data.Monoid (mappend)

import           Foreign
import           Foreign.C.Types (CInt(..), CLLong(..))


#ifdef  __GLASGOW_HASKELL__
import           GHC.Num     (quotRemInteger)
import           GHC.Types   (Int(..))

# if __GLASGOW_HASKELL__ < 611
import GHC.Integer.Internals
# else
import GHC.Integer.GMP.Internals
# endif
#endif


------------------------------------------------------------------------------
-- Decimal Encoding
------------------------------------------------------------------------------

-- Signed integers
------------------

-- | Decimal encoding of an 'Int8' using the ASCII digits.
{-# INLINE int8Dec #-}
int8Dec :: Int8 -> Builder
int8Dec = E.encodeWithB E.int8Dec

-- | Decimal encoding of an 'Int16' using the ASCII digits.
{-# INLINE int16Dec #-}
int16Dec :: Int16 -> Builder
int16Dec = E.encodeWithB E.int16Dec

-- | Decimal encoding of an 'Int32' using the ASCII digits.
{-# INLINE int32Dec #-}
int32Dec :: Int32 -> Builder
int32Dec = E.encodeWithB E.int32Dec

-- | Decimal encoding of an 'Int64' using the ASCII digits.
{-# INLINE int64Dec #-}
int64Dec :: Int64 -> Builder
int64Dec = E.encodeWithB E.int64Dec

-- | Decimal encoding of an 'Int' using the ASCII digits.
{-# INLINE intDec #-}
intDec :: Int -> Builder
intDec = E.encodeWithB E.intDec


-- Unsigned integers
--------------------

-- | Decimal encoding of a 'Word8' using the ASCII digits.
{-# INLINE word8Dec #-}
word8Dec :: Word8 -> Builder
word8Dec = E.encodeWithB E.word8Dec

-- | Decimal encoding of a 'Word16' using the ASCII digits.
{-# INLINE word16Dec #-}
word16Dec :: Word16 -> Builder
word16Dec = E.encodeWithB E.word16Dec

-- | Decimal encoding of a 'Word32' using the ASCII digits.
{-# INLINE word32Dec #-}
word32Dec :: Word32 -> Builder
word32Dec = E.encodeWithB E.word32Dec

-- | Decimal encoding of a 'Word64' using the ASCII digits.
{-# INLINE word64Dec #-}
word64Dec :: Word64 -> Builder
word64Dec = E.encodeWithB E.word64Dec

-- | Decimal encoding of a 'Word' using the ASCII digits.
{-# INLINE wordDec #-}
wordDec :: Word -> Builder
wordDec = E.encodeWithB E.wordDec


-- Floating point numbers
-------------------------

-- TODO: Use Bryan O'Sullivan's double-conversion package to speed it up.

-- | Decimal encoding of an IEEE 'Float' using the ASCII digits.
{-# INLINE floatDec #-}
floatDec :: Float -> Builder
floatDec = string8 . show

-- | Decimal encoding of an IEEE 'Double' using the ASCII digits.
{-# INLINE doubleDec #-}
doubleDec :: Double -> Builder
doubleDec = string8 . show

-- | Encode a 'String' using 'E.char8'.
{-# INLINE string8 #-}
string8 :: String -> Builder
string8 = E.encodeListWithF E.char8

------------------------------------------------------------------------------
-- Hexadecimal Encoding
------------------------------------------------------------------------------

-- without lead
---------------

-- | Shortest hexadecimal encoding of a 'Word8' using lower-case characters.
{-# INLINE word8Hex #-}
word8Hex :: Word8 -> Builder
word8Hex = E.encodeWithB E.word8Hex

-- | Shortest hexadecimal encoding of a 'Word16' using lower-case characters.
{-# INLINE word16Hex #-}
word16Hex :: Word16 -> Builder
word16Hex = E.encodeWithB E.word16Hex

-- | Shortest hexadecimal encoding of a 'Word32' using lower-case characters.
{-# INLINE word32Hex #-}
word32Hex :: Word32 -> Builder
word32Hex = E.encodeWithB E.word32Hex

-- | Shortest hexadecimal encoding of a 'Word64' using lower-case characters.
{-# INLINE word64Hex #-}
word64Hex :: Word64 -> Builder
word64Hex = E.encodeWithB E.word64Hex

-- | Shortest hexadecimal encoding of a 'Word' using lower-case characters.
{-# INLINE wordHex #-}
wordHex :: Word -> Builder
wordHex = E.encodeWithB E.wordHex


-- fixed width; leading zeroes
------------------------------

-- | Encode an 'Int8' using 2 hexadecimal characters.
{-# INLINE int8HexFixed #-}
int8HexFixed :: Int8 -> Builder
int8HexFixed = E.encodeWithF E.int8HexFixed

-- | Encode an 'Int16' using 4 hexadecimal characters.
{-# INLINE int16HexFixed #-}
int16HexFixed :: Int16 -> Builder
int16HexFixed = E.encodeWithF E.int16HexFixed

-- | Encode an 'Int32' using 8 hexadecimal characters.
{-# INLINE int32HexFixed #-}
int32HexFixed :: Int32 -> Builder
int32HexFixed = E.encodeWithF E.int32HexFixed

-- | Encode an 'Int64' using 16 hexadecimal characters.
{-# INLINE int64HexFixed #-}
int64HexFixed :: Int64 -> Builder
int64HexFixed = E.encodeWithF E.int64HexFixed

-- | Encode a 'Word8' using 2 hexadecimal characters.
{-# INLINE word8HexFixed #-}
word8HexFixed :: Word8 -> Builder
word8HexFixed = E.encodeWithF E.word8HexFixed

-- | Encode a 'Word16' using 4 hexadecimal characters.
{-# INLINE word16HexFixed #-}
word16HexFixed :: Word16 -> Builder
word16HexFixed = E.encodeWithF E.word16HexFixed

-- | Encode a 'Word32' using 8 hexadecimal characters.
{-# INLINE word32HexFixed #-}
word32HexFixed :: Word32 -> Builder
word32HexFixed = E.encodeWithF E.word32HexFixed

-- | Encode a 'Word64' using 16 hexadecimal characters.
{-# INLINE word64HexFixed #-}
word64HexFixed :: Word64 -> Builder
word64HexFixed = E.encodeWithF E.word64HexFixed

-- | Encode an IEEE 'Float' using 8 hexadecimal characters in big-endian
-- order.
{-# INLINE floatHexFixed #-}
floatHexFixed :: Float -> Builder
floatHexFixed = E.encodeWithF E.floatHexFixed

-- | Encode an IEEE 'Double' using 16 hexadecimal characters in big-endian
-- order.
{-# INLINE doubleHexFixed #-}
doubleHexFixed :: Double -> Builder
doubleHexFixed = E.encodeWithF E.doubleHexFixed

-- | Encode each byte of a 'S.ByteString' using its fixed-width hex encoding.
{-# NOINLINE byteStringHexFixed #-} -- share code
byteStringHexFixed :: S.ByteString -> Builder
byteStringHexFixed = E.encodeByteStringWithF E.word8HexFixed

-- | Encode each byte of a lazy 'L.ByteString' using its fixed-width hex encoding.
{-# NOINLINE lazyByteStringHexFixed #-} -- share code
lazyByteStringHexFixed :: L.ByteString -> Builder
lazyByteStringHexFixed = E.encodeLazyByteStringWithF E.word8HexFixed


------------------------------------------------------------------------------
-- Fast decimal 'Integer' encoding.
------------------------------------------------------------------------------

#ifdef  __GLASGOW_HASKELL__
-- An optimized version of the integer serialization code
-- in blaze-textual (c) 2011 MailRank, Inc. Bryan O'Sullivan
-- <bos@mailrank.com>. It is 2.5x faster on Int-sized integers and 4.5x faster
-- on larger integers.

#ifdef INTEGER_GMP
# define PAIR(a,b) (# a,b #)
#else
# define PAIR(a,b) (a,b)
#endif

-- | Maximal power of 10 fitting into an 'Int' without using the MSB.
--     10 ^ 9  for 32 bit ints  (31 * log 2 / log 10 =  9.33)
--     10 ^ 18 for 64 bit ints  (63 * log 2 / log 10 = 18.96)
--
-- FIXME: Think about also using the MSB. For 64 bit 'Int's this makes a
-- difference.
maxPow10 :: Integer
maxPow10 = toInteger $ (10 :: Int) ^ caseWordSize_32_64 (9 :: Int) 18

-- | Decimal encoding of an 'Integer' using the ASCII digits.
integerDec :: Integer -> Builder
integerDec (S# i#) = intDec (I# i#)
integerDec i
    | i < 0     = E.encodeWithF E.char8 '-' `mappend` go (-i)
    | otherwise =                                     go ( i)
  where
    errImpossible fun = 
        error $ "integerDec: " ++ fun ++ ": the impossible happened."

    go :: Integer -> Builder
    go n | n < maxPow10 = intDec (fromInteger n)
         | otherwise    = 
             case putH (splitf (maxPow10 * maxPow10) n) of
               (x:xs) -> intDec x `mappend` E.encodeListWithF intDecPadded xs
               []     -> errImpossible "go"

    splitf :: Integer -> Integer -> [Integer]
    splitf pow10 n0
      | pow10 > n0  = [n0]
      | otherwise   = splith (splitf (pow10 * pow10) n0)
      where
        splith []     = errImpossible "splith"
        splith (n:ns) = 
            case n `quotRemInteger` pow10 of
                PAIR(q,r) | q > 0     -> q : r : splitb ns
                          | otherwise ->     r : splitb ns
     
        splitb []     = []
        splitb (n:ns) = case n `quotRemInteger` pow10 of
                            PAIR(q,r) -> q : r : splitb ns

    putH :: [Integer] -> [Int]
    putH []     = errImpossible "putH"
    putH (n:ns) = case n `quotRemInteger` maxPow10 of
                    PAIR(x,y)
                        | q > 0     -> q : r : putB ns
                        | otherwise ->     r : putB ns
                        where q = fromInteger x
                              r = fromInteger y

    putB :: [Integer] -> [Int]
    putB []     = []
    putB (n:ns) = case n `quotRemInteger` maxPow10 of
                    PAIR(q,r) -> fromInteger q : fromInteger r : putB ns


foreign import ccall unsafe "static _hs_bytestring_int_dec_padded9" 
    c_int_dec_padded9 :: CInt -> Ptr Word8 -> IO ()

foreign import ccall unsafe "static _hs_bytestring_long_long_int_dec_padded18" 
    c_long_long_int_dec_padded18 :: CLLong -> Ptr Word8 -> IO ()

{-# INLINE intDecPadded #-}
intDecPadded :: E.FixedEncoding Int
intDecPadded = caseWordSize_32_64
    (E.fixedEncoding  9 $ c_int_dec_padded9            . fromIntegral)
    (E.fixedEncoding 18 $ c_long_long_int_dec_padded18 . fromIntegral)

#else
-- compilers other than GHC

-- | Decimal encoding of an 'Integer' using the ASCII digits.
integerDec :: Integer -> Builder
integerDec = string8 . show
#endif
