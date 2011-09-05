-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Hexadecimal encoding of nibbles (4-bit) and octets (8-bit) as ASCII
-- characters.
--
-- The current implementation is based on a table based encoding inspired by
-- the code in the 'base64-bytestring' library by Bryan O'Sullivan. In our
-- benchmarks on a 32-bit machine it turned out to be the fastest
-- implementation option.
--
module Data.ByteString.Lazy.Builder.BasicEncoding.Internal.Base16 (
    EncodingTable
  , upperTable
  , lowerTable
  , encode4_as_8 
  , encode8_as_16h
  , encode8_as_8_8
  ) where

import Control.Applicative 

import qualified Data.ByteString          as S
import qualified Data.ByteString.Internal as S

import Foreign


-- Creating the encoding tables
-------------------------------

-- TODO: Use table from C implementation.

-- | An encoding table for Base16 encoding.
newtype EncodingTable = EncodingTable (ForeignPtr Word8)

tableFromList :: [Word8] -> EncodingTable
tableFromList xs = case S.pack xs of S.PS fp _ _ -> EncodingTable fp

unsafeIndex :: EncodingTable -> Int -> IO Word8
unsafeIndex (EncodingTable table) = peekElemOff (unsafeForeignPtrToPtr table)

{-# NOINLINE upperAlphabet #-}
upperAlphabet :: EncodingTable
upperAlphabet = 
    tableFromList $ map (fromIntegral . fromEnum) $ ['0'..'9'] ++ ['A'..'F']

{-# NOINLINE lowerAlphabet #-}
lowerAlphabet :: EncodingTable
lowerAlphabet = 
    tableFromList $ map (fromIntegral . fromEnum) $ ['0'..'9'] ++ ['a'..'f']

base16EncodingTable :: EncodingTable -> IO EncodingTable
base16EncodingTable alphabet = do
    xs <- sequence $ concat $ [ [ix j, ix k] | j <- [0..15], k <- [0..15] ]
    return $ tableFromList xs
  where
    ix = unsafeIndex alphabet

-- | The encoding table for hexadecimal values with upper-case characters;
-- e.g., DEADBEEF.
{-# NOINLINE upperTable #-}
upperTable :: EncodingTable
upperTable = unsafePerformIO $ base16EncodingTable upperAlphabet

-- | The encoding table for hexadecimal values with lower-case characters;
-- e.g., deadbeef.
{-# NOINLINE lowerTable #-}
lowerTable :: EncodingTable
lowerTable = unsafePerformIO $ base16EncodingTable lowerAlphabet


-- Encoding nibbles and octets
------------------------------

-- | Encode a nibble as an octet.
--
-- > encode4_as_8 lowerTable 10 = fromIntegral (char 'a')
--
{-# INLINE encode4_as_8 #-}
encode4_as_8 :: EncodingTable -> Word8 -> IO Word8
encode4_as_8 table x = unsafeIndex table (2 * fromIntegral x + 1)

-- | Encode an octet as 16bit word comprising both encoded nibbles ordered
-- according to the host endianness. Writing these 16bit to memory will write
-- the nibbles in the correct order (i.e. big-endian).
{-# INLINE encode8_as_16h #-}
encode8_as_16h :: EncodingTable -> Word8 -> IO Word16
encode8_as_16h (EncodingTable table) = 
    peekElemOff (castPtr $ unsafeForeignPtrToPtr table) . fromIntegral

-- | Encode an octet as a big-endian ordered tuple of octets; i.e.,
--
-- >   encode8_as_8_8 lowerTable 10
-- > = (fromIntegral (chr '0'), fromIntegral (chr 'a'))
--
{-# INLINE encode8_as_8_8 #-}
encode8_as_8_8 :: EncodingTable -> Word8 -> IO (Word8, Word8)
encode8_as_8_8 table x = 
    (,) <$> unsafeIndex table i <*> unsafeIndex table (i + 1)
  where
    i = 2 * fromIntegral x

