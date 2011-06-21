-- | Copyright : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Functions for creating 'Builder's by ASCII encoding characters
-- (cf. <http://tools.ietf.org/html/rfc20>).
--
-- They are intended for constructing output in formats that explicitly
-- restrict the character encoding to 7-bit ASCII encoded characters. In all
-- other cases, a lossless Unicode encoding (e.g.,
-- "Data.ByteString.Builder.Char.Utf8") is a better choice.
--
module Data.ByteString.Builder.Char.Ascii
    ( 
      -- * ASCII encoding of characters
      Ascii(..)

      -- * Hexadecimal encoding using ASCII encoded characters
    , AsciiHex(..)
    , AsciiHexNoLead(..)
    ) where

import Numeric       (showHex)
import Data.Char     (toUpper)
import Data.Foldable (foldMap)

import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy.Internal as L

import Data.ByteString.Builder.Internal
import Data.ByteString.Builder.Write

import qualified System.IO.Write.Char.Ascii as W


import Foreign

-- | ASCII encoding of character(s) with a Unicode codepoint below 128. The
-- class methods differ in how non-encable characters with a codepoint greater
-- or equal to 128 are handled.
class Ascii a where
    -- | Throws an error if there is a non-encodable character.
    ascii        :: a -> Builder

    -- | Writes the given replacement character for non-encodable characters.
    -- The replacement character must have a codepoint below 128. Otherwise
    -- an error is thrown.
    asciiReplace :: Char -> a -> Builder

    -- | Non-encodable characters are ignored.
    --
    -- Note that dropping characters during encoding is often dangerous, as it
    -- may lead to unintended interpretations of the written output. Either use
    -- 'asciiReplace' to insert a character whose insertion is safe in your
    -- context or, if possible, use a lossless encoding like UTF-8.
    asciiDrop    :: a -> Builder


-- | Values that support a fixed-width hexadecimal encoding using ASCII encoded
-- characters.
class AsciiHex a where
    -- | Fixed-width hexadecimal encoding with lower-case characters.
    --
    -- > toLazyByteString (asciiHexLower [26 :: Word16, 15]) = pack "001a000f"
    --
    asciiHexLower :: a -> Builder

    -- | Fixed-width hexadecimal encoding with upper-case characters.
    --
    -- > toLazyByteString (asciiHexLower [26 :: Word16, 15]) = pack "001A000F"
    --
    asciiHexUpper :: a -> Builder


-- | Values that support a hexadecimal encoding with no leading zeros using
-- ASCII encoded characters.
--
-- Note that there is no instance for lists or other sequence-like types, as
-- concatenating the encodings of a sequence's elements results in a
-- non-injective (i.e., ambiguous) function.
class AsciiHexNoLead a where
    -- | Hexadecimal encoding with lower-case characters and no leading zeros.
    --
    -- > toLazyByteString (asciiHexLowerNoLead (26 :: Word16)) = "1a"
    --
    asciiHexLowerNoLead :: a -> Builder

    -- | Hexadecimal encoding with upper-case characters and no leading zeros.
    --
    -- > toLazyByteString (asciiHexLowerNoLead (26 :: Word16)) = "1A"
    --
    asciiHexUpperNoLead :: a -> Builder


-- Ascii instances

instance Ascii Char where
    {-# INLINE ascii #-}
    ascii          = fromWrite W.ascii
    {-# INLINE asciiReplace #-}
    asciiReplace c = fromWrite (W.asciiReplace c)
    {-# INLINE asciiDrop #-}
    asciiDrop      = fromWrite W.asciiDrop

instance Ascii a => Ascii [a] where
    {-# INLINE ascii #-}
    ascii          = foldMap ascii
    {-# INLINE asciiReplace #-}
    asciiReplace c = foldMap (asciiReplace c)
    {-# INLINE asciiDrop #-}
    asciiDrop      = foldMap asciiDrop

-- AsciiHex instances

instance AsciiHex a => AsciiHex [a] where
    {-# INLINE asciiHexLower #-}
    asciiHexLower = foldMap asciiHexLower
    {-# INLINE asciiHexUpper #-}
    asciiHexUpper = foldMap asciiHexUpper

instance AsciiHex Word where
    {-# INLINE asciiHexLower #-}
    asciiHexLower = fromWrite W.asciiHexLower
    {-# INLINE asciiHexUpper #-}
    asciiHexUpper = fromWrite W.asciiHexUpper

instance AsciiHex Word8 where
    {-# INLINE asciiHexLower #-}
    asciiHexLower = fromWrite W.asciiHexLower
    {-# INLINE asciiHexUpper #-}
    asciiHexUpper = fromWrite W.asciiHexUpper

instance AsciiHex Word16 where
    {-# INLINE asciiHexLower #-}
    asciiHexLower = fromWrite W.asciiHexLower
    {-# INLINE asciiHexUpper #-}
    asciiHexUpper = fromWrite W.asciiHexUpper

instance AsciiHex Word32 where
    {-# INLINE asciiHexLower #-}
    asciiHexLower = fromWrite W.asciiHexLower
    {-# INLINE asciiHexUpper #-}
    asciiHexUpper = fromWrite W.asciiHexUpper

instance AsciiHex Word64 where
    {-# INLINE asciiHexLower #-}
    asciiHexLower = fromWrite W.asciiHexLower
    {-# INLINE asciiHexUpper #-}
    asciiHexUpper = fromWrite W.asciiHexUpper

instance AsciiHex Int where
    {-# INLINE asciiHexLower #-}
    asciiHexLower = fromWrite W.asciiHexLower
    {-# INLINE asciiHexUpper #-}
    asciiHexUpper = fromWrite W.asciiHexUpper

instance AsciiHex Int8 where
    {-# INLINE asciiHexLower #-}
    asciiHexLower = fromWrite W.asciiHexLower
    {-# INLINE asciiHexUpper #-}
    asciiHexUpper = fromWrite W.asciiHexUpper

instance AsciiHex Int16 where
    {-# INLINE asciiHexLower #-}
    asciiHexLower = fromWrite W.asciiHexLower
    {-# INLINE asciiHexUpper #-}
    asciiHexUpper = fromWrite W.asciiHexUpper

instance AsciiHex Int32 where
    {-# INLINE asciiHexLower #-}
    asciiHexLower = fromWrite W.asciiHexLower
    {-# INLINE asciiHexUpper #-}
    asciiHexUpper = fromWrite W.asciiHexUpper

instance AsciiHex Int64 where
    {-# INLINE asciiHexLower #-}
    asciiHexLower = fromWrite W.asciiHexLower
    {-# INLINE asciiHexUpper #-}
    asciiHexUpper = fromWrite W.asciiHexUpper

instance AsciiHex S.ByteString where
    asciiHexLower = mapWriteByteString W.asciiHexLower
    asciiHexUpper = mapWriteByteString W.asciiHexUpper

instance AsciiHex L.ByteString where
    asciiHexLower = mapWriteLazyByteString W.asciiHexLower
    asciiHexUpper = mapWriteLazyByteString W.asciiHexUpper

-- AsciiHexNoLead instances

instance AsciiHexNoLead Integer where
    asciiHexLowerNoLead i = ascii               $ showHex i ""
    asciiHexUpperNoLead i = ascii $ map toUpper $ showHex i ""

instance AsciiHexNoLead Word where
    {-# INLINE asciiHexLowerNoLead #-}
    asciiHexLowerNoLead = fromWrite W.asciiHexLowerNoLead
    {-# INLINE asciiHexUpperNoLead #-}
    asciiHexUpperNoLead = fromWrite W.asciiHexUpperNoLead

instance AsciiHexNoLead Word8 where
    {-# INLINE asciiHexLowerNoLead #-}
    asciiHexLowerNoLead = fromWrite W.asciiHexLowerNoLead
    {-# INLINE asciiHexUpperNoLead #-}
    asciiHexUpperNoLead = fromWrite W.asciiHexUpperNoLead

instance AsciiHexNoLead Word16 where
    {-# INLINE asciiHexLowerNoLead #-}
    asciiHexLowerNoLead = fromWrite W.asciiHexLowerNoLead
    {-# INLINE asciiHexUpperNoLead #-}
    asciiHexUpperNoLead = fromWrite W.asciiHexUpperNoLead

instance AsciiHexNoLead Word32 where
    {-# INLINE asciiHexLowerNoLead #-}
    asciiHexLowerNoLead = fromWrite W.asciiHexLowerNoLead
    {-# INLINE asciiHexUpperNoLead #-}
    asciiHexUpperNoLead = fromWrite W.asciiHexUpperNoLead

instance AsciiHexNoLead Word64 where
    {-# INLINE asciiHexLowerNoLead #-}
    asciiHexLowerNoLead = fromWrite W.asciiHexLowerNoLead
    {-# INLINE asciiHexUpperNoLead #-}
    asciiHexUpperNoLead = fromWrite W.asciiHexUpperNoLead

instance AsciiHexNoLead Int where
    {-# INLINE asciiHexLowerNoLead #-}
    asciiHexLowerNoLead = fromWrite W.asciiHexLowerNoLead
    {-# INLINE asciiHexUpperNoLead #-}
    asciiHexUpperNoLead = fromWrite W.asciiHexUpperNoLead

instance AsciiHexNoLead Int8 where
    {-# INLINE asciiHexLowerNoLead #-}
    asciiHexLowerNoLead = fromWrite W.asciiHexLowerNoLead
    {-# INLINE asciiHexUpperNoLead #-}
    asciiHexUpperNoLead = fromWrite W.asciiHexUpperNoLead

instance AsciiHexNoLead Int16 where
    {-# INLINE asciiHexLowerNoLead #-}
    asciiHexLowerNoLead = fromWrite W.asciiHexLowerNoLead
    {-# INLINE asciiHexUpperNoLead #-}
    asciiHexUpperNoLead = fromWrite W.asciiHexUpperNoLead

instance AsciiHexNoLead Int32 where
    {-# INLINE asciiHexLowerNoLead #-}
    asciiHexLowerNoLead = fromWrite W.asciiHexLowerNoLead
    {-# INLINE asciiHexUpperNoLead #-}
    asciiHexUpperNoLead = fromWrite W.asciiHexUpperNoLead

instance AsciiHexNoLead Int64 where
    {-# INLINE asciiHexLowerNoLead #-}
    asciiHexLowerNoLead = fromWrite W.asciiHexLowerNoLead
    {-# INLINE asciiHexUpperNoLead #-}
    asciiHexUpperNoLead = fromWrite W.asciiHexUpperNoLead

