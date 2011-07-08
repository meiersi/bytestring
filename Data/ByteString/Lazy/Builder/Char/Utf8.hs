-- | Copyright : (c) 2010      Jasper Van der Jeugt 
--               (c) 2010-2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Functions for creating 'Builder's by UTF-8 encoding characters.
--
module Data.ByteString.Lazy.Builder.Char.Utf8
    ( 
      -- * UTF-8 encoding characters
      Utf8(..)

      -- * Hexadecimal encoding using UTF-8 encoded characters
    , utf8HexLower
    , utf8HexUpper
    , utf8HexLowerNoLead
    , utf8HexUpperNoLead
    ) where

import Data.Foldable (foldMap)

import Data.ByteString.Lazy.Builder.Internal
import Data.ByteString.Lazy.Builder.Write
import Data.ByteString.Lazy.Builder.Char.Ascii

import qualified System.IO.Write.Char.Utf8  as W


-- | UTF-8 encodable values.
class Utf8 a where
    -- | Encode using UTF-8.
    utf8 :: a -> Builder

instance Utf8 Char where
    {-# INLINE utf8 #-}
    utf8 = fromWrite W.utf8

instance Utf8 a => Utf8 [a] where
    {-# INLINE utf8 #-}
    utf8 = foldMap utf8

-- | Fixed-width hexadecimal encoding with lower-case characters encoded using
-- UTF-8.
--
-- > toLazyByteString (utf8HexLower [26 :: Word16, 15]) = pack "001a000f"
--
-- Note that we exploit that the UTF-8 encoding coincides with the ASCII
-- encoding on codepoints below 128. This is the origin of the
-- 'AsciiHex' type-class constraint.
--
utf8HexLower :: AsciiHex a => a -> Builder
utf8HexLower = asciiHexLower

-- | Fixed-width hexadecimal encoding with upper-case characters encoded using
-- UTF-8.
--
-- > toLazyByteString (utf8HexLower [26 :: Word16, 15]) = pack "001A000F"
--
utf8HexUpper :: AsciiHex a => a -> Builder
utf8HexUpper = asciiHexUpper


-- | Hexadecimal encoding with no leading zeros and lower-case characters 
-- encoded using UTF-8.
--
-- > toLazyByteString (utf8HexLowerNoLead (26 :: Word16)) = "1a"
--
utf8HexLowerNoLead :: AsciiHexNoLead a => a -> Builder
utf8HexLowerNoLead = asciiHexLowerNoLead

-- | Hexadecimal encoding with no leading zeros and upper-case characters
-- encoded using UTF-8.
--
-- > toLazyByteString (utf8HexUpperNoLead (26 :: Word16)) = "1A"
--
utf8HexUpperNoLead :: AsciiHexNoLead a => a -> Builder
utf8HexUpperNoLead = asciiHexUpperNoLead

