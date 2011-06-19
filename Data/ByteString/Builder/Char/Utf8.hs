-- | Copyright : (c) 2010      Jasper Van der Jeugt 
--               (c) 2010-2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
module Data.ByteString.Builder.Char.Utf8
    ( 
      -- * UTF-8 encoded characters
      Utf8(..)

      -- * Hexadecimal encoding using UTF-8 encoded characters
    , utf8HexLower
    , utf8HexUpper
    , utf8HexLowerNoLead
    , utf8HexUpperNoLead
    ) where

import Data.Foldable (foldMap)

import Data.ByteString.Builder.Internal
import Data.ByteString.Builder.Write
import Data.ByteString.Builder.Char.Ascii

import qualified System.IO.Write.Char.Ascii as W
import qualified System.IO.Write.Char.Utf8  as W


class Utf8 a where
    utf8 :: a -> Builder

instance Utf8 Char where
    {-# INLINE utf8 #-}
    utf8 = fromWrite W.utf8

instance Utf8 a => Utf8 [a] where
    {-# INLINE utf8 #-}
    utf8 = foldMap utf8

utf8HexLower :: AsciiHex a => a -> Builder
utf8HexLower = asciiHexLower

utf8HexUpper :: AsciiHex a => a -> Builder
utf8HexUpper = asciiHexUpper

utf8HexLowerNoLead :: W.AsciiHexWritable a => a -> Builder
utf8HexLowerNoLead = asciiHexLowerNoLead

utf8HexUpperNoLead :: W.AsciiHexWritable a => a -> Builder
utf8HexUpperNoLead = asciiHexUpperNoLead
