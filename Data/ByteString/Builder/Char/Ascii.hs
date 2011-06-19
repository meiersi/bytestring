-- | Copyright : (c) 2010      Jasper Van der Jeugt 
--               (c) 2010-2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
module Data.ByteString.Builder.Char.Ascii
    ( 
      -- * ASCII encoding of characters
      Ascii(..)

      -- * Hexadecimal encoding using ASCII encoded characters
    , asciiHexLowerNoLead
    , asciiHexUpperNoLead
    , AsciiHex(..)
    ) where

import Data.Foldable (foldMap)

import Data.ByteString.Builder.Internal
import Data.ByteString.Builder.Write

import qualified System.IO.Write.Char.Ascii as W

import Foreign

asciiHexLowerNoLead :: W.AsciiHexWritable a => a -> Builder
asciiHexLowerNoLead = fromWrite W.asciiHexLowerNoLead

asciiHexUpperNoLead :: W.AsciiHexWritable a => a -> Builder
asciiHexUpperNoLead = fromWrite W.asciiHexUpperNoLead

class Ascii a where
    ascii        :: a -> Builder
    asciiReplace :: Char -> a -> Builder
    asciiDrop    :: a -> Builder

instance Ascii Char where
    ascii          = fromWrite W.ascii
    asciiReplace c = fromWrite (W.asciiReplace c)
    asciiDrop      = fromWrite W.asciiDrop

instance Ascii a => Ascii [a] where
    {-# INLINE ascii #-}
    ascii          = foldMap ascii
    {-# INLINE asciiReplace #-}
    asciiReplace c = foldMap (asciiReplace c)
    {-# INLINE asciiDrop #-}
    asciiDrop      = foldMap asciiDrop


class AsciiHex a where
    asciiHexLower :: a -> Builder
    asciiHexUpper :: a -> Builder

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


