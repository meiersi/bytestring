-- |
-- Copyright   : (c) 2010      Jasper Van der Jeugt 
--               (c) 2010-2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
module Data.ByteString.Builder.Char.Utf8
    ( 
      -- * Creating Builders by UTF-8 encoding character sequences
      Utf8(..)
    , utf8Show

      -- ** Special encodings as character sequences fused with UTF-8 encoding
    , Hex(..)
    ) where

import Data.Monoid

import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy.Internal as L

import Data.ByteString.Builder.Internal
import Data.ByteString.Builder.Write

import qualified System.IO.Write.Char.Utf8 as Utf8

import Foreign

class Utf8 a where
    utf8 :: a -> Builder

    {-# INLINE utf8List #-}
    utf8List :: [a] -> Builder
    utf8List = mconcat . map utf8

instance Utf8 a => Utf8 [a] where
    {-# INLINE utf8 #-}
    utf8 = utf8List


instance Utf8 Char where
    {-# INLINE utf8 #-}
    {-# INLINE utf8List #-}
    utf8     = fromWrite Utf8.char
    utf8List = fromWriteList Utf8.char


-- | /O(n)/. Serialize a value by 'Show'ing it and UTF-8 encoding the resulting
-- 'String'.
--
utf8Show :: Show a => a -> Builder
utf8Show = utf8 . show


-- TODO: Create ASCII encoding module that provides this hex encoding

class Hex a where
    hexLower     :: a   -> Builder

    {-# INLINE hexLowerList #-}
    hexLowerList :: [a] -> Builder
    hexLowerList = mconcat . map hexLower

instance Hex a => Hex [a] where
    {-# INLINE hexLower #-}
    hexLower = hexLowerList


instance Hex Int8 where
    {-# INLINE hexLower #-}
    {-# INLINE hexLowerList #-}
    hexLower     = fromWrite     Utf8.hexLower
    hexLowerList = fromWriteList Utf8.hexLower
    

instance Hex S.ByteString where
    hexLower = mapWriteByteString Utf8.hexLower


instance Hex L.ByteString where
    hexLower = mapWriteLazyByteString Utf8.hexLower
