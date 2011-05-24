-- |
-- Copyright   : (c) 2010      Jasper Van der Jeugt 
--               (c) 2010-2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
module Data.ByteString.Builder.Char
    ( 
      -- * Creating Builders by UTF-8 encoding character sequences
      fromCharUtf8
    , fromStringUtf8
    , fromShowUtf8
    ) where

import Data.ByteString.Builder.Internal
import Data.ByteString.Builder.Write

import qualified System.IO.Write.Char.Utf8 as Utf8

-- | /O(1)/. Serialize a Unicode character using the UTF-8 encoding.
--
fromCharUtf8 :: Char -> Builder
fromCharUtf8 = fromWrite Utf8.char

-- | /O(n)/. Serialize a Unicode 'String' using the UTF-8 encoding.
--
fromStringUtf8 :: String -> Builder
fromStringUtf8 = fromWriteList Utf8.char

-- | /O(n)/. Serialize a value by 'Show'ing it and UTF-8 encoding the resulting
-- 'String'.
--
fromShowUtf8 :: Show a => a -> Builder
fromShowUtf8 = fromStringUtf8 . show
