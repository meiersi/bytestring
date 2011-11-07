-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- This module is intended to be imported qualified. It provides a minimal
-- version of bytestrings for testing encodings and building encoding tables.  It
-- is neither tuned for efficiency nor does it provide much type-safety.
-- 
module Codec.Bounded.Encoding.Internal.Region (
    Region
  , fromList
  , toList
  , unsafeIndex
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Foreign

-- | A region of memory referenced by a base pointer and its size.
type Region = (ForeignPtr Word8, Int)

-- | Convert a list to a 'Region'.
fromList :: [Word8] -> IO Region 
fromList ys0 = do
    let n = length ys0
    fpbuf <- mallocForeignPtrBytes n
    withForeignPtr fpbuf $ \sp -> do
        ep <- fill ys0 sp
        return (fpbuf, ep `minusPtr` sp)
  where
    fill []     op = do return op
    fill (y:ys) op = do poke op y
                        fill ys (op `plusPtr` 1)

-- | Convert a 'Region' to a list.
toList :: Region -> IO [Word8]
toList (fpbuf, size) = 
    withForeignPtr fpbuf $ \sp0 -> do
        let ep0 = sp0 `plusPtr` size
            go sp | sp < ep0  = (:) <$> peek sp <*> go (sp `plusPtr` 1)
                  | otherwise = return []
        go sp0

-- | @unsafeIndex r i@ returns the value of the @i@-th byte in 'Region' @r@.
unsafeIndex :: Region -> Int -> IO Word8
unsafeIndex = peekElemOff . unsafeForeignPtrToPtr . fst

