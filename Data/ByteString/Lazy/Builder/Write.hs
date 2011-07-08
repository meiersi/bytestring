{-# LANGUAGE CPP, BangPatterns, MonoPatBinds #-}
-- |
-- Module      : Data.ByteString.Lazy.Builder.Write
-- Copyright   : (c) 2010-2011 Simon Meier
--             . (c) 2010      Jasper van der Jeugt
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
module Data.ByteString.Lazy.Builder.Write (

    fromWrite
  , fromWriteList
  , unfoldrWrite
  
  , mapWriteByteString
  , mapWriteLazyByteString

  ) where

import Data.ByteString.Lazy.Builder.Internal

import qualified Data.ByteString               as S
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy.Internal as L

import Data.Monoid

import System.IO.Write.Internal hiding (append)

import Foreign


-- IMPLEMENTATION NOTE: Sadly, 'fromWriteList' cannot be used for foldr/build
-- fusion. Its performance relies on hoisting several variables out of the
-- inner loop.  That's not possible when writing 'fromWriteList' as a 'foldr'.
-- If we had stream fusion for lists, then we could fuse 'fromWriteList', as
-- 'fromWriteStream' can keep control over the execution.


-- | Lift a 'Write' to a conversion function from a single value to a
-- 'Builder'.
--
-- We rewrite consecutive uses of 'fromWrite' such that the bound-checks are
-- fused. For example,
--
-- > fromWrite (utf8 c1) `mappend` fromWrite (utf8 c2)
--
-- is rewritten such that the resulting 'Builder' checks only once, if ther are
-- at least 8 free bytes, instead of checking twice, if there are at least 4
-- free bytes. This optimization is not observationally equivalent in a strict
-- sense, as it influences the boundaries of the generated chunks. However, for
-- a user of this library it is observationally equivalent, as chunk boundaries
-- of a lazy 'L.ByteString' can only be observed through the internal
-- interface. Morevoer, we expect that all 'Write's write much fewer than 4kb
-- (the default short buffer size). Hence, it is safe to ignore the additional
-- memory spilled due to the more agressive buffer wrapping introduced by this
-- optimization.
--
{-# INLINE[1] fromWrite #-}
fromWrite :: Write a -> (a -> Builder)
fromWrite w = 
    mkBuilder
  where
    bound = getBound w
    mkBuilder x = builder step
      where
        step k (BufferRange op ope)
          | op `plusPtr` bound <= ope = do
              op' <- runWrite w x op
              let !br' = BufferRange op' ope
              k br'
          | otherwise = return $ bufferFull bound op (step k)

{-# RULES 
   "append/fromWrite" forall w1 w2 x1 x2.
       append (fromWrite w1 x1) (fromWrite w2 x2) 
     = fromWrite (write2 w1 w2) (x1, x2) 
  #-}

-- TODO: The same rules for 'putBuilder (..) >> putBuilder (..)'

-- | Lift a 'Write' to a conversion function from a list of values to a
-- 'Builder'. This function is more efficient than the canonical
--
-- > mconcat . map (fromWrite w)
--
-- or
--
-- > foldMap (fromWrite w)
--
-- because it moves several variables out of the inner loop. 
{-# INLINE fromWriteList #-}
fromWriteList :: Write a -> [a] -> Builder
fromWriteList w = 
    makeBuilder
  where
    bound = getBound w
    makeBuilder xs0 = builder $ step xs0
      where
        step xs1 k !(BufferRange op0 ope0) = go xs1 op0
          where
            go [] !op = do
               let !br' = BufferRange op ope0
               k br'

            go xs@(x':xs') !op
              | op `plusPtr` bound <= ope0 = do
                  !op' <- runWrite w x' op
                  go xs' op'
             | otherwise = return $ bufferFull bound op (step xs k)

-- TODO: Add 'foldMap/fromWrite' its variants
-- TODO: Ensure that 'fromWrite w . f = fromWrite (w #. f)'

-- | A 'Builder' that unfolds a sequence of elements from a seed value and
-- writes each of them to the buffer.
{-# INLINE unfoldrWrite #-}
unfoldrWrite :: Write b -> (a -> Maybe (b, a)) -> a -> Builder
unfoldrWrite w = 
    makeBuilder
  where
    bound = getBound w
    makeBuilder f x0 = builder $ step x0
      where
        step x1 !k = fill x1
          where
            fill x !(BufferRange pf0 pe0) = go (f x) pf0
              where
                go !Nothing        !pf = do
                    let !br' = BufferRange pf pe0
                    k br'
                go !(Just (y, x')) !pf
                  | pf `plusPtr` bound <= pe0 = do
                      !pf' <- runWrite w y pf
                      go (f x') pf'
                  | otherwise = return $ bufferFull bound pf $ 
                      \(BufferRange pfNew peNew) -> do 
                          !pfNew' <- runWrite w y pfNew
                          fill x' (BufferRange pfNew' peNew)

-- | Consecutively execute a 'Write' on every byte of a strict 'S.ByteString'.
{-# INLINE mapWriteByteString #-}
mapWriteByteString :: Write Word8 -> S.ByteString -> Builder
mapWriteByteString w =
    \bs -> builder $ step bs
  where
    bound = getBound w
    step (S.PS ifp ioff isize) !k = 
        goBS (unsafeForeignPtrToPtr ifp `plusPtr` ioff)
      where
        !ipe = unsafeForeignPtrToPtr ifp `plusPtr` (ioff + isize)
        goBS !ip0 !br@(BufferRange op0 ope)
          | ip0 >= ipe = do 
              touchForeignPtr ifp -- input buffer consumed
              k br

          | op0 `plusPtr` bound < ope = 
              goPartial (ip0 `plusPtr` min outRemaining inpRemaining)

          | otherwise  = return $ bufferFull bound op0 (goBS ip0) 
          where
            outRemaining = (ope `minusPtr` op0) `div` bound
            inpRemaining = ipe `minusPtr` ip0 

            goPartial !ipeTmp = go ip0 op0
              where
                go !ip !op
                  | ip < ipeTmp = do
                      x   <- peek ip
                      op' <- runWrite w x op
                      go (ip `plusPtr` 1) op'
                  | otherwise =
                      goBS ip (BufferRange op ope)

-- | Chunk-wise application of 'mapWriteByteString'.
{-# INLINE mapWriteLazyByteString #-}
mapWriteLazyByteString :: Write Word8 -> L.ByteString -> Builder
mapWriteLazyByteString w = 
    L.foldrChunks (\x b -> mapWriteByteString w x `mappend` b) mempty
