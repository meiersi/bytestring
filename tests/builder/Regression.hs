{-# LANGUAGE PackageImports, CPP, BangPatterns, MonoPatBinds #-}
-- |
-- Copyright   : (c) 2010-2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Performance and compatibility regression tests for the Builder.
--
module Regression where

import qualified "blaze-builder" Blaze.ByteString.Builder as Blaze

import qualified "new-bytestring" Data.ByteString.Lazy    as NewL
import qualified "new-bytestring" Data.ByteString.Builder as NewL

import qualified "bytestring" Data.ByteString.Lazy as OldL

import Foreign
import Criterion.Main

------------------------------------------------------------------------------
-- Benchmark Data
------------------------------------------------------------------------------

n :: Int
n = 10000

nShort :: Int
nShort = 64

{-# NOINLINE word8s #-}
word8s :: [Word8]
word8s = take n $ map fromIntegral $ [(0::Int)..]

{-# NOINLINE shortWord8s #-}
shortWord8s :: [Word8]
shortWord8s = take nShort word8s

------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

newBS, oldBS, blaze :: String
newBS = "new-bytestring"
oldBS = "old-bytestring"
blaze = "blaze"

main :: IO ()
main = defaultMain
    [ benchmark ("packing [Word8] length " ++ show n)
        [ (newBS, NewL.length . NewL.pack)
        , (oldBS, OldL.length . OldL.pack)
        , (blaze, OldL.length . Blaze.toLazyByteString . Blaze.fromWord8s)
        ]
        word8s
    , benchmark ("packing [Word8] length " ++ show nShort)
        [ (newBS, NewL.length . NewL.pack)
        , (oldBS, OldL.length . OldL.pack)
        , (blaze, OldL.length . Blaze.toLazyByteString . Blaze.fromWord8s)
        ]
        shortWord8s
    ]
  where
    benchmark gname tasks x =
        bgroup gname $ map mkBench tasks
      where
        mkBench (name, f) = bench name $ whnf f x


{-
import Foreign

import Data.Monoid
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString.Lazy          as L

{-
-- | Create a builder that execute a single 'Write'.
{-# INLINE fromWrite #-}
fromWrite :: Write -> Builder
fromWrite write =
    fromBuildStepCont step
  where
    step k (BufRange op ope)
      | op `plusPtr` getBound write <= ope = do
          op' <- runWrite write op
          let !br' = BufRange op' ope
          k br'
      | otherwise = return $ bufferFull (getBound write) op (step k)

{-# INLINE fromWriteSingleton #-}
fromWriteSingleton :: (a -> Write) -> (a -> Builder)
fromWriteSingleton write = 
    mkBuilder
  where
    bound = getBound' "fromWriteSingleton" write
    mkBuilder x = fromBuildStepCont step
      where
        step k (BufRange op ope)
          | op `plusPtr` bound <= ope = do
              op' <- runWrite (write x) op
              let !br' = BufRange op' ope
              k br'
          | otherwise = return $ bufferFull bound op (step k)

-- | Construct a 'Builder' writing a list of data one element at a time.
{-# INLINE fromWriteList #-}
fromWriteList :: (a -> Write) -> [a] -> Builder
fromWriteList write = 
    makeBuilder
  where
    bound = getBound' "fromWriteList" write
    makeBuilder xs0 = fromBuildStepCont $ step xs0
      where
        step xs1 k !(BufRange op0 ope0) = go xs1 op0
          where
            go [] !op = do
               let !br' = BufRange op ope0
               k br'

            go xs@(x':xs') !op
              | op `plusPtr` bound <= ope0 = do
                  !op' <- runWrite (write x') op
                  go xs' op'
              | otherwise = return $ bufferFull bound op (step xs k)
-}

-- | A 'Builder' that unfolds a sequence of elements from a seed value and
-- writes each of them to the buffer.
{-# INLINE fromWriteUnfoldr #-}
fromWriteUnfoldr :: (b -> Write) -> (a -> Maybe (b, a)) -> a -> Builder
fromWriteUnfoldr write = 
    makeBuilder
  where
    bound = getBound' "fromWriteUnfoldr" write
    makeBuilder f x0 = fromBuildStepCont $ step x0
      where
        step x1 !k = fill x1
          where
            fill x !(BufRange pf0 pe0) = go (f x) pf0
              where
                go !Nothing        !pf = do
                    let !br' = BufRange pf pe0
                    k br'
                go !(Just (y, x')) !pf
                  | pf `plusPtr` bound <= pe0 = do
                      !pf' <- runWrite (write y) pf
                      go (f x') pf'
                  | otherwise = return $ bufferFull bound pf $ 
                      \(BufRange pfNew peNew) -> do 
                          !pfNew' <- runWrite (write y) pfNew
                          fill x' (BufRange pfNew' peNew)


{-# INLINE mapWriteByteString #-}
mapWriteByteString :: (Word8 -> Write) -> S.ByteString -> Builder
mapWriteByteString write =
    \bs -> fromBuildStepCont $ step bs
  where
    step (S.PS ifp ioff isize) !k = 
        goBS (unsafeForeignPtrToPtr ifp `plusPtr` ioff)
      where
        !ipe = unsafeForeignPtrToPtr ifp `plusPtr` (ioff + isize)
        goBS !ip0 !br@(BufRange op0 ope)
          | ip0 >= ipe = do 
              touchForeignPtr ifp -- input buffer consumed
              k br

          | op0 `plusPtr` writeBound < ope = 
              goPartial (ip0 `plusPtr` min outRemaining inpRemaining)

          | otherwise  = return $ bufferFull writeBound op0 (goBS ip0) 
          where
            writeBound   = getBound' "mapWriteByteString" write 
            outRemaining = (ope `minusPtr` op0) `div` writeBound
            inpRemaining = ipe `minusPtr` ip0 

            goPartial !ipeTmp = go ip0 op0
              where
                go !ip !op
                  | ip < ipeTmp = do
                      w   <- peek ip
                      op' <- runWrite (write w) op
                      go (ip `plusPtr` 1) op'
                  | otherwise =
                      goBS ip (BufRange op ope)

{-# INLINE mapWriteLazyByteString #-}
mapWriteLazyByteString :: (Word8 -> Write) -> L.ByteString -> Builder
mapWriteLazyByteString write = 
    L.foldrChunks (\w b -> mapWriteByteString write w `mappend` b) mempty

{-# INLINE mapBlaze #-}
mapBlaze :: (Word8 -> Word8) -> L.ByteString -> L.ByteString
mapBlaze f = toLazyByteString . mapWriteLazyByteString (writeWord8 . f)

{-# INLINE filterBlaze #-}
filterBlaze :: (Word8 -> Bool) -> L.ByteString -> L.ByteString
filterBlaze p = 
    toLazyByteString . mapWriteLazyByteString write
  where
    write = writeIf p writeWord8 (const mempty)

test = mapBlaze (+1) $ L.take 1000 $ L.cycle $ L.pack [1..100]

-}
