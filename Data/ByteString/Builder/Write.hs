{-# LANGUAGE CPP, BangPatterns, MonoPatBinds #-}
-- |
-- Copyright   : (c) 2010-2011 Simon Meier
--               (c) 2010      Jasper van der Jeugt
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- FIXME: Improve documentation.
--
module Data.ByteString.Builder.Write (

  -- * Constructing builders from @Write@s
    fromWrite
  , fromWriteSingleton
  , fromWriteList

  ) where

import Data.ByteString.Builder.Internal

import Foreign

import System.IO.Write.Internal

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
