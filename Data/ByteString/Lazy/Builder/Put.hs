{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- | Copyright : (c) 2012 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Portability : GHC
--
-- The 'Put' monad generalizes 'Builder's to monadic actions that write a
-- stream of bytes as their side-effect.
-----------------------------------------------------------------------------
module Data.ByteString.Lazy.Builder.Put
    (
      Put

    -- * Execution
    , hPut
    , toLazyByteString
    , toLazyByteStringWith

    -- * Conversion to and from Builders
    , putBuilder
    , fromPut

    -- * Prefixing Puts with their (chunk) size
    , putSizePrefixed
    , putChunked

    ) where

import qualified Data.ByteString.Lazy                  as L
import           Data.ByteString.Lazy.Builder.Internal
                 hiding (toLazyByteStringWith)
import           Data.ByteString.Lazy.Builder.Transformers 
                 (putChunked, putSizePrefixed)

#if MIN_VERSION_base(4,4,0)
import           System.IO.Unsafe (unsafePerformIO)
#else
import           Foreign
#endif

{-# NOINLINE toLazyByteString #-}
toLazyByteString :: Put a              -- ^ 'Put' to execute
                 -> (a, L.ByteString)  -- ^ Result and lazy 'L.ByteString'
                                       -- written as its side-effect
toLazyByteString = toLazyByteStringWith 
    (safeStrategy smallChunkSize defaultChunkSize) (\x -> (x, L.empty))


{-# INLINE toLazyByteStringWith #-}
toLazyByteStringWith
    :: AllocationStrategy
       -- ^ Buffer allocation strategy to use
    -> (a -> (b, L.ByteString))
       -- ^ Continuation to use for generating the final result and the tail
       -- of its side-effect (the written bytes).
    -> Put a
       -- ^ 'Put' to execute
    -> (b, L.ByteString)
       -- ^ Resulting lazy 'L.ByteString'
toLazyByteStringWith strategy k p =
    ciosToLazyByteString strategy k $ unsafePerformIO $
        buildStepToCIOS strategy (runPut p)
