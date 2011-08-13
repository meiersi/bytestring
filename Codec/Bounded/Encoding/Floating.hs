{-# LANGUAGE ScopedTypeVariables, MonoPatBinds, MagicHash #-}
-- |
-- Copyright   : (c) 2010 Simon Meier
--
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (uses unsafeCoerce)
--
-- 'Encoding's for encoding floating point numbers represented as 'Float' or
-- 'Double' values using big-endian, little-endian, and host-endian encodings.
--
module Codec.Bounded.Encoding.Floating
    ( 

    -- * Big-endian encodings
      floatBE
    , doubleBE

    -- * Little-endian encodings
    , floatLE
    , doubleLE

    -- * Host-endian encodings
    , floatHost
    , doubleHost

    -- * Coercions for internal use
    , coerceFloatToWord32
    , coerceDoubleToWord64
    ) where


import Codec.Bounded.Encoding.Internal (Encoding, writeStorable, (#.) )
import Codec.Bounded.Encoding.Word     (word32BE, word32LE, word64BE, word64LE)

import Foreign 

-- | Coerce a 'Float' to a 'Word32' as-is.
{-# INLINE coerceFloatToWord32 #-}
coerceFloatToWord32 :: Float -> Word32
coerceFloatToWord32 = fromFloat

-- | Coerce a 'Double' to a 'Word64' as-is.
{-# INLINE coerceDoubleToWord64 #-}
coerceDoubleToWord64 :: Double -> Word64
coerceDoubleToWord64 = fromFloat

-- The implementation of the following function is based on
--
-- http://hackage.haskell.org/package/data-binary-ieee754-0.4.2.1
--
-- Module: Data.Binary.IEEE754
-- Copyright: 2010 John Millikin <jmillikin@gmail.com>
-- License: MIT
--
fromFloat :: forall w f. (Storable w, Storable f, RealFloat f) => f -> w
fromFloat x
  | isIEEE x && sizeOf (undefined :: f) == sizeOf (undefined :: w) =
      unsafePerformIO $ alloca $ \buf -> do
	poke (castPtr buf) x
	peek buf
  | otherwise = error 
      "Coded.Bounded.Encoding.Floating: missing support for encoding floating point numbers on your platform!"

{- The speed of the above implementation is not great. The plan is to use the
   implementations below for real speed once the following ticket is solved:
 
   See http://hackage.haskell.org/trac/ghc/ticket/4092
  
-- | Coerce a 'Float' to a 'Word32'; i.e., interpret the 32-bit 'Float' value
-- as an unsigned 32-bit 'Int. 
--
-- FIXME: Check with GHC developers if this is really safe. Does the register
-- allocater handle such a case correctly, if the 'Float' is in an FPU
-- register?
{-# INLINE coerceFloatToWord32 #-}
coerceFloatToWord32 :: Float -> Word32
coerceFloatToWord32 = unsafeCoerce

-- | Coerce a 'Double' to a 'Word64'.
{-# INLINE coerceDoubleToWord64 #-}
coerceDoubleToWord64 :: Double -> Word64
coerceDoubleToWord64 = unsafeCoerce
-}

-- | Encode a 'Float' in big endian format.
{-# INLINE floatBE #-}
floatBE :: Encoding Float
floatBE = word32BE #. coerceFloatToWord32

-- | Encode a 'Float' in little endian format.
{-# INLINE floatLE #-}
floatLE :: Encoding Float
floatLE = word32LE #. coerceFloatToWord32

-- | Encode a 'Double' in big endian format.
{-# INLINE doubleBE #-}
doubleBE :: Encoding Double
doubleBE = word64BE #. coerceDoubleToWord64

-- | Encode a 'Double' in little endian format.
{-# INLINE doubleLE #-}
doubleLE :: Encoding Double
doubleLE = word64LE #. coerceDoubleToWord64


-- | Encode a 'Float' in native host order and host endianness. Values written
-- this way are not portable to different endian machines, without conversion.
--
{-# INLINE floatHost #-}
floatHost :: Encoding Float
floatHost = writeStorable

-- | Encode a 'Double' in native host order and host endianness.
{-# INLINE doubleHost #-}
doubleHost :: Encoding Double
doubleHost = writeStorable

