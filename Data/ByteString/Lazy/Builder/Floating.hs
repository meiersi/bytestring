{-# LANGUAGE CPP, MonoPatBinds #-}
-- | Copyright: (c) 2010 Simon Meier
-- License    : BSD3-style (see LICENSE)
-- 
-- Maintainer : Simon Meier <iridcode@gmail.com>
-- Stability  : experimental
-- Portability: tested on GHC only
--
-- Constructing 'Builder's by encoding IEEE-754 floating point numbers
-- represented as 'Float' and 'Double' values using big-endian, little-endian,
-- and host-endian encodings.
--
module Data.ByteString.Lazy.Builder.Floating
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

    ) where

import Data.ByteString.Lazy.Builder.Internal
import Data.ByteString.Lazy.Builder.BoundedEncoding
import qualified Codec.Bounded.Encoding as E

-- | Encode a 'Float' in big endian format.
{-# INLINE floatBE #-}
floatBE :: Float -> Builder
floatBE = encodeWith E.floatBE

-- | Encode a 'Float' in little endian format.
{-# INLINE floatLE #-}
floatLE :: Float -> Builder
floatLE = encodeWith E.floatLE

-- | Encode a 'Double' in big endian format.
{-# INLINE doubleBE #-}
doubleBE :: Double -> Builder
doubleBE = encodeWith E.doubleBE

-- | Encode a 'Double' in little endian format.
{-# INLINE doubleLE #-}
doubleLE :: Double -> Builder
doubleLE = encodeWith E.doubleLE

-- | Encode a single native machine 'Int'. The 'Int' is encoded in host order,
-- host endian form, for the machine you're on. On a 64 bit machine the 'Int'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values encoded this way
-- are not portable to different endian or int sized machines, without
-- conversion.
--
-- | Encode a 'Float' in native host order. Values encoded this way are not
-- portable to different endian machines, without conversion.

{-# INLINE floatHost #-}
floatHost :: Float -> Builder
floatHost = encodeWith E.floatHost

-- | Encode a 'Double' in native host order.
{-# INLINE doubleHost #-}
doubleHost :: Double -> Builder
doubleHost = encodeWith E.doubleHost


