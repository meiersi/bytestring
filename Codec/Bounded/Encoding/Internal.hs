{-# LANGUAGE ScopedTypeVariables, CPP, BangPatterns, MonoPatBinds #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Copyright   : 2010, 2011 Simon Meier, 2010 Jasper van der Jeugt
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- This module is internal. It is only intended to be used by the 'bytestring'
-- and the 'text' library. Please contact the maintainer, if you need to use
-- this module in your library. We are glad to accept patches for further
-- standard encodings of standard Haskell values.
--
-- If you need to write your own primitive encoding, then be aware that you are
-- writing code with /all saftey belts off/; i.e., 
-- *this is the code that might make your application vulnerable to buffer-overflow attacks!* 
-- The "Codec.Bounded.Encoding.Internal.Test" module provides you with
-- utilities for testing your encodings thoroughly.
--
module Codec.Bounded.Encoding.Internal (
  -- * Poking a buffer
  
  -- | We abstract raw 'IO' actions for poking a buffer in a separate type.
  -- This allows us to change the actual implementation of buffer-poking in the
  -- future with minimal breakage of client code inside this library. Note
  -- that, in contrast to the 'poke' function provided by 'Storable', a 'Poke'
  -- is not restricted to poking a fixed number of bytes.
  --
    Poke
  , pokeIO
  , pokeN

  -- * Encodings
  , Encoding
  , runEncoding
  , getBound
  , getPoke

  -- ** Unsafe creation of Encodings
  , boundedEncoding
  , exactEncoding
  , writeStorable

  -- ** Using IO inside a Encoding
  , liftIO

  -- ** Safe combinators

  -- | The following combinators ensure that the bound on the maximal number of
  -- bytes written is always computed correctly. Hence, applying them to safe
  -- encodings always results in a safe encoding. Moreover, care is taken to
  -- compute that bound such that the compiler can optimize it to a compile
  -- time constant, if that is possible.
  
  -- *** Basic building blocks
  
  -- | These combinators cannot be implemented without breaking the 'Encoding'
  -- abstraction.
  , (#.)
  , comapEncoding
  , (<#>)
  , encode2
  , emptyEncoding
  , encodeIf
  , encodeMaybe
  , encodeEither

  -- *** Convenience
  
  -- | These combinators can be implemented on top of the basic building
  -- blocks. We provide them here for convenience.
  , (#>)
  , prepend 
  , (<#)
  , append 
  , encode3
  , encode4
  , encode8
  
  ) where

import Control.Monad ( (>=>) )
import Data.Monoid
import Foreign
import Prelude hiding (maxBound)

------------------------------------------------------------------------------
-- Poking a buffer
------------------------------------------------------------------------------

-- Sadly GHC is not smart enough: code where we branch and each branch should
-- execute a few IO actions and then return a value cannot be taught to GHC. At
-- least not such that it returns the value of the branches unpacked.
--
-- Hmm.. at least he behaves much better for the Monoid instance of Encoding
-- than the one for Poke. Serializing UTF-8 chars gets a slowdown of a
-- factor 2 when 2 chars are composed. Perhaps I should try out the writeList
-- instances also, as they may be more sensitive to too much work per Char.
--

-- | Poking a sequence of bytes. 'Poke's can be sequenced using their 'Monoid'
-- instance. 
newtype Poke = Poke { runPoke :: Ptr Word8 -> IO (Ptr Word8) }

instance Monoid Poke where
  {-# INLINE mempty #-}
  mempty = Poke $ return

  {-# INLINE mappend #-}
  (Poke po1) `mappend` (Poke po2) = Poke $ po1 >=> po2

  {-# INLINE mconcat #-}
  mconcat = foldr mappend mempty

-- | Poke a sequence of bytes starting from the given pointer and return the
-- pointer to the next free byte.
--
-- Note that the 'IO' action underlying this 'Poke' must poke /exactly/ the
-- bytes between the given start-pointer and the returned end-pointer. If more bytes
-- were poked, then data outside the buffer might be overwritten; i.e, the
-- resulting code would likely be vulnerable to a buffer-overflow attack. If
-- fewer bytes were poked, then some sensitive data might leak because not all
-- data in the buffer is overwritten.
{-# INLINE pokeIO #-}
pokeIO :: (Ptr Word8 -> IO (Ptr Word8)) -> Poke
pokeIO = Poke

-- | An abbrevation for constructing 'Poke's of fixed-length sequences.
--
-- /Preconditions:/ the given number of the poked bytes must agree precisely
-- with the actual implementation (analogously to 'pokeIO').
{-# INLINE pokeN #-}
pokeN :: Int -> (Ptr Word8 -> IO ()) -> Poke
pokeN size io = Poke $ \op -> io op >> return (op `plusPtr` size)



------------------------------------------------------------------------------
-- Writing to a buffer
------------------------------------------------------------------------------

infixl 4 #.   -- comapEncoding
infixr 5 <#>  -- encode2
infixr 5 #>   -- prepend
infixl 4 <#   -- append


-- | Encodings of Haskell values that result in a bounded number of bytes.
data Encoding a = Encoding {-# UNPACK #-} !Int (a -> Poke)

-- | Get the raw encoding function encapsulated by an 'Encoding'.
{-# INLINE getPoke #-}
getPoke :: Encoding a -> a -> Poke
getPoke (Encoding _ f) = f

-- | The maximal number of bytes produced by an 'Encoding'.
{-# INLINE getBound #-}
getBound :: Encoding a -> Int
getBound (Encoding b _) = b

-- | Run an 'Encoding' to encode a value starting from the given pointer and return
-- the pointer of the next free byte.
{-# INLINE runEncoding #-}
runEncoding :: Encoding a -> a -> Ptr Word8 -> IO (Ptr Word8)
runEncoding (Encoding _ f) = runPoke . f

-- | Utility function to compute the maximal bound of two encodings.
{-# INLINE maxBound #-}
maxBound :: Encoding a -> Encoding b -> Int
maxBound w1 w2 = max (getBound w1) (getBound w2)

-- | Utility function to compute the sum of the bounds of two encodings.
{-# INLINE addBounds #-}
addBounds :: Encoding a -> Encoding b -> Int
addBounds w1 w2 = getBound w1 + getBound w2


-- Unsafe creation of encodings
-------------------------------

-- | Create an 'Encoding' from a bound on the maximal number of bytes written
-- and an implementation of the encoding scheme.
--
-- /Precondition:/ the bound must be valid for the implementation of the
-- encoding scheme.
{-# INLINE boundedEncoding #-}
boundedEncoding :: Int          -- ^ Maximal number of bytes written
             -> (a -> Poke)  -- ^ Implementation of the encoding scheme
             -> Encoding a      
boundedEncoding = Encoding

-- | Create an 'Encoding' from an 'IO' action that encodings a fixed number of bytes.
--
-- /Preconditions:/ the given number of the written bytes must agree precisely
-- with the actual implementation (analogously to 'pokeIO').
{-# INLINE exactEncoding #-}
exactEncoding :: Int                       -- ^ Number of bytes written
           -> (a -> Ptr Word8 -> IO ()) -- ^ 'IO' action writing exactly that
                                        -- many bytes from the given start pointer
           -> Encoding a
exactEncoding size io = Encoding size (pokeN size . io)

-- | 'Encoding' a 'Storable' value using 'poke'.
{-# INLINE writeStorable #-}
writeStorable :: forall a. Storable a => Encoding a
writeStorable = 
    exactEncoding (sizeOf (undefined :: a)) (\x op -> poke (castPtr op) x)


-- Safe combinators
-------------------

-- | 'Encoding's are cofunctors. The following laws hold.
--
-- > w #. id      = w
-- > w #. (f . g) = w #. f #. g
--
-- A typical use of 'comapEncoding' is the definition of 
--
-- > int32 = word32 #. fromIntegral@
--
-- Once the the base library provides a Cofunctor class, we will make 'Encoding' an instance of it.
{-# INLINE comapEncoding #-}
comapEncoding :: (b -> a) -> Encoding a -> Encoding b
comapEncoding g (Encoding b f) = Encoding b (f . g)

-- | An infix synonym for 'comapEncoding'.
{-# INLINE (#.) #-}
(#.) :: Encoding a -> (b -> a) -> Encoding b
(#.) = flip comapEncoding

-- | Prepend the a fixed sequence of bytes to an 'Encoding'.
--
-- >   showEncoding ((utf8, '0') #> (utf8, 'x') #> utf8HexLower) (26 :: Word16) 
-- > = "0x001a"
--
{-# INLINE prepend #-}
prepend :: (Encoding a, a) -> Encoding b -> Encoding b
prepend (w1, x) w2 = encode2 w1 w2 #. (\y -> (x, y))

-- | An infix synonym for 'prepend'.
{-# INLINE (#>) #-}
(#>) :: (Encoding a, a) -> Encoding b -> Encoding b
(#>) = prepend


-- | Append the a fixed sequence of bytes to an 'Encoding'.
--
-- >   showEncoding (utf8HexLower <# (utf8, '\'')) (26 :: Word16) 
-- > = "001a'"
--
{-# INLINE append #-}
append :: Encoding a -> (Encoding b, b) -> Encoding a
append w1 (w2, y) = encode2 w1 w2 #. (\x -> (x, y))

-- | An infix synonym for 'append'.
{-# INLINE (<#) #-}
(<#) :: Encoding a -> (Encoding b, b) -> Encoding a
(<#) = append

-- | The encoding that always results in the empty sequence of bytes. 
-- Note that 'emptyEncoding' does not inspect its argument at all; i.e.,
--
-- > showEncoding emptyEncoding undefined = ""
--
{-# INLINE emptyEncoding #-}
emptyEncoding :: Encoding a
emptyEncoding = Encoding 0 mempty

-- | Conditionally select an 'Encoding'.
--
-- > asciiDrop = encodeIf (< '\128') unsafeAscii emptyEncoding
--
{-# INLINE encodeIf #-}
encodeIf :: (a -> Bool) -> Encoding a -> Encoding a -> Encoding a
encodeIf p wTrue wFalse = 
    Encoding (maxBound wTrue wFalse) f
  where
    f x = Poke $ if p x then runEncoding wTrue x else runEncoding wFalse x

-- | Select an 'Encoding' depending on a 'Maybe' value.
{-# INLINE encodeMaybe #-}
encodeMaybe :: (Encoding a, a) -> Encoding b -> Encoding (Maybe b)
encodeMaybe (wNothing, x) wJust =
    Encoding (max (getBound wNothing) (getBound wJust))
          (Poke . maybe (runEncoding wNothing x) (runEncoding wJust))

-- | Select an 'Encoding' depending on an 'Either' value.
{-# INLINE encodeEither #-}
encodeEither :: Encoding a
            -> Encoding b
            -> Encoding (Either a b)
encodeEither wLeft wRight =
    Encoding (maxBound wLeft wRight)
          (Poke . either (runEncoding wLeft) (runEncoding wRight))

-- | A right-associative infix synonym for 'encode2', which composes
-- two 'Encoding's sequentially. For example,
--
-- > showEncoding (char <#> char) ('x','y') = "xy"
--
-- where 'Data.ByteString.Lazy.Builder.BoundedEncoding.Utf8.char' UTF-8 encodes a 'Char'. 
-- You can combine multiple encodings using '(<#>)' multiple times.
--
-- > showEncoding (char <#> char <#> char) ('x',('y','z')) = 'xyz'
--
-- Combined with '#.' you can also prepend and/or append fixed values
-- to an encoding.
--
-- > showEncoding (char <#> char <#> char #. (\x -> ('"', (x, '"')))) 'x' = '"x"'
--
{-# INLINE (<#>) #-}
(<#>) :: Encoding a -> Encoding b -> Encoding (a, b)
(<#>) = encode2

-- | Sequentially compose two 'Encoding's. 
{-# INLINE encode2 #-}
encode2 :: Encoding a -> Encoding b -> Encoding (a, b)
encode2 w1 w2 =
    Encoding (addBounds w1 w2) f
  where
    f (a, b) = getPoke w1 a `mappend` getPoke w2 b

-- | Sequentially compose three 'Encoding's.
{-# INLINE encode3 #-}
encode3 :: Encoding a -> Encoding b -> Encoding c -> Encoding (a, b, c)
encode3 w1 w2 w3 =
    Encoding (addBounds w1 w2 + getBound w3) f
  where
    f (a, b, c) = getPoke w1 a `mappend` getPoke w2 b 
                                 `mappend` getPoke w3 c

-- | Sequentially compose four 'Encoding's.
{-# INLINE encode4 #-}
encode4 :: Encoding a -> Encoding b -> Encoding c -> Encoding d -> Encoding (a, b, c, d)
encode4 w1 w2 w3 w4 =
    Encoding (addBounds w1 w2 + addBounds w3 w4) f
  where
    f (a, b, c, d) = getPoke w1 a `mappend` getPoke w2 b 
                                    `mappend` getPoke w3 c
                                    `mappend` getPoke w4 d

-- | Sequentially compose eight 'Encoding's.
{-# INLINE encode8 #-}
encode8 :: Encoding a1 -> Encoding a2 -> Encoding a3 -> Encoding a4
       -> Encoding a5 -> Encoding a6 -> Encoding a7 -> Encoding a8
       -> Encoding (a1, a2, a3, a4, a5, a6, a7, a8)
encode8 w1 w2 w3 w4 w5 w6 w7 w8=
    Encoding (addBounds w1 w2 + addBounds w3 w4 +
           addBounds w5 w6 + addBounds w7 w8) f
  where
    f (x1, x2, x3, x4, x5, x6, x7, x8) =
        getPoke w1 x1 `mappend` getPoke w2 x2 `mappend`
        getPoke w3 x3 `mappend` getPoke w4 x4 `mappend`
        getPoke w5 x5 `mappend` getPoke w6 x6 `mappend`
        getPoke w7 x7 `mappend` getPoke w8 x8


-- IO inside a Encoding
--------------------

-- | @liftIO io encoding@ creates a bounded encoding that executes the @io@
-- action to compute the value that is then encoded.
{-# INLINE liftIO #-}
liftIO :: Encoding a -> Encoding (IO a)
liftIO w =
    Encoding (getBound w) (\io -> Poke $ \op -> do x <- io; runEncoding w x op)


