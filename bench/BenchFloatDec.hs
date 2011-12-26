{-# LANGUAGE OverloadedStrings, PackageImports, ScopedTypeVariables, BangPatterns #-}
-- |
-- Copyright   : (c) 2011 Simon Meier
--               (c) 2011 MailRank, Inc.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Benchmarking the double to decimal encoding.
--
-- The results as of (2011/12/26) are such that 96% of the time is spent on
-- the conversion from 'Double' to an abstract 'digits + exponent'
-- representation. Only, three percent are spent on the conversion from this
-- abstract representation to the corresponding sequence of bytes. We
-- therefore choose the approach that we use Haskell's built-in conversion
-- from 'Float' to 'String' and then encode this 'String'.
module Main (main) where

import           Prelude hiding (words)

import           Data.Char (ord, chr)
import           Data.Monoid (Monoid(..))
import           Data.Foldable (foldMap)
import           Data.ByteString.Char8 ()
import qualified Data.ByteString                  as S
import qualified Data.ByteString.Lazy             as L

import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Builder.Extras
import           Data.ByteString.Lazy.Builder.ASCII
import           Data.ByteString.Lazy.Builder.BasicEncoding
                   ( FixedEncoding, BoundedEncoding, (>$<) )
import qualified Data.ByteString.Lazy.Builder.BasicEncoding          as E
import qualified Data.ByteString.Lazy.Builder.BasicEncoding.Internal as EI

import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.Text               as Blaze
import qualified "bytestring" Data.ByteString.Lazy as OldL

-- import Test.QuickCheck

import           Control.DeepSeq
import           Criterion.Main

import Foreign

------------------------------------------------------------------------------
-- Benchmark support
------------------------------------------------------------------------------

countToZero :: Int -> Maybe (Int, Int)
countToZero 0 = Nothing
countToZero n = Just (n, n - 1)


------------------------------------------------------------------------------
-- Benchmark
------------------------------------------------------------------------------

-- input data (NOINLINE to ensure memoization)
----------------------------------------------

-- | Few-enough repetitions to avoid making GC too expensive.
nRepl :: Int
nRepl = 10000

{-# NOINLINE intData #-}
intData :: [Int]
intData = [1..nRepl]

{-# NOINLINE floatData #-}
floatData :: [Float]
floatData = map (\x -> sin (fromIntegral x) * (3.14159 * fromIntegral x) ^ (30 :: Int)) intData

{-# NOINLINE doubleData #-}
doubleData :: [Double]
doubleData = map (\x -> sin (fromIntegral x) * (3.14159 * fromIntegral x) ^ (30 :: Int)) intData


-- benchmark wrappers
---------------------

{-# INLINE benchBlaze #-}
benchBlaze :: String -> a -> (a -> Blaze.Builder) -> Benchmark
benchBlaze name x b =
    bench (name ++" (" ++ show nRepl ++ ")") $
        whnf (OldL.length . Blaze.toLazyByteString . b) x

{-# INLINE benchB #-}
benchB :: String -> a -> (a -> Builder) -> Benchmark
benchB name x b =
    bench (name ++" (" ++ show nRepl ++ ")") $
        whnf (L.length . toLazyByteString . b) x


-- benchmarks
-------------

sanityCheckInfo :: [String]
sanityCheckInfo =
  [ "Sanity checks:"
  , " lengths of input data: " ++ show
      [ length intData, length floatData, length doubleData ]
  , " custom double encoder correct: " ++
      (show $ viaString == viaCustom)
  , " blaze-textual double encoder correct: " ++
      (show $ viaString == viaBlazeTextual)
  ]
  where
    viaString = L.unpack $ toLazyByteString $ foldMap doubleDec doubleData
    viaCustom = L.unpack $ toLazyByteString $ foldMap double    doubleData
    viaBlazeTextual = 
      OldL.unpack $ Blaze.toLazyByteString $ foldMap Blaze.double doubleData

main :: IO ()
main = do
  mapM_ putStrLn sanityCheckInfo
  putStrLn ""
  Criterion.Main.defaultMain
    [ bgroup "Builder"
      [ bgroup "Non-bounded encodings"
        [ 
          bench "double to digits (custom)" $ nf (map floatToDigits) doubleData
        , benchB "foldMap float (custom)"                     floatData          $ foldMap float
        , benchB "foldMap double (custom)"                    doubleData         $ foldMap double
        , benchB "foldMap floatDec (via string)"              floatData          $ foldMap floatDec
        , benchB "foldMap doubleDec (via string)"             doubleData         $ foldMap doubleDec
        , benchBlaze "foldMap floatDec (blaze-textual)"       floatData          $ foldMap Blaze.float
        , benchBlaze "foldMap doubleDec (blaze-textual)"      doubleData         $ foldMap Blaze.double
        ]
      ]
    ]

------------------------------------------------------------------------------
-- Custom decimal encoding code for 'Double's
------------------------------------------------------------------------------

-- Adapted from 'blaze-textual' module "Blaze.Text.Double.Native".
-- Copyright: (c) 2011 MailRank, Inc.  
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>

data T = T [Int] {-# UNPACK #-} !Int

instance NFData T where
    rnf (T digits e) = rnf digits `seq` rnf e `seq` ()
  

float :: Float -> Builder
float = double . realToFrac

double :: Double -> Builder
double f
    | isInfinite f              = byteStringCopy $
                                  if f > 0 then "Infinity" else "-Infinity"
    | f < 0 || isNegativeZero f = char7 '-' `mappend` goGeneric (floatToDigits (-f))
    | f >= 0                    = goGeneric (floatToDigits f)
    | otherwise                 = byteStringCopy "NaN"
  where
   goGeneric p@(T _ e)
     | e < 0 || e > 7 = goExponent p
     | otherwise      = goFixed    p
   goExponent (T is e) =
       case is of
         []     -> error "putFormattedFloat"
         [0]    -> byteStringCopy "0.0e0"
         [d]    -> digit d `mappend` byteStringCopy ".0e" `mappend` intDec (e-1)
         (d:ds) -> digit d `mappend` char7 '.' `mappend` digits ds `mappend`
                   char7 'e' `mappend` intDec (e-1)
   goFixed (T is e)
       | e <= 0    = char7 '0' `mappend` char7 '.' `mappend`
                     mconcat (replicate (-e) (char7 '0')) `mappend`
                     digits is
       | otherwise = let g 0 rs     = char7 '.' `mappend` mk0 rs
                         g n []     = char7 '0' `mappend` g (n-1) []
                         g n (r:rs) = digit r `mappend` g (n-1) rs
                     in g e is
   mk0 [] = char7 '0'
   mk0 rs = digits rs

{-# INLINE digit #-}
digit :: Int -> Builder
digit = char7 . chr . (ord '0' +)

{-# INLINE digits #-}
digits :: [Int] -> Builder
digits (d:ds) = digit d `mappend` digits ds
digits _      = mempty

floatToDigits :: Double -> T
floatToDigits 0 = T [0] 0
floatToDigits x = T (reverse rds) k
 where
  (f0, e0)     = decodeFloat x
  (minExp0, _) = floatRange (undefined::Double)
  p = floatDigits x
  b = floatRadix x
  minExp = minExp0 - p -- the real minimum exponent
  -- Haskell requires that f be adjusted so denormalized numbers
  -- will have an impossibly low exponent.  Adjust for this.
  (# f, e #) =
   let n = minExp - e0 in
   if n > 0 then (# f0 `div` (b^n), e0+n #) else (# f0, e0 #)
  (# r, s, mUp, mDn #) =
   if e >= 0
   then let be = b^ e
        in if f == b^(p-1)
           then (# f*be*b*2, 2*b, be*b, b #)
           else (# f*be*2, 2, be, be #)
   else if e > minExp && f == b^(p-1)
        then (# f*b*2, b^(-e+1)*2, b, 1 #)
        else (# f*2, b^(-e)*2, 1, 1 #)
  k = fixup k0
   where
    k0 | b == 2 = (p - 1 + e0) * 3 `div` 10
        -- logBase 10 2 is slightly bigger than 3/10 so the following
        -- will err on the low side.  Ignoring the fraction will make
        -- it err even more.  Haskell promises that p-1 <= logBase b f
        -- < p.
       | otherwise = ceiling ((log (fromInteger (f+1) :: Double) +
                               fromIntegral e * log (fromInteger b)) / log 10)
    fixup n
      | n >= 0    = if r + mUp <= exp10 n * s then n else fixup (n+1)
      | otherwise = if exp10 (-n) * (r + mUp) <= s then n else fixup (n+1)

  gen ds !rn !sN !mUpN !mDnN =
   let (dn0, rn') = (rn * 10) `divMod` sN
       mUpN' = mUpN * 10
       mDnN' = mDnN * 10
       !dn   = fromInteger dn0
       !dn'  = dn + 1
   in case (# rn' < mDnN', rn' + mUpN' > sN #) of
        (# True,  False #) -> dn : ds
        (# False, True #)  -> dn' : ds
        (# True,  True #)  -> if rn' * 2 < sN then dn : ds else dn' : ds
        (# False, False #) -> gen (dn:ds) rn' sN mUpN' mDnN'

  rds | k >= 0    = gen [] r (s * exp10 k) mUp mDn
      | otherwise = gen [] (r * bk) s (mUp * bk) (mDn * bk)
      where bk = exp10 (-k)
                    
-- | A precompiled lookup table for exponents. We don't yet have arrays on
-- this level. Hence, we have to use this rather verbose pattern matching.
exp10 :: Int -> Integer
exp10 i = case i of
   0  -> 10 ^  0
   1  -> 10 ^  1
   2  -> 10 ^  2
   3  -> 10 ^  3
   4  -> 10 ^  4
   5  -> 10 ^  5
   6  -> 10 ^  6
   7  -> 10 ^  7
   8  -> 10 ^  8
   9  -> 10 ^  9
   10 -> 10 ^ 10
   11 -> 10 ^ 11
   12 -> 10 ^ 12
   13 -> 10 ^ 13
   14 -> 10 ^ 14
   15 -> 10 ^ 15
   16 -> 10 ^ 16
   17 -> 10 ^ 17
   _  -> 10 ^ i
