{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Testing composition of 'Builders'.

module Data.ByteString.Lazy.Builder.Tests (tests) where

import           Data.Monoid
import           Data.Foldable (asum, foldMap)

import           Control.Applicative
import           Control.Monad
import           Control.Exception (evaluate)

import           Foreign

import qualified Data.DList      as D

import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L

import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Builder.Extras
import           Data.ByteString.Lazy.Builder.ASCII
import qualified Data.ByteString.Lazy.Builder.Internal           as BI
import qualified Data.ByteString.Lazy.Builder.BasicEncoding      as E
import           Data.ByteString.Lazy.Builder.BasicEncoding.TestUtils

import           System.IO
import           System.Directory

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
                   ( Arbitrary(..), oneof, choose, listOf, elements )
import           Test.QuickCheck.Property (morallyDubiousIOProperty)
import           Test.QuickCheck.Property (printTestCase)


tests :: [Test]
tests = [testBuilderRecipe, testHandlePutBuilder]

------------------------------------------------------------------------------
-- Testing chunk-handling.
------------------------------------------------------------------------------

testBuilderRecipe :: Test
testBuilderRecipe = 
    testProperty "builder recipe" $ testRecipe <$> arbitrary
  where
    testRecipe r = 
        printTestCase msg $ x1 == x2
      where
        x1 = renderRecipe r
        x2 = buildRecipe r
        toString = show -- map (chr . fromIntegral)
        msg = unlines 
          [ "recipe: " ++ show r
          , "render: " ++ toString x1 
          , "build : " ++ toString x2
          , "diff  : " ++ show (dropWhile (uncurry (==)) $ zip x1 x2)
          ]

testHandlePutBuilder :: Test
testHandlePutBuilder = 
    testProperty "builder recipe" testRecipe 
  where
    testRecipe :: (String, String, String, [Action]) -> Bool
    testRecipe args@(before, between, after, actions) = unsafePerformIO $ do
        tempDir <- getTemporaryDirectory
        (tempFile, tempH) <- openTempFile tempDir "TestBuilder"
        -- switch to UTF-8 encoding
        hSetEncoding tempH utf8
        -- output recipe with intermediate direct writing to handle
        let b = foldMap buildAction actions
        hPutStr tempH before
        hPutBuilder tempH b
        hPutStr tempH between
        hPutBuilder tempH b
        hPutStr tempH after
        hClose tempH
        -- read file
        lbs <- L.readFile tempFile
        evaluate (L.length $ lbs)
        removeFile tempFile
        -- compare to pure builder implementation
        let lbsRef = toLazyByteString $ mconcat 
              [stringUtf8 before, b, stringUtf8 between, b, stringUtf8 after]
        -- report
        let msg = unlines 
              [ "task:     " ++ show args
              , "via file: " ++ show lbs
              , "direct :  " ++ show lbsRef
              -- , "diff  : " ++ show (dropWhile (uncurry (==)) $ zip x1 x2)
              ]
            success = lbs == lbsRef
        unless success (error msg)
        return success


-- Recipes with which to test the builder functions
---------------------------------------------------

data Mode = 
       Threshold Int
     | Insert 
     | Copy
     | Hex
     deriving( Eq, Ord, Show )

data Action = 
       SBS Mode S.ByteString
     | LBS Mode L.ByteString
     | W8  Word8
     | W8S [Word8]
     | IDec Integer
     | FDec Float
     | DDec Double
     | Flush
     | EnsureFree Word
     deriving( Eq, Ord, Show )

data Strategy = Safe | Untrimmed
     deriving( Eq, Ord, Show )

data Recipe = Recipe Strategy Int Int L.ByteString [Action]
     deriving( Eq, Ord, Show )

renderRecipe :: Recipe -> [Word8]
renderRecipe (Recipe _ _ _ cont as) =
    D.toList $ foldMap renderAction as `mappend` renderLBS cont
  where
    renderAction (SBS Hex bs)   = foldMap hexWord8 $ S.unpack bs
    renderAction (SBS _ bs)     = D.fromList $ S.unpack bs
    renderAction (LBS Hex lbs)  = foldMap hexWord8 $ L.unpack lbs
    renderAction (LBS _ lbs)    = renderLBS lbs
    renderAction (W8 w)         = return w
    renderAction (W8S ws)       = D.fromList ws
    renderAction Flush          = mempty
    renderAction (EnsureFree _) = mempty
    renderAction (IDec i)       = D.fromList $ encodeASCII $ show i
    renderAction (FDec f)       = D.fromList $ encodeASCII $ show f
    renderAction (DDec d)       = D.fromList $ encodeASCII $ show d

    renderLBS = D.fromList . L.unpack
    hexWord8  = D.fromList . wordHexFixed_list

buildAction :: Action -> Builder
buildAction (SBS Hex bs)            = byteStringHexFixed bs
buildAction (SBS Copy bs)           = byteStringCopy bs
buildAction (SBS Insert bs)         = byteStringInsert bs
buildAction (SBS (Threshold i) bs)  = byteStringThreshold i bs
buildAction (LBS Hex lbs)           = lazyByteStringHexFixed lbs
buildAction (LBS Copy lbs)          = lazyByteStringCopy lbs
buildAction (LBS Insert lbs)        = lazyByteStringInsert lbs
buildAction (LBS (Threshold i) lbs) = lazyByteStringThreshold i lbs
buildAction (W8 w)                  = word8 w
buildAction (W8S ws)                = E.encodeListWithF E.word8 ws
buildAction (IDec i)                = integerDec i
buildAction (FDec f)                = floatDec f
buildAction (DDec d)                = doubleDec d
buildAction Flush                   = flush
buildAction (EnsureFree minFree)    = ensureFree $ fromIntegral minFree

buildRecipe :: Recipe -> [Word8]
buildRecipe (Recipe how firstSize otherSize cont as) =
    L.unpack $ toLBS $ foldMap buildAction as
  where
    toLBS = toLazyByteStringWith (strategy how firstSize otherSize) cont
      where
        strategy Safe      = safeStrategy
        strategy Untrimmed = untrimmedStrategy



-- 'Arbitary' instances
-----------------------

instance Arbitrary L.ByteString where
    arbitrary = L.fromChunks <$> listOf arbitrary
    shrink lbs
      | L.null lbs = []
      | otherwise = pure $ L.take (L.length lbs `div` 2) lbs

instance Arbitrary S.ByteString where
    arbitrary =
        trim S.drop =<< trim S.take =<< S.pack <$> listOf arbitrary
      where
        trim f bs = oneof [pure bs, f <$> choose (0, S.length bs) <*> pure bs]

    shrink bs
      | S.null bs = []
      | otherwise = pure $ S.take (S.length bs `div` 2) bs

instance Arbitrary Mode where
    arbitrary = oneof [Threshold <$> arbitrary, pure Insert, pure Copy, pure Hex]

    shrink (Threshold i) = Threshold <$> shrink i
    shrink _             = []

instance Arbitrary Action where
    arbitrary = oneof
      [ SBS <$> arbitrary <*> arbitrary
      , LBS <$> arbitrary <*> arbitrary
      , W8  <$> arbitrary
      , W8S <$> listOf arbitrary
      , pure Flush
      , (EnsureFree . (`mod` 0xffff)) <$> arbitrary 
        -- never request more than 64kb, free space
      , IDec <$> arbitrary
      , FDec <$> arbitrary
      , DDec <$> arbitrary
      ]
      where

    shrink (SBS m bs) = 
      (SBS <$> shrink m <*> pure bs) <|>
      (SBS <$> pure m   <*> shrink bs)
    shrink (LBS m lbs) = 
      (LBS <$> shrink m <*> pure lbs) <|>
      (LBS <$> pure m   <*> shrink lbs)
    shrink (W8 w)   = W8 <$> shrink w
    shrink (W8S ws) = W8S <$> shrink ws
    shrink Flush    = []
    shrink (IDec i) = IDec <$> shrink i
    shrink (FDec f) = FDec <$> shrink f
    shrink (DDec d) = DDec <$> shrink d

instance Arbitrary Strategy where
    arbitrary = elements [Safe, Untrimmed]
    shrink _  = []

instance Arbitrary Recipe where
    arbitrary = 
        Recipe <$> arbitrary 
               <*> ((`mod` 33333) <$> arbitrary)  -- bound max chunk-sizes
               <*> ((`mod` 33337) <$> arbitrary)
               <*> arbitrary 
               <*> listOf arbitrary

    -- shrinking the actions first is desirable
    shrink (Recipe a b c d e) = asum
      [ (\x -> Recipe a b c d x) <$> shrink e
      , (\x -> Recipe a b c x e) <$> shrink d
      , (\x -> Recipe a b x d e) <$> shrink c
      , (\x -> Recipe a x c d e) <$> shrink b
      , (\x -> Recipe x b c d e) <$> shrink a
      ] 

------------------------------------------------------------------------------
-- Testing the Driver <-> Builder protocol
------------------------------------------------------------------------------

-- | Ensure that there are at least 'n' free bytes for the following 'Builder'.
{-# INLINE ensureFree #-}
ensureFree :: Int -> Builder
ensureFree minFree =
    BI.builder step
  where
    step k br@(BI.BufferRange op ope)
      | ope `minusPtr` op < minFree = return $ BI.bufferFull minFree op k
      | otherwise                   = k br
      where
        next br'@(BI.BufferRange op' ope')
          |  free < minFree = 
              error $ "ensureFree: requested " ++ show minFree ++ " bytes, " ++
                      "but got only " ++ show free ++ " bytes"
          | otherwise = k br'
          where
            free = ope' `minusPtr` op'
