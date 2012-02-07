module Main where

--import           Test.Framework (defaultMain, Test, testGroup)

import qualified Data.ByteString.Lazy.Builder.BasicEncoding.Tests
import qualified Data.ByteString.Lazy.Builder.Tests
import           TestFramework


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Builder"
       Data.ByteString.Lazy.Builder.Tests.tests

  , testGroup "BasicEncoding"
       Data.ByteString.Lazy.Builder.BasicEncoding.Tests.tests
  ]

