module Main where

import           Test.Framework (defaultMain, Test, testGroup)

import qualified Data.ByteString.Lazy.Builder.BasicEncoding.Tests


main :: IO ()
main = defaultMain tests 

tests :: [Test]
tests =
  [ testGroup "Data.ByteString.Lazy.Builder.BasicEncoding.Tests"
       Data.ByteString.Lazy.Builder.BasicEncoding.Tests.tests
  ]

