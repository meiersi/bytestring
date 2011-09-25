module Main where

import           Test.Framework (defaultMain, testGroup)

import qualified Data.ByteString.Lazy.Builder.BasicEncoding.Tests


main :: IO ()
main = defaultMain tests 

tests =
  [ testGroup "Data.ByteString.Lazy.Builder.BasicEncoding.Tests"
       Data.ByteString.Lazy.Builder.BasicEncoding.Tests.tests
  ]

