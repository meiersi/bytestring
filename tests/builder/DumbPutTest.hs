{-# LANGUAGE PackageImports, OverloadedStrings #-}
import qualified "new-bytestring" Data.ByteString  as S
import "new-bytestring" Data.ByteString.Char8 ()

import Data.Monoid

import System.IO

import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Builder.Extras
import qualified Data.ByteString.Lazy.Builder.Utf8 as Utf8

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

main = do
    putStrLn "Start."
    h <- openFile "DumbPutTest-out.txt" WriteMode
    hSetBuffering h $ BlockBuffering (Just 32000)
    showBufferMode h
    hPutBuilder h $    Utf8.string "Hello simon! Does it wörk?" 
                    <> flush 
                    <> mconcat (replicate 10 (Utf8.string "really?\n"))
    hSetEncoding h utf8
    hPutStrLn h "Tésting thé interäctiön with 'hPutStrLn'"
    hPutBuilder h $    Utf8.string "Have binary: " 
                    <> int32LE 10
                    <> byteStringInsert "Harr! Char8 Encöding\n"
                    <> byteStringCopy (S.pack $ replicate 10 33)
                    <> Utf8.string "More stuff."
    showBufferMode h
    hClose h
    putStrLn "Done."
  where
    showBufferMode h = do
        mode <- hGetBuffering h
        putStrLn $ "Buffer mode: " ++ show mode

