-- |
-- Copyright   : (c) 2010-2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Running example for documentation of Data.ByteString.Builder
--
module CSV where

import qualified Data.ByteString.Lazy              as L
import Data.ByteString.Builder          
import Data.ByteString.Builder.Write    


import           System.IO.Write       (Write, writeIf, write2, (#.))
import qualified System.IO.Write as W  (utf8)

import Criterion.Main
import Data.Monoid
import Data.List
import Data.Int

------------------------------------------------------------------------------
-- Simplified CSV
------------------------------------------------------------------------------

infixr 4 <>

(<>) :: Monoid m => m -> m -> m
(<>) = mappend


data Cell = StringC String
          | IntC Int
          deriving( Eq, Ord, Show )

type Row   = [Cell]
type Table = [Row]


renderString :: String -> Builder
renderString cs = utf8 $ "\"" ++ concatMap escape cs ++ "\"" 
  where
    escape '\\' = "\\"
    escape '\"' = "\\\""
    escape c    = return c

renderCell :: Cell -> Builder
renderCell (StringC cs) = renderString cs
renderCell (IntC i)     = utf8 $ show i
           
renderRow :: Row -> Builder
renderRow  = mconcat . intersperse (utf8 ',') . map renderCell

renderTable :: Table -> Builder
renderTable rs = mconcat [renderRow r <> utf8 '\n' | r <- rs]

encodeUtf8CSV :: Table -> L.ByteString
encodeUtf8CSV = toLazyByteString . renderTable

strings :: [String]
strings =  ["hello", "\"1\"", "λ-wörld"]

table :: Table
table = [map StringC strings, map IntC [-3..3]]

maxiTable :: Table
maxiTable = take 1000 $ cycle table
      
------------------------------------------------------------------------------
-- Version 1
------------------------------------------------------------------------------

-- improved
renderString1 :: String -> Builder
renderString1 cs = utf8 '"' <> foldMap escape cs <> utf8 '"'
  where
    escape '\\' = utf8 '\\' <> utf8 '\\'
    escape '\"' = utf8 '\\' <> utf8 '\"'
    escape c    = utf8 c

-- carry over definitions
renderCell1 :: Cell -> Builder
renderCell1 (StringC cs) = renderString1 cs
renderCell1 (IntC i)     = utf8 $ show i
           
renderRow1 :: Row -> Builder
renderRow1  = mconcat . intersperse (utf8 ',') . map renderCell1

renderTable1 :: Table -> Builder
renderTable1 rs = mconcat [renderRow1 r <> utf8 '\n' | r <- rs]

encodeUtf8CSV1 :: Table -> L.ByteString
encodeUtf8CSV1 = toLazyByteString . renderTable1


------------------------------------------------------------------------------
-- Version 2
------------------------------------------------------------------------------


renderRow2 :: Row -> Builder
renderRow2 []     = mempty
renderRow2 (c:cs) = 
    renderCell1 c <> mconcat [ utf8 ',' <> renderCell1 c' | c' <- cs ]
    -- renderCell1 c <> foldr (\c' b -> utf8 ',' <> renderCell1 c' <> b) mempty cs

renderTable2 :: Table -> Builder
renderTable2 = 
    -- mconcat [renderRow2 r <> utf8 '\n' | r <- rs]
    foldr (\r b -> renderRow2 r <> utf8 '\n' <> b) mempty

encodeUtf8CSV2 :: Table -> L.ByteString
encodeUtf8CSV2 = toLazyByteString . renderTable2


------------------------------------------------------------------------------
-- Version 3
------------------------------------------------------------------------------

-- improved
renderString3 :: String -> Builder
renderString3 cs = utf8 '"' <> fromWriteList writeEscaped cs <> utf8 '"'
  where
    writeEscaped :: Write Char
    writeEscaped = 
      writeIf (== '\\') (write2 W.utf8 W.utf8 #. const ('\\', '\\')) $
      writeIf (== '\"') (write2 W.utf8 W.utf8 #. const ('\\', '\"')) $
      W.utf8

-- carry over definitions
renderCell3 :: Cell -> Builder
renderCell3 (StringC cs) = renderString3 cs
renderCell3 (IntC i)     = utf8 $ show i
           
renderRow3 :: Row -> Builder
renderRow3 []     = mempty
renderRow3 (c:cs) = 
    renderCell3 c <> mconcat [ utf8 ',' <> renderCell3 c' | c' <- cs ]
    -- renderCell3 c <> mconcat [ utf8 ',' <> renderCell3 c' | c' <- cs ]

renderTable3 :: Table -> Builder
renderTable3 = 
    -- mconcat [renderRow3 r <> utf8 '\n' | r <- rs]
    foldr (\r b -> renderRow3 r <> utf8 '\n' <> b) mempty

encodeUtf8CSV3 :: Table -> L.ByteString
encodeUtf8CSV3 = toLazyByteString . renderTable3

------------------------------------------------------------------------------
-- Benchmark Data
------------------------------------------------------------------------------

n :: Int
n = 10000

{-# NOINLINE tableInput #-}
tableInput :: [(String, Table)]
tableInput = [ (show n, take n $ cycle table) ]

{-# NOINLINE stringInput #-}
stringInput :: [(String, [String])]
stringInput = [ (show n, take n $ cycle strings) ]


------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

original, version1, version2, version3 
  :: ([Char], Builder -> Int64, Builder -> L.ByteString)
(original, version1, version2, version3) =
    (f "original", f "version1", f "version2", f "version3")
  where
    f x = (x, L.length . toLazyByteString, toLazyByteString)

benchmarks :: [Benchmark]
testResults :: [String]
(benchmarks, testResults) = unzip $ 
    [ comparison "strings" $
        [ impl original (mconcat . map renderString ) stringInput
        , impl version1 (mconcat . map renderString1) stringInput
        , impl version3 (mconcat . map renderString3) stringInput
        ]
    , comparison "table" $
        [ impl original renderTable  tableInput
        , impl version1 renderTable1 tableInput
        , impl version2 renderTable2 tableInput
        , impl version3 renderTable3 tableInput
        ]
    ]
  where
    comparison :: Eq d => String -> [([Benchmark], [d])] -> (Benchmark, String)
    comparison name xs =
        ( bgroup name $ concat bs
        , (if success then "OK" else "FAIL" ) ++ ". " ++ name
        )
      where
        (bs, ts) = unzip xs
        success  = and $ zipWith (==) ts $ tail ts

    impl :: (String, b -> c, b -> d) -> (a -> b) -> [(String, a)] 
          -> ([Benchmark], [d])
    impl (name, fBench, fTest) f inputs = 
      ( do (inputName, x) <- inputs
           return $ bgroup name [bench inputName $ whnf (fBench . f) x]
      , map (fTest . f . snd) inputs
      )


main :: IO ()
main = do
  putStrLn ""
  putStrLn "Comparing results of different implementations:"
  putStrLn $ unlines $ map ("  " ++) testResults
  putStrLn "Benchmarking:"
  defaultMain benchmarks

