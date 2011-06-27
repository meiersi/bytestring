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

import qualified Data.ByteString               as S
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Builder.Write as B
import qualified Data.DList                    as D


import           System.IO.Write       (Write, writeIf, write2, (#.))
import qualified System.IO.Write as W  (utf8)

import Criterion.Main
import Data.Monoid
import Data.List (intersperse)

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


renderString :: String -> B.Builder
renderString cs = B.utf8 $ "\"" ++ concatMap escape cs ++ "\"" 
  where
    escape '\\' = "\\"
    escape '\"' = "\\\""
    escape c    = return c

renderCell :: Cell -> B.Builder
renderCell (StringC cs) = renderString cs
renderCell (IntC i)     = B.utf8 $ show i
           
renderRow :: Row -> B.Builder
renderRow  = mconcat . intersperse (B.utf8 ',') . map renderCell

renderTable :: Table -> B.Builder
renderTable rs = mconcat [renderRow r <> B.utf8 '\n' | r <- rs]

encodeUtf8CSV :: Table -> L.ByteString
encodeUtf8CSV = B.toLazyByteString . renderTable

strings :: [String]
strings =  ["hello", "\"1\"", "λ-wörld"]

table :: Table
table = [map StringC strings, map IntC [-3..3]]

n :: Int
n = 1000

maxiTable :: Table
maxiTable = take n $ cycle table

------------------------------------------------------------------------------
-- Difference List based
------------------------------------------------------------------------------

type DString = D.DList Char

-- improved
renderStringD :: String -> DString
renderStringD cs = return '"' <> mconcat (map escape cs) <> return '"'
  where
    escape '\\' = D.DL $ \k -> '\\':'\\':k
    escape '\"' = D.DL $ \k ->  '\\':'"':k
    escape c    = return c

-- carry over definitions
renderCellD :: Cell -> DString
renderCellD (StringC cs) = renderStringD cs
renderCellD (IntC i)     = D.fromList $ show i

renderRowD :: Row -> DString
renderRowD []     = mempty
renderRowD (c:cs) = 
    renderCellD c <> mconcat [ return ',' <> renderCellD c' | c' <- cs ]

renderTableD :: Table -> DString
renderTableD rs = mconcat [renderRowD r <> return '\n' | r <- rs]

encodeCSV :: Table -> String
encodeCSV = D.toList . renderTableD

encodeUtf8CSV_Dlist :: Table -> L.ByteString
encodeUtf8CSV_Dlist = B.toLazyByteString . B.fromWriteList W.utf8 . encodeCSV
      
------------------------------------------------------------------------------
-- Version 1
------------------------------------------------------------------------------

-- improved
renderString1 :: String -> B.Builder
renderString1 cs = B.utf8 '"' <> B.foldMap escape cs <> B.utf8 '"'
  where
    escape '\\' = B.utf8 '\\' <> B.utf8 '\\'
    escape '\"' = B.utf8 '\\' <> B.utf8 '\"'
    escape c    = B.utf8 c

-- carry over definitions
renderCell1 :: Cell -> B.Builder
renderCell1 (StringC cs) = renderString1 cs
renderCell1 (IntC i)     = B.utf8 $ show i
           
renderRow1 :: Row -> B.Builder
renderRow1  = mconcat . intersperse (B.utf8 ',') . map renderCell1

renderTable1 :: Table -> B.Builder
renderTable1 rs = mconcat [renderRow1 r <> B.utf8 '\n' | r <- rs]

encodeUtf8CSV1 :: Table -> L.ByteString
encodeUtf8CSV1 = B.toLazyByteString . renderTable1


------------------------------------------------------------------------------
-- Version 2
------------------------------------------------------------------------------


renderRow2 :: Row -> B.Builder
renderRow2 []     = mempty
renderRow2 (c:cs) = 
    renderCell1 c <> mconcat [ B.utf8 ',' <> renderCell1 c' | c' <- cs ]

renderTable2 :: Table -> B.Builder
renderTable2 rs = mconcat [renderRow2 r <> B.utf8 '\n' | r <- rs]

encodeUtf8CSV2 :: Table -> L.ByteString
encodeUtf8CSV2 = B.toLazyByteString . renderTable2


------------------------------------------------------------------------------
-- Version 3
------------------------------------------------------------------------------

-- improved
renderString3 :: String -> B.Builder
renderString3 cs = B.utf8 '"' <> B.fromWriteList writeEscaped cs <> B.utf8 '"'
  where
    writeEscaped :: Write Char
    writeEscaped = 
      writeIf (== '\\') (write2 W.utf8 W.utf8 #. const ('\\', '\\')) $
      writeIf (== '\"') (write2 W.utf8 W.utf8 #. const ('\\', '\"')) $
      W.utf8

-- carry over definitions
renderCell3 :: Cell -> B.Builder
renderCell3 (StringC cs) = renderString3 cs
renderCell3 (IntC i)     = B.utf8 $ show i
           
renderRow3 :: Row -> B.Builder
renderRow3 []     = mempty
renderRow3 (c:cs) = 
    renderCell3 c <> mconcat [ B.utf8 ',' <> renderCell3 c' | c' <- cs ]

renderTable3 :: Table -> B.Builder
renderTable3 rs = mconcat [renderRow3 r <> B.utf8 '\n' | r <- rs]

encodeUtf8CSV3 :: Table -> L.ByteString
encodeUtf8CSV3 = B.toLazyByteString . renderTable3

------------------------------------------------------------------------------
-- Benchmark Data
------------------------------------------------------------------------------

{-# NOINLINE tableInput #-}
tableInput :: [(String, Table)]
tableInput = 
  -- [ (show $ length table, table) 
  [ (show n, take n $ cycle table) ]

{-# NOINLINE stringInput #-}
stringInput :: [(String, [String])]
stringInput = [ (show n, take n $ cycle strings) ]


------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

original, version1, version2, version3 
  :: ([Char], B.Builder -> Int64, B.Builder -> L.ByteString)
(original, version1, version2, version3) =
    (f "original", f "version1", f "version2", f "version3")
  where
    f x = (x, L.length . B.toLazyByteString, B.toLazyByteString)

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

  {-

main :: IO ()
main = do
  print $ encodeUtf8CSV table
  print $ map S.length $ L.toChunks $ encodeUtf8CSV maxiTable
  -}
  {-
  putStrLn ""
  putStrLn "Comparing results of different implementations:"
  putStrLn $ unlines $ map ("  " ++) testResults
  putStrLn "Benchmarking:"
  defaultMain benchmarks
  -}
main :: IO ()
main = do
  print $ encodeUtf8CSV table
  print $ map S.length $ L.toChunks $ encodeUtf8CSV maxiTable
  print $ L.length $ encodeUtf8CSV maxiTable
  print ""
  defaultMain
    [ bench "encodeCSV maxiTable (DList)" $ 
        whnf (length . encodeCSV) maxiTable
    , bench "encodeCSV maxiTable (DList + Utf8 encode)" $ 
        whnf (L.length . encodeUtf8CSV_Dlist) maxiTable
    , bench "encodeUtf8CSV maxiTable (original)" $ 
        whnf (L.length . encodeUtf8CSV) maxiTable
    , bench "encodeUtf8CSV maxiTable (version 1)" $ 
        whnf (L.length . encodeUtf8CSV1) maxiTable
    , bench "encodeUtf8CSV maxiTable (version 2)" $ 
        whnf (L.length . encodeUtf8CSV2) maxiTable
    , bench "encodeUtf8CSV maxiTable (version 3)" $ 
        whnf (L.length . encodeUtf8CSV3) maxiTable
    ]



