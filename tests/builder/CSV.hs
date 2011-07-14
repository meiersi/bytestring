{-# LANGUAGE PackageImports #-}
-- |
-- Copyright   : (c) 2010-2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Running example for documentation of Data.ByteString.Lazy.Builder
--
module CSV where

import qualified "new-bytestring" Data.ByteString                     as S
import qualified "new-bytestring" Data.ByteString.Lazy                as L
import           "new-bytestring" Data.ByteString.Lazy.Builder        
import           "new-bytestring" Data.ByteString.Lazy.Builder.Extras 

import qualified Data.DList                                           as D


import Codec.Bounded.Encoding (Encoding, encodeIf, encode2, (#.), utf8)

import Criterion.Main
import Data.Monoid
import Data.Foldable (foldMap)
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


renderString :: String -> Builder
renderString cs = stringUtf8 $ "\"" ++ concatMap escape cs ++ "\"" 
  where
    escape '\\' = "\\"
    escape '\"' = "\\\""
    escape c    = return c

renderCell :: Cell -> Builder
renderCell (StringC cs) = renderString cs
renderCell (IntC i)     = stringUtf8 $ show i
           
renderRow :: Row -> Builder
renderRow  = mconcat . intersperse (charUtf8 ',') . map renderCell

renderTable :: Table -> Builder
renderTable rs = mconcat [renderRow r <> charUtf8 '\n' | r <- rs]

encodeUtf8CSV :: Table -> L.ByteString
encodeUtf8CSV = toLazyByteString . renderTable

strings :: [String]
strings =  ["hello", "\"1\"", "λ-wörld"]

table :: Table
table = [map StringC strings, map IntC [-3..3]]

n :: Int
n = 1000

maxiTable :: Table
maxiTable = take n $ cycle table

maxiStrings :: [String]
maxiStrings = take n $ cycle strings


------------------------------------------------------------------------------
-- Difference List based
------------------------------------------------------------------------------

type DString = D.DList Char

-- improved
renderStringD :: String -> DString
renderStringD cs = return '"' <> mconcat (map escape cs) <> return '"'
  where
    escape '\\' = D.DL $ \k -> '\\':'\\':k
    escape '\"' = D.DL $ \k -> '\\':'"' :k
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
encodeUtf8CSV_Dlist = toLazyByteString . encodeListWith utf8 . encodeCSV
      
------------------------------------------------------------------------------
-- Version 1
------------------------------------------------------------------------------

-- improved
renderString1 :: String -> Builder
renderString1 cs = charUtf8 '"' <> foldMap escape cs <> charUtf8 '"'
  where
    escape '\\' = charUtf8 '\\' <> charUtf8 '\\'
    escape '\"' = charUtf8 '\\' <> charUtf8 '\"'
    escape c    = charUtf8 c

-- carry over definitions
renderCell1 :: Cell -> Builder
renderCell1 (StringC cs) = renderString1 cs
renderCell1 (IntC i)     = stringUtf8 $ show i
           
renderRow1 :: Row -> Builder
renderRow1  = mconcat . intersperse (charUtf8 ',') . map renderCell1

renderTable1 :: Table -> Builder
renderTable1 rs = mconcat [renderRow1 r <> charUtf8 '\n' | r <- rs]

encodeUtf8CSV1 :: Table -> L.ByteString
encodeUtf8CSV1 = toLazyByteString . renderTable1


------------------------------------------------------------------------------
-- Version 2
------------------------------------------------------------------------------


renderRow2 :: Row -> Builder
renderRow2 []     = mempty
renderRow2 (c:cs) = 
    renderCell1 c <> mconcat [ charUtf8 ',' <> renderCell1 c' | c' <- cs ]

renderTable2 :: Table -> Builder
renderTable2 rs = mconcat [renderRow2 r <> charUtf8 '\n' | r <- rs]

encodeUtf8CSV2 :: Table -> L.ByteString
encodeUtf8CSV2 = toLazyByteString . renderTable2


------------------------------------------------------------------------------
-- Version 3
------------------------------------------------------------------------------

-- improved
renderString3 :: String -> Builder
renderString3 cs = 
    charUtf8 '"' <> encodeListWith escapedUtf8 cs <> charUtf8 '"'
  where
    escapedUtf8 :: Encoding Char
    escapedUtf8 = 
      encodeIf (== '\\') (encode2 utf8 utf8 #. const ('\\', '\\')) $
      encodeIf (== '\"') (encode2 utf8 utf8 #. const ('\\', '\"')) $
      utf8

-- carry over definitions
renderCell3 :: Cell -> Builder
renderCell3 (StringC cs) = renderString3 cs
renderCell3 (IntC i)     = stringUtf8 $ show i
           
renderRow3 :: Row -> Builder
renderRow3 []     = mempty
renderRow3 (c:cs) = 
    renderCell3 c <> mconcat [ charUtf8 ',' <> renderCell3 c' | c' <- cs ]

renderTable3 :: Table -> Builder
renderTable3 rs = mconcat [renderRow3 r <> charUtf8 '\n' | r <- rs]

encodeUtf8CSV3 :: Table -> L.ByteString
encodeUtf8CSV3 = toLazyByteString . renderTable3

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
    [ bench "renderString maxiStrings (original)" $ 
        whnf (L.length . toLazyByteString . foldMap renderString1) maxiStrings
    , bench "renderString maxiStrings (Encoding)" $ 
        whnf (L.length . toLazyByteString . foldMap renderString3) maxiStrings
    , bench "encodeCSV maxiTable (DList)" $ 
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



