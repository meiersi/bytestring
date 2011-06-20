-- |
-- Created: 2011 06 14
-- Author:  Simon Meier <iridcode@gmail.com>
--
-- Testcase for rewriting rules that involve 'foldr'. The testcase is
-- motivated by the Builder being implemented for the bytestring library.
-- There, we would like to exploit equations like
--
--   mconcat . map (fromWrite w) = fromWriteList w
--
-- in order to gain additional (2x - 10x) more efficiency. However, I could not
-- formulate a rewriting rule that matches the appropriate intermediate form of
-- 'mconcat . map (fromWrite w)'. The following definitions illustrate the
-- problem with the same names but simpler types.
--
-- See
-- https://github.com/meiersi/bytestring/blob/master/Data/ByteString/Builder/Write.hs 
-- for more context on Writes and Builders.
module Test_RewriteFoldr where

import Data.Monoid
import Data.List
import qualified Data.Foldable as F

import GHC.Base


newtype Builder = Builder String
  deriving( Show )

{-# INLINE [1] append #-}
append :: Builder -> Builder -> Builder
append x            (Builder "") = x
append (Builder "") y            = y
append (Builder x ) (Builder y)  = Builder (x ++ " | " ++ y)

{-# INLINE [1] empty #-}
empty  :: Builder
empty = Builder ""

instance Monoid Builder where
  {-# INLINE mempty #-}
  mempty = empty
  {-# INLINE mappend #-}
  mappend = append
  {-# INLINE mconcat #-}
  mconcat = foldr mappend mempty

-- | Writes are primitive serialization functions.
data Write a = Write (a -> String)

(#.) :: Write a -> (b -> a) -> Write b
(Write w) #. f = Write (w . f)

-- | An dummy implementation of a write for strings.
writeString :: Write String
writeString = Write id

-- | Writes can be used to construct builders for singleton arguments...
{-# INLINE [1] fromWrite #-}
fromWrite :: Write a -> a -> Builder
fromWrite (Write f) x = Builder ("write: " ++ f x)

-- | ...as well as for lists. Here, we mark what 'fromWriteXXX' function was
-- used to detect if a rewriting rule was applied or not.
{-# INLINE fromWriteList #-}
fromWriteList :: Write a -> [a] -> Builder
fromWriteList (Write f) xs = 
    Builder (concat $ intersperse " | " [ "writeList: " ++ f x | x <- xs])

-- In the actual implementation, 'fromWriteList w' is significantly more
-- efficient than 'mconcat . map (fromWrite w)'. It moves several variables out
-- of the inner loop and probably also helps the compilers strictness analyzer.
-- 
-- This rule should convert the intermediate version of 'mconcat . map
-- (fromWrite w)' and as many variants as possible to the efficient
-- 'fromWriteList' version.
{-# RULES
"foldr/fromWrite" forall w.
    foldr (\x -> append (fromWrite w x)) empty = fromWriteList w 

"foldr/fromWrite/#." forall w f b.
    foldr (\x -> append (fromWrite w (f x))) b
  = foldr (\x -> append (fromWrite (w #. f) x)) b

"foldr/fromWrite/mapFB" forall w.
    foldr (mapFB append (fromWrite w)) empty = fromWriteList w 

"foldr/fromWrite/mapFB/#." forall w f b.
    foldr (mapFB append (\x -> fromWrite w (f x))) b
  = foldr (mapFB append (fromWrite (w #. f))) b
 #-}


-- All of the following expressions should yield the same result when compiling
-- with -O2. However, only the first two do, while the 'fails' expression gets
-- probably converted to some intermediate representation that the rule doesn't
-- match on.
goal :: [String] -> Builder
goal = fromWriteList writeString

works :: [String] -> Builder
works = foldr (\x b -> append (fromWrite writeString x) b) empty

works2 :: [String] -> Builder
works2 =  mconcat . map (fromWrite writeString)

works3 :: [String] -> Builder
works3 =  mconcat . map (fromWrite writeString) . map reverse

-- Late "application" of missing INLINE pragma on default implementation of
-- 'foldMap' from 'Foldable'.
{-# RULES "Specialize foldMap_Builder" F.foldMap = foldMap_Builder #-}
{-# INLINE foldMap_Builder #-}
foldMap_Builder :: F.Foldable f => (a -> Builder) -> f a -> Builder
foldMap_Builder f = F.foldr (mappend . f) mempty


works4 :: [String] -> Builder
works4 = foldMap_Builder $ fromWrite writeString

works5 :: [String] -> Builder
works5 = F.foldMap $ fromWrite writeString

-- foldr (mappend . fromWrite writeString) mempty = 
-- foldr (append . fromWrite writeString) empty = 
-- foldr (\x -> append (fromWrite writeString x)) empty = 


main :: IO ()
main = do
    putStrLn "The following three results should all use 'fromWriteList':"
    mapM_ (print . ($ input)) [goal, works, works2, works3, works4, works5]
  where
    input :: [String]
    input = ["hello", "world", "!"]


