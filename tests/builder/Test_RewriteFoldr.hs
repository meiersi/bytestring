-- |
-- Created: 2011 06 14
-- Author:  Simon Meier <iridcode@gmail.com>
--
-- Testcase for rewriting rules on Builders and Puts.
module Test_RewriteFoldr where

import Data.Monoid
import Data.List
import qualified Data.Foldable as F
import Control.Applicative hiding (empty)

import GHC.Base

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

-- | Observable append of non-empty 'String's.
obsAppend :: String -> String -> String
obsAppend x  [] = x
obsAppend [] y  = y
obsAppend x  y  = x ++ " | " ++ y

------------------------------------------------------------------------------
-- Builders
------------------------------------------------------------------------------

newtype Builder = Builder String
  deriving( Show )

{-# INLINE [1] append #-}
append :: Builder -> Builder -> Builder
append (Builder x ) (Builder y)  = Builder (obsAppend x y)

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

-- Late "application" of missing INLINE pragma on default implementation of
-- 'foldMap' from 'Foldable'.
{-# RULES "Specialize foldMap_Builder" F.foldMap = foldMap_Builder #-}
{-# INLINE foldMap_Builder #-}
foldMap_Builder :: F.Foldable f => (a -> Builder) -> f a -> Builder
foldMap_Builder f = F.foldr (mappend . f) mempty


------------------------------------------------------------------------------
-- Puts
------------------------------------------------------------------------------

data Put a = Put a String
  deriving( Show )

instance Functor Put where
  {-# INLINE fmap #-}
  fmap f (Put x cs) = Put (f x) cs

{-# INLINE [1] bind_Put #-}
bind_Put :: Put a -> (a -> Put b) -> Put b
bind_Put (Put x cs) f = 
    let Put x' cs' = f x in Put x' (cs `obsAppend` cs')

{-# INLINE [1] left_result_Put #-}
left_result_Put :: Put a -> Put b -> Put a
left_result_Put (Put a acs) (Put _ bcs) = Put a (acs `obsAppend` bcs)

{-# INLINE [1] right_result_Put #-}
right_result_Put :: Put a -> Put b -> Put b
right_result_Put (Put _ acs) (Put b bcs) = Put b (acs `obsAppend` bcs)

instance Applicative Put where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  Put f fcs <*> Put a acs = Put (f a) (fcs `obsAppend` acs)
  (<*) = left_result_Put
  (*>) = right_result_Put

instance Monad Put where
  {-# INLINE return #-}
  return x = Put x ""
  {-# INLINE (>>=) #-}
  (>>=) = bind_Put
  {-# INLINE (>>) #-}
  (>>) = right_result_Put

{-# INLINE putBuilder #-}
putBuilder :: Builder -> Put ()
putBuilder (Builder cs) = Put () cs

{-# INLINE fromPut #-}
fromPut :: Put () -> Builder
fromPut (Put _ cs) = Builder cs

{-# RULES
"Put/*>" forall m n.
      m `right_result_Put` n
    = putBuilder (fromPut m `append` fromPut n)

"Put/<*" forall m n.
      m `left_result_Put` n
    = putBuilder (fromPut m `append` fromPut n)
 #-}

------------------------------------------------------------------------------
-- Encodings
------------------------------------------------------------------------------

-- | Encodings are primitive serialization functions.
data Encoding a = Encoding (a -> String)

(#.) :: Encoding a -> (b -> a) -> Encoding b
(Encoding w) #. f = Encoding (w . f)

encodePair :: Encoding a -> Encoding b -> Encoding (a, b)
encodePair (Encoding w1) (Encoding w2) = Encoding $ \(x,y) -> w1 x ++ " .2. " ++ w2 y

-- | An dummy implementation of a write for strings.
writeString :: Encoding String
writeString = Encoding id

fromString :: String -> Builder
fromString = encodeWith writeString

putString :: String -> Put ()
putString = putBuilder . fromString


-- | Encodings can be used to construct builders for singleton arguments...
{-# INLINE [1] encodeWith #-}
encodeWith :: Encoding a -> a -> Builder
encodeWith (Encoding f) x = Builder ("write: " ++ f x)

{-# RULES "append/encodeWith" forall w1 w2 x1 x2.
       append (encodeWith w1 x1) (encodeWith w2 x2) 
     = encodeWith (encodePair w1 w2) (x1, x2)
  #-}

-- | ...as well as for lists. Here, we mark what 'encodeWithXXX' function was
-- used to detect if a rewriting rule was applied or not.
{-# INLINE encodeListWith #-}
encodeListWith :: Encoding a -> [a] -> Builder
encodeListWith (Encoding f) xs = 
    Builder (concat $ intersperse " | " [ "writeList: " ++ f x | x <- xs])

-- In the actual implementation, 'encodeListWith w' is significantly more
-- efficient than 'mconcat . map (encodeWith w)'. It moves several variables out
-- of the inner loop and probably also helps the compilers strictness analyzer.
-- 
-- This rule should convert the intermediate version of 'mconcat . map
-- (encodeWith w)' and as many variants as possible to the efficient
-- 'encodeListWith' version.
{-# RULES
"foldr/encodeWith" forall w.
    foldr (\x -> append (encodeWith w x)) empty 
  = encodeListWith w 

"foldr/encodeWith/#." forall w f b.
    foldr (\x -> append (encodeWith w (f x))) b
  = foldr (\x -> append (encodeWith (w #. f) x)) b

"foldr/encodeWith/mapFB" forall w.
    foldr (mapFB append (encodeWith w)) empty = encodeListWith w 

"foldr/encodeWith/mapFB/#." forall w f b.
    foldr (mapFB append (\x -> encodeWith w (f x))) b
  = foldr (mapFB append (encodeWith (w #. f))) b
 #-}


------------------------------------------------------------------------------
-- Test cases
------------------------------------------------------------------------------

goal :: [String] -> Builder
goal = encodeListWith writeString

works :: [String] -> Builder
works = foldr (\x b -> append (encodeWith writeString x) b) empty

works2 :: [String] -> Builder
works2 =  mconcat . map (encodeWith writeString)

works3 :: [String] -> Builder
works3 =  mconcat . map (encodeWith writeString) . map reverse

works4 :: [String] -> Builder
works4 = foldMap_Builder $ encodeWith writeString

works5 :: [String] -> Builder
works5 = F.foldMap $ encodeWith writeString

-- Is not optimized as well as the other cases: interchangeability of mapM_ and
-- putBuilder is to hard for the compiler.
test6 :: [String] -> Builder
test6 = fromPut . mapM_ (putBuilder . encodeWith writeString)

test7 :: [String] -> Builder
test7 = F.foldMap (\x -> fromString x `mappend` fromString (tail x))

-- This case is rather nice: the triple put's are fused together and would
-- share a single bound-check in the actual implementation.
test8 :: [String] -> Builder
test8 (x:y:z:_) = fromPut $ do {putString x; putString y; putString z}

test9 :: [String] -> Builder
test9 (x:y:_) = encodeWith (encodePair writeString writeString) (x, y)


main :: IO ()
main = do
    mapM_ (print . ($ input)) [ goal, works, works2, works3, works4, works5
                              , test6, test7, test8, test9 ]
  where
    input :: [String]
    input = ["hello", "world", "!"]


