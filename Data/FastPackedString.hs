{-# OPTIONS -cpp -fffi #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FastPackedString
-- Copyright   :  (c) The University of Glasgow 2001,
--                    David Roundy 2003-2005,
--                    Don Stewart 2005
--
-- License : GPL (David says: I'm happy to also license this file BSD
--      style but don't want to bother distributing two license files
--      with darcs.
--
-- Maintainer  :  dons@cse.unsw.edu.au
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This program is free software; you can redistribute it and\/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
--
------------------------------------------------------------------------
--
-- An efficient implementation of strings.
--
-- Original GHC implementation by Bryan O\'Sullivan,
-- Rewritten to use UArray by Simon Marlow.
-- Rewritten to support slices and use ForeignPtr by David Roundy
-- Cleanups and a bug fix by Don Stewart
--

module Data.FastPackedString (
        -- * The @PackedString@ type
        PackedString,           -- abstract, instances: Eq, Ord, Show, Typeable

         -- * Converting to and from @PackedString@s
        packString,             -- :: String -> PackedString
        packWords,              -- :: [Word8] -> PackedString
        unpackPS,               -- :: PackedString -> String
        unpackWords,            -- :: PackedString -> [Word8]
        unpackPSfromUTF8,       -- :: PackedString -> String
        generatePS,             -- :: Int -> (Ptr Word8 -> Int -> IO Int) -> IO PackedString
#if defined(__GLASGOW_HASKELL__)
        constructPS,            -- :: (Ptr Word8) -> Int -> IO () -> IO PackedString
#endif
        mallocedCString2PS,     -- :: CString -> IO PackedString
        withCStringPS,          -- :: PackedString -> (CString -> IO a) -> IO a
        unsafeWithInternals,    -- :: PackedString -> (Ptr Word8 -> Int -> IO a) -> IO a

        -- * I\/O with @PackedString@s
        hGetPS,                 -- :: Handle -> Int -> IO PackedString
        hPutPS,                 -- :: Handle -> PackedString -> IO ()
        hGetContentsPS,         -- :: Handle -> IO PackedString
        readFilePS,             -- :: FilePath -> IO PackedString
        writeFilePS,            -- :: FilePath -> PackedString -> IO ()
        mmapFilePS,             -- :: FilePath -> IO PackedString

        -- * Extensions to the I\/O interface
        LazyFile(..),
        readFileLazily,         -- :: FilePath -> IO LazyFile
#if defined(USE_ZLIB)
        gzReadFilePS,           -- :: FilePath -> IO PackedString
        gzReadFileLazily,       -- :: FilePath -> IO LazyFile
        gzWriteFilePS,          -- :: FilePath -> PackedString -> IO ()
        gzWriteFilePSs,         -- :: FilePath -> [PackedString] -> IO ()
#endif

        -- * Basic list functions
        nilPS,          -- :: PackedString
        eqPS,           -- :: PackedString -> PackedString -> Bool
        comparePS,      -- :: PackedString -> PackedString -> Ordering
        consPS,         -- :: Char -> PackedString -> PackedString
        headPS,         -- :: PackedString -> Char
        tailPS,         -- :: PackedString -> PackedString
        lastPS,         -- :: PackedString -> Char
        initPS,         -- :: PackedString -> PackedString
        nullPS,         -- :: PackedString -> Bool
        lengthPS,       -- :: PackedString -> Int
        appendPS,       -- :: PackedString -> PackedString -> PackedString

        -- * List transformations
        mapPS,          -- :: (Char -> Char) -> PackedString -> PackedString
        reversePS,      -- :: PackedString -> PackedString
        joinPS,         -- :: PackedString -> [PackedString] -> PackedString

        -- * Reducing lists (folds)
        foldlPS,        -- :: (a -> Char -> a) -> a -> PackedString -> a
        foldrPS,        -- :: (Char -> a -> a) -> a -> PackedString -> a

        -- ** Special folds
        concatPS,       -- :: [PackedString] -> PackedString
        anyPS,          -- :: (Char -> Bool) -> PackedString -> Bool

        -- * Sublists
        takePS,         -- :: Int -> PackedString -> PackedString
        dropPS,         -- :: Int -> PackedString -> PackedString
        splitAtPS,      -- :: Int -> PackedString -> (PackedString, PackedString)
        takeWhilePS,    -- :: (Char -> Bool) -> PackedString -> PackedString
        dropWhilePS,    -- :: (Char -> Bool) -> PackedString -> PackedString
        spanPS,         -- :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
        breakPS,        -- :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)

        -- * Searching lists

        -- ** Searching by equality
        elemPS,         -- :: Char -> PackedString -> Bool

        -- ** Searching with a predicate
        filterPS,       -- :: (Char -> Bool) -> PackedString -> PackedString
        findPS,         -- :: (Char -> Bool) -> PackedString -> Maybe Char

        -- * Indexing lists
        indexPS,        -- :: PackedString -> Int -> Char
        elemIndexPS,    -- :: Char -> PackedString -> Maybe Int
        findIndexPS,    -- :: (Char -> Bool) -> PackedString -> Maybe Int
        findIndicesPS,  -- :: (Char -> Bool) -> PackedString -> [Int]

        -- * Special lists

        -- ** Lines and words
        linesPS,        -- :: PackedString -> [PackedString]
        unlinesPS,      -- :: [PackedString] -> PackedString
        wordsPS,        -- :: PackedString -> [PackedString]
        unwordsPS,      -- :: PackedString -> [PackedString]

        -- ** Ordered lists
        sortPS,         -- :: PackedString -> PackedString

        -- * Extensions to the List functions
        indexWord8PS,   -- :: PackedString -> Int -> Word8
        elemIndexWord8PS,-- :: Word8 -> PackedString -> Maybe Int
        dropWhitePS,    -- :: PackedString -> PackedString
        breakWhitePS,   -- :: PackedString -> Maybe (PackedString,PackedString)
        spanEndPS,      -- :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
        breakOnPS,      -- :: Char -> PackedString -> (PackedString, PackedString)
        hashPS,
        splitPS,        -- :: Char -> PackedString -> [PackedString]
        splitWithPS,    -- :: (Char -> Bool) -> PackedString -> [PackedString]
        breakFirstPS,   -- :: Char -> PackedString -> Maybe (PackedString,PackedString)
        breakLastPS,    -- :: Char -> PackedString -> Maybe (PackedString,PackedString)
        readIntPS,      -- :: PackedString -> Maybe (Int, PackedString)
        fromHex2PS,     -- :: PackedString -> PackedString
        fromPS2Hex,     -- :: PackedString -> PackedString
        betweenLinesPS, --  :: PackedString -> PackedString -> PackedString -> Maybe (PackedString)
        breakAfterNthNewline,   
        breakBeforeNthNewline,

        -- should this even be available?
        unsafeConcatLenPS, -- :: Int -> [PackedString] -> PackedString

   ) where

import Data.Bits                (rotateL)
import Data.Char                (chr, ord, String, isSpace)
import Data.Int                 (Int32)
import Data.Word                (Word8)
import Data.Maybe               (listToMaybe)

import Control.Monad            (when, liftM)
import Control.Exception        (bracket)

import System.IO
import System.IO.Unsafe         (unsafePerformIO, unsafeInterleaveIO)
import System.Mem               (performGC)

import Foreign.Ptr              (Ptr, FunPtr, plusPtr, nullPtr, minusPtr, castPtr)
import Foreign.ForeignPtr       (touchForeignPtr, newForeignPtr, withForeignPtr, mallocForeignPtrArray, ForeignPtr)
import Foreign.Storable         (peekElemOff, peek, poke)
import Foreign.C.String         (CString)
import Foreign.C.Types          (CSize, CLong, CInt)
import Foreign.Marshal.Alloc    (free)
import Foreign.Marshal.Array
import Foreign.Marshal.Utils    (with)

#if defined(__GLASGOW_HASKELL__)
import qualified Foreign.Concurrent as FC (newForeignPtr)

#if defined(USE_MMAP)
import System.Posix             (handleToFd)
#endif

#endif

-- -----------------------------------------------------------------------------

-- | A space-efficient representation of a 'String', which supports various
-- efficient operations.  A 'PackedString' contains 8-bit characters only.
--
data PackedString = PS {-# UNPACK #-} !(ForeignPtr Word8) !Int !Int

------------------------------------------------------------------------

instance Eq  PackedString 
    where (==)    = eqPS

instance Ord PackedString 
    where compare = comparePS

instance Show PackedString where
    showsPrec p ps r = showsPrec p (unpackPS ps) r

------------------------------------------------------------------------

-- | Equality on the 'PackedString' type
eqPS :: PackedString -> PackedString -> Bool
eqPS a b = (comparePS a b) == EQ
{-# INLINE eqPS #-}

-- | 'comparePS' provides an 'Ordering' for 'PackedStrings' supporting slices.
comparePS :: PackedString -> PackedString -> Ordering
comparePS (PS _ _ 0) (PS _ _ 0) = EQ    -- short cut for empty strings
comparePS (PS x1 s1 l1) (PS x2 s2 l2) = unsafePerformIO $ 
    withForeignPtr x1 $ \p1 -> 
        withForeignPtr x2 $ \p2 -> do 
            i <- c_memcmp (p1 `plusPtr` s1) (p2 `plusPtr` s2) (min l1 l2)
            return $ case i `compare` 0 of
                EQ  -> l1 `compare` l2
                x   -> x

-- Using memcmp provides a good improvement in time and space over old version.
-- For 100M strings: 
--  total alloc = 209,725,240 bytes,    0.14 secs
-- versus
--  total alloc = 1,048,585,780 bytes,  4.86 secs

-- -----------------------------------------------------------------------------
-- Constructing and destructing packed strings

-- | The empty 'PackedString'
nilPS :: PackedString
nilPS = unsafePerformIO $ mallocForeignPtr 1 >>= \fp -> return $ PS fp 0 0
{-# NOINLINE nilPS #-}

-- | Convert a 'String' into a 'PackedString'
packString :: String -> PackedString
packString str = createPS (length str) $ \p -> pokeArray p $ map c2w str

-- | Convert a '[Word8]' into a 'PackedString'
packWords :: [Word8] -> PackedString
packWords s = createPS (length s) $ \p -> pokeArray p s

-- | Convert a 'PackedString' into a 'String'
unpackPS :: PackedString -> String
unpackPS (PS ps s l) = map w2c $ unsafePerformIO $ withForeignPtr ps $ \p -> 
    peekArray l (p `plusPtr` s)

-- with mmap on openbsd,     
--  pstr <- mmapFilePS "/home/dons/tmp/tests/70k" ; 
--  unpackPS pstr `seq` return ()
-- segfaults in #0  0x1c0fe1f6 in GHCziStorable_readWord8OffPtr_info ()
-- at around 62k chars

-- | Convert a 'PackedString' to a '[Word8]'
unpackWords :: PackedString -> [Word8]
unpackWords ps@(PS x s _)
    | nullPS ps     = []
    | otherwise     =
        (unsafePerformIO $ withForeignPtr x $ \p -> peekElemOff p s) 
            : unpackWords (unsafeTailPS ps)

-- | Convert a 'PackedString' in UTF8 form to a 'String'
unpackPSfromUTF8 :: PackedString -> String
unpackPSfromUTF8 (PS _ _ 0) = []
unpackPSfromUTF8 (PS x s l) = unsafePerformIO $ withForeignPtr x $ \p -> do 
    outbuf <- mallocArray l
    lout   <- utf8_to_ints outbuf (p `plusPtr` s) l
    when (lout < 0) $ error "Bad UTF8!"
    str    <- (map chr) `liftM` peekArray lout outbuf
    free outbuf
    return str

-- | Given the maximum size needed and a function to make the contents
-- of a PackedString, generatePS makes the 'PackedString'. The
-- generating function is required to return the actual size (<= the
-- maximum size).
generatePS :: Int -> (Ptr Word8 -> IO Int) -> IO PackedString
generatePS i f = do 
    p <- mallocArray i
    i' <- f p
    p' <- reallocArray p i'
    fp <- newForeignPtr c_free p'
    return $ PS fp 0 i'

#if defined(__GLASGOW_HASKELL__)
-- | Construct a 'PackedString' given a C Word8 buffer, a length,
-- and an IO action representing a finalizer.
-- This function is not available on Hugs.
constructPS :: (Ptr Word8) -> Int -> IO () -> IO PackedString
constructPS p l f = do 
    fp <- FC.newForeignPtr p f
    return $ PS fp 0 l
#endif

mallocedCString2PS :: CString -> IO PackedString
mallocedCString2PS cs = do 
    fp <- newForeignPtr c_free (castPtr cs)
    l <- c_strlen cs
    return $ PS fp 0 (fromIntegral l)

withCStringPS :: PackedString -> (CString -> IO a) -> IO a
withCStringPS (PS ps s l) = bracket alloc free_cstring
    where 
      alloc = withForeignPtr ps $ \p -> do 
                buf <- c_malloc (fromIntegral l+1)
                c_memcpy (castPtr buf) (castPtr p `plusPtr` s) (fromIntegral l)
                poke (buf `plusPtr` l) (0::Word8)
                return $ castPtr buf

-- | Do something with the internals of a 'PackedString'. Beware of
-- altering the contents!
unsafeWithInternals :: PackedString -> (Ptr Word8 -> Int -> IO a) -> IO a
unsafeWithInternals (PS fp s l) f = withForeignPtr fp $ \p -> f (p `plusPtr` s) l

------------------------------------------------------------------------
-- Conversion between 'Word8' and 'Char'

w2c :: Word8 -> Char
w2c = chr . fromIntegral
{-# INLINE w2c #-}

c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

-- -----------------------------------------------------------------------------
-- List-like functions for PackedStrings

-- | 'consPS' is analogous to (:) for lists
consPS :: Char -> PackedString -> PackedString
consPS c (PS x s l) = createPS (l+1) $ \p -> withForeignPtr x $ \f -> do
        c_memcpy (p `plusPtr` 1) (f `plusPtr` s) l  -- 99% less space
        poke p (c2w c)

-- | Extract the first element of a packed string, which must be non-empty.
headPS :: PackedString -> Char
headPS ps@(PS x s _)        -- ps ! 0 is inlined manually to eliminate a (+0)
  | nullPS ps   = errorEmptyList "headPS"
  | otherwise   = w2c $ unsafePerformIO $ withForeignPtr x $ \p -> 
                    peekElemOff p s
{-# INLINE headPS #-}

-- | Extract the elements after the head of a packed string, which must be non-empty.
tailPS :: PackedString -> PackedString
tailPS (PS p s l) 
    | l <= 0    = errorEmptyList "tailPS"
    | l == 1    = nilPS                                                                    
    | otherwise = PS p (s+1) (l-1)
{-# INLINE tailPS #-}

-- | Extract the last element of a packed string, which must be finite and non-empty.
lastPS :: PackedString -> Char
lastPS ps@(PS x s l)        -- ps ! 0 is inlined manually to eliminate a (+0)
  | nullPS ps   = errorEmptyList "lastPS"
  | otherwise   = w2c $ unsafePerformIO $ withForeignPtr x $ \p -> 
                    peekElemOff p (s+l-1)
{-# INLINE lastPS #-}

-- | Return all the elements of a list except the last one.
-- The list must be finite and non-empty.
initPS :: PackedString -> PackedString
initPS (PS p s l) 
    | l <= 0    = errorEmptyList "initPS"
    | l == 1    = nilPS                                                                    
    | otherwise = PS p s (l-1)                                                          
{-# INLINE initPS #-}

-- | Test whether a packed string is empty.
nullPS :: PackedString -> Bool
nullPS (PS _ _ l) = l == 0
{-# INLINE nullPS #-}

-- | 'lengthPS' returns the length of a packed string as an 'Int'.
lengthPS :: PackedString -> Int
lengthPS (PS _ _ l) = l
{-# INLINE lengthPS #-}

-- | Append two packed strings
appendPS :: PackedString -> PackedString -> PackedString
appendPS xs ys
    | nullPS xs = ys
    | nullPS ys = xs
    | otherwise  = concatPS [xs,ys]
{-# INLINE appendPS #-}

-- | 'map' @f xs@ is the packed string obtained by applying @f@ to each
-- element of @xs@, i.e.,
mapPS :: (Char -> Char) -> PackedString -> PackedString
mapPS func (PS ps s l) = createPS l $ \p -> withForeignPtr ps $ \f -> 
    mint (f `plusPtr` s) p l
    where mint :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
          mint _ _ 0    = return ()
          mint f t len  = do val <- peek f
                             poke t $ c2w $ func $ w2c val
                             mint (f `plusPtr` 1) (t `plusPtr` 1) (len - 1)

-- | 'filterPS', applied to a predicate and a packed string, returns a
-- packed string containing those characters that satisfy the predicate.
filterPS :: (Char -> Bool) -> PackedString -> PackedString
filterPS f ps 
    | nullPS ps = ps
    | otherwise = packString (filter f (unpackPS ps))   -- todo better

-- | The 'find' function takes a predicate and a packed string and
-- returns the first element in matching the predicate, or 'Nothing' if
-- there is no such element.
findPS :: (Char -> Bool) -> PackedString -> Maybe Char
findPS p ps = case filterPS p ps of
            p' | nullPS p' -> Nothing
               | otherwise -> Just (unsafeHeadPS p')

-- | 'foldlPS', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a packed string, reduces the
-- packed string using the binary operator, from left to right.
foldlPS :: (a -> Char -> a) -> a -> PackedString -> a
foldlPS f b ps = foldl f b (unpackPS ps)

-- | 'foldrPS', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a packed string,
-- reduces the packed string using the binary operator, from right to left.
foldrPS :: (Char -> a -> a) -> a -> PackedString -> a
foldrPS f v ps = foldr f v (unpackPS ps)

-- | 'takeWhilePS', applied to a predicate @p@ and a packed string @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhilePS :: (Char -> Bool) -> PackedString -> PackedString
takeWhilePS f ps = seq f $ takePS (findIndexOrEndPS (not . f) ps) ps
{-# INLINE takeWhilePS #-}

-- | 'dropWhilePS' @p xs@ returns the suffix remaining after 'takeWhilePS' @p xs@.
dropWhilePS :: (Char -> Bool) -> PackedString -> PackedString
dropWhilePS f ps = seq f $ dropPS (findIndexOrEndPS (not . f) ps) ps
{-# INLINE dropWhilePS #-}

-- | 'takePS' @n@, applied to a packed string @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
takePS :: Int -> PackedString -> PackedString
takePS n ps@(PS x s l)
    | n <= 0    = nilPS
    | n >= l    = ps
    | otherwise = PS x s n
{-# INLINE takePS #-}

-- | 'dropPS' @n xs@ returns the suffix of @xs@ after the first @n@
-- elements, or @[]@ if @n > 'length' xs@.
dropPS  :: Int -> PackedString -> PackedString
dropPS n ps@(PS x s l)
    | n <= 0    = ps
    | n >  l    = nilPS
    | otherwise = PS x (s+n) (l-n)
{-# INLINE dropPS #-}

-- | 'splitAtPS' @n xs@ is equivalent to @('takePS' n xs, 'dropPS' n xs)@.
splitAtPS :: Int -> PackedString -> (PackedString, PackedString)
splitAtPS  n ps  = (takePS n ps, dropPS n ps)
{-# INLINE splitAtPS #-}

-- | 'span' @p xs@ breaks the packed string into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
spanPS :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
spanPS  p ps = breakPS (not . p) ps

-- | 'break' @p@ is equivalent to @'spanPS' ('not' . p)@.
breakPS :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
breakPS p ps = case findIndexOrEndPS p ps of n -> (takePS n ps, dropPS n ps)

-- | 'reverse' @xs@ returns the elements of @xs@ in reverse order.
reversePS :: PackedString -> PackedString
reversePS (PS x s l) = createPS l $ \p -> withForeignPtr x $ \f -> 
        c_reverse p (f `plusPtr` s) l -- 99% less space, very much faster

-- | 'elemPS' is the list membership predicate.
elemPS :: Char -> PackedString -> Bool
elemPS c ps = case elemIndexPS c ps of
    Nothing -> False
    Just _  -> True

-- | Concatenate a list of packed strings.
concatPS :: [PackedString] -> PackedString
concatPS []     = nilPS
concatPS [ps]   = ps
concatPS xs     = unsafePerformIO $ do 
    let start_size = 1024
    p <- mallocArray start_size
    f p 0 1024 xs

    where f ptr len _ [] = do 
                ptr' <- reallocArray ptr len
                fp   <- newForeignPtr c_free ptr'
                return $ PS fp 0 len

          f ptr len to_go pss@(PS p s l:pss')
           | l <= to_go = do withForeignPtr p $ \pf ->
                                 c_memcpy (ptr `advancePtr` len)
                                          (pf `advancePtr` s) l
                             f ptr (len + l) (to_go - l) pss'

           | otherwise = do let new_total = ((len + to_go) * 2) `max` (len + l)
                            ptr' <- reallocArray ptr new_total
                            f ptr' len (new_total - len) pss

-- | Analogous to @concatPS@, only you tell it how big the result will
-- be. If you lie then Bad Things will happen.
unsafeConcatLenPS :: Int -> [PackedString] -> PackedString
unsafeConcatLenPS n []   = n `seq` nilPS
unsafeConcatLenPS _ [ps] = ps
unsafeConcatLenPS total_length pss = createPS total_length $ \p-> cpPSs p pss
    where cpPSs :: Ptr Word8 -> [PackedString] -> IO ()
          cpPSs _ [] = return ()
          cpPSs p (PS x s l:rest) = do 
                withForeignPtr x $ \pf -> c_memcpy p (pf `plusPtr` s) l
                cpPSs (p `plusPtr` l) rest

-- | 'PackedString' index (subscript) operator, starting from 0.
indexPS :: PackedString -> Int -> Char
indexPS ps n 
    | n < 0            = error "FastPackedString.indexPS: negative index"
    | n >= lengthPS ps = error "FastPackedString.indexPS: index too large"
    | otherwise        = w2c $ ps ! n
{-# INLINE indexPS #-}

-- | 'indexWord8PS' is like 'indexPS', except that the result is of type 'Word8'
indexWord8PS :: PackedString -> Int -> Word8
indexWord8PS ps n 
    | n < 0            = error "FastPackedString.indexWord8PS: negative index"
    | n >= lengthPS ps = error "FastPackedString.indexWord8PS: index too large"
    | otherwise        = ps ! n
{-# INLINE indexWord8PS #-}

-- (Internal) unsafe 'PackedString' index (subscript) operator, starting
-- from 0, returning a 'Word8'
(!) :: PackedString -> Int -> Word8
(PS x s _l) ! i = unsafePerformIO $ withForeignPtr x $ \p -> peekElemOff p (s+i)
{-# INLINE (!) #-}

-- | Applied to a predicate and a packed string, 'anyPS' determines if
-- any element of the list satisfies the predicate.
anyPS :: (Char -> Bool) -> PackedString -> Bool
anyPS f (PS x s l) = unsafePerformIO $ withForeignPtr x $ \ptr ->
        lookat (ptr `plusPtr` s) (ptr `plusPtr` (s+l))
    where lookat :: Ptr Word8 -> Ptr Word8 -> IO Bool
          lookat p st | p == st     = return False
                      | otherwise   = do w <- peek p
                                         if f $ w2c w
                                            then return True
                                            else lookat (p `plusPtr` 1) st

-- | 'linesPS' breaks a packed string up into a list of packed strings
-- at newline characters.  The resulting strings do not contain
-- newlines.
linesPS :: PackedString -> [PackedString]
linesPS ps 
    | nullPS ps = []
    | otherwise = case elemIndexWord8PS (c2w '\n') ps of
             Nothing -> [ps]
             Just n -> takePS n ps : linesPS (dropPS (n+1) ps)
{-# INLINE linesPS #-}

-- | 'unlinesPS' is an inverse operation to 'linesPS'.  It joins lines,
-- after appending a terminating newline to each.
unlinesPS :: [PackedString] -> PackedString
unlinesPS ss = concatPS $ map (\s -> s `appendPS` newline) ss
    where   
      newline = packString "\n"

-- | 'wordsPS' breaks a packed string up into a list of words, which
-- were delimited by white space.
wordsPS :: PackedString -> [PackedString]
wordsPS ps = filter (not.nullPS) (splitWithPS isSpace ps)

{-
-- darcs version has different behaviour.
-- We may wish to export these under an alternate name.

linesPS :: PackedString -> [PackedString]
linesPS ps = case wfindPS (c2w '\n') ps of
             Nothing -> [ps]
             Just n -> takePS n ps : linesPS (dropPS (n+1) ps)

unlinesPS :: [PackedString] -> PackedString
unlinesPS ss = concatPS $ intersperse_newlines ss
    where intersperse_newlines (a:b:s) = a:newline: intersperse_newlines (b:s)
          intersperse_newlines s = s
          newline = packString "\n"

wordsPS :: PackedString -> [PackedString]
wordsPS ps = splitWithPS isSpace ps

-}

-- | The 'unwordsPS' function is analogous to the 'unwords' function.
unwordsPS :: [PackedString] -> PackedString
unwordsPS = joinPS (packString " ")

-- | The 'joinPS' function takes a 'PackedString' and a list of 'PackedString's
-- and concatenates the list after interspersing the first argument between
-- each element of the list.
joinPS :: PackedString -> [PackedString] -> PackedString
joinPS filler pss = concatPS (splice pss)
    where
        splice []  = []
        splice [x] = [x]
        splice (x:y:xs) = x:filler:splice (y:xs)

-- | Sort using an /unstable/ sorting algorithm (QSORT(3))
sortPS :: PackedString -> PackedString
sortPS (PS x s l) = createPS l $ \p -> withForeignPtr x $ \f -> do
        c_memcpy p (f `plusPtr` s) l
        c_qsort p l -- inplace

-- | The 'elemIndexPS' function returns the index of the first element
-- in the given 'PackedString' which is equal (by memchr) to the query
-- element, or 'Nothing' if there is no such element.
elemIndexPS :: Char -> PackedString -> Maybe Int
elemIndexPS c ps = elemIndexWord8PS (c2w c) ps
{-# INLINE elemIndexPS #-}

-- | 'elemIndexWord8PS' is like 'elemIndexPS', except that it takes a
-- 'Word8' as the element to search for.
elemIndexWord8PS :: Word8 -> PackedString -> Maybe Int
elemIndexWord8PS c (PS x s l) = unsafePerformIO $ 
    withForeignPtr x $ \p -> do
        let p' = p `plusPtr` s
            q  = memchr p' (fromIntegral c) (fromIntegral l)
        return $ if q == nullPtr then Nothing else Just (q `minusPtr` p')
{-# INLINE elemIndexWord8PS #-}

-- | The 'findIndexPS' function takes a predicate and a 'PackedString'
-- and returns the index of the first element in the packed string
-- satisfying the predicate.
findIndexPS :: (Char -> Bool) -> PackedString -> Maybe Int
findIndexPS f = listToMaybe . findIndicesPS f

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndicesPS :: (Char -> Bool) -> PackedString -> [Int]
findIndicesPS p ps = loop 0 ps
	where
       loop n ps' | nullPS ps'           = []
       loop n ps' | p (unsafeHeadPS ps') = n : loop (n + 1) (unsafeTailPS ps')
                  | otherwise            = loop (n + 1) (unsafeTailPS ps')

-- | 'findIndicesPS' is a variant of findIndexPS, that returns the
-- length of the string if no element is found, rather than Nothing.
findIndexOrEndPS :: (Char -> Bool) -> PackedString -> Int
findIndexOrEndPS f ps
    | nullPS ps           = 0
    | f (unsafeHeadPS ps) = 0
    | otherwise           = seq f $ 1 + findIndexOrEndPS f (unsafeTailPS ps)

------------------------------------------------------------------------
-- Extensions to the list interface

-- (Internal) unchecked version of @headPS@, you must provide some
-- guarantee that the packed string is non-empty
unsafeHeadPS :: PackedString -> Char
unsafeHeadPS (PS x s _) = w2c $ unsafePerformIO $ withForeignPtr x $ \p -> 
                    peekElemOff p s
{-# INLINE unsafeHeadPS #-}

-- (Internal) unchecked version of @tailPS@, as for unsafeHeadPS
unsafeTailPS :: PackedString -> PackedString
unsafeTailPS (PS ps s l)
    | l == 1 = nilPS
    | otherwise  = PS ps (s+1) (l-1)
{-# INLINE unsafeTailPS #-}

findFromEndUntilPS :: (Char -> Bool) -> PackedString -> Int
findFromEndUntilPS f ps@(PS x s l) = seq f $
    if nullPS ps then 0
    else if f $ lastPS ps then l
         else findFromEndUntilPS f (PS x s (l-1))

{-# INLINE dropWhitePS #-}
dropWhitePS :: PackedString -> PackedString
dropWhitePS (PS x s l) =
    unsafePerformIO $ withForeignPtr x $ \p->
    do i <- first_nonwhite (p `plusPtr` s) l
       return $ if i == l then nilPS else PS x (s+i) (l-i)

spanEndPS :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
spanEndPS  p ps = splitAtPS (findFromEndUntilPS (not.p) ps) ps

{-# INLINE breakOnPS #-}
breakOnPS :: Char -> PackedString -> (PackedString, PackedString)
breakOnPS c p = case elemIndexPS c p of
                    Nothing -> (p,nilPS)
                    Just n -> (takePS n p, dropPS n p)

{-# INLINE hashPS #-}
hashPS :: PackedString -> Int32
hashPS (PS x s l) = unsafePerformIO $ withForeignPtr x $ \p -> do 
    hash (p `plusPtr` s) l

hash :: Ptr Word8 -> Int -> IO Int32
hash ptr len = f (0 :: Int32) ptr len
 where f h _ 0 = return h
       f h p n = do x <- peek p
                    let h' =  (fromIntegral x) + (rotateL h 8)
                    f h' (p `advancePtr` 1) (n-1)

{-# INLINE breakWhitePS #-}
breakWhitePS :: PackedString -> (PackedString,PackedString)
breakWhitePS (PS x s l) =
    unsafePerformIO $ withForeignPtr x $ \p->
    do i <- first_white (p `plusPtr` s) l
       if i == 0 then return (nilPS, PS x s l)
                 else if i == l
                      then return (PS x s l, nilPS)
                      else return (PS x s i, PS x (s+i) (l-i))

{-# INLINE breakFirstPS #-}
breakFirstPS :: Char -> PackedString -> Maybe (PackedString,PackedString)
breakFirstPS c p = case elemIndexPS c p of
                       Nothing -> Nothing
                       Just n -> Just (takePS n p, dropPS (n+1) p)

{-# INLINE breakLastPS #-}
breakLastPS :: Char -> PackedString -> Maybe (PackedString,PackedString)
breakLastPS c p = case findLastPS c p of
                      Nothing -> Nothing
                      Just n -> Just (takePS n p, dropPS (n+1) p)

{-# INLINE findLastPS #-}
findLastPS :: Char -> PackedString -> Maybe Int
findLastPS c ps = wfindLastPS (c2w c) ps

{-# INLINE wfindLastPS #-}
wfindLastPS :: Word8 -> PackedString -> Maybe Int
wfindLastPS c (PS x s l) =
    unsafePerformIO $ withForeignPtr x $ \p->
                    findit (-1) (p `plusPtr` s) 0
    where findit h p i = if i >= l
                         then if h < 0
                              then return Nothing
                              else return $ Just h
                         else do here <- peekElemOff p i
                                 if c == here
                                    then findit i p (i+1)
                                    else findit h p (i+1)

{-# INLINE splitPS #-}
splitPS :: Char -> PackedString -> [PackedString]
splitPS c = wsplitPS (c2w c)

{-# INLINE wsplitPS #-}
wsplitPS :: Word8 -> PackedString -> [PackedString]
wsplitPS c ps = case elemIndexWord8PS c ps of
                Nothing -> if nullPS ps then [] else [ps]
                Just n -> takePS n ps : wsplitPS c (dropPS (n+1) ps)

splitWithPS :: (Char -> Bool) -> PackedString -> [PackedString]
splitWithPS f ps =
    case [ m | m <- [0..lengthPS ps-1], f (w2c (ps ! m)) ] of
    [] -> if nullPS ps then [] else [ps]
    (n:_) -> takePS n ps : splitWithPS f (dropPS (n+1) ps)

-- | readIntPS skips any whitespace at the beginning of its argument, and
-- reads an Int from the beginning of the PackedString.  If there is no
-- integer at the beginning of the string, it returns Nothing, otherwise it
-- just returns the int read, along with a PackedString containing the
-- remainder of its input.  The actual parsing is done by the standard C
-- library function strtol.
--
readIntPS :: PackedString -> Maybe (Int, PackedString)
readIntPS (PS x s l) = unsafePerformIO $ withForeignPtr x $ \p -> 
    with p $ \endpp -> do
       val <- c_strtol (p `plusPtr` s) endpp 0
       skipped <- (`minusPtr` (p `plusPtr` s)) `liftM` peek endpp
       if skipped == 0
          then return Nothing
          else return $ Just (fromIntegral val,
                              PS x (s+skipped) (l-skipped))

-- | fromPS2Hex
fromPS2Hex :: PackedString -> PackedString
fromPS2Hex (PS x s l) = createPS (2*l) $ \p -> withForeignPtr x $ \f ->
           conv_to_hex p (f `plusPtr` s) l

-- | fromHex2PS
fromHex2PS :: PackedString -> PackedString
fromHex2PS (PS x s l) = createPS (l `div` 2) $ \p -> withForeignPtr x $ \f ->
           conv_from_hex p (f `plusPtr` s) (l `div` 2)

-- | betweenLinesPS returns the PackedString between the two lines given,
-- or Nothing if they do not appear.
betweenLinesPS :: PackedString -> PackedString -> PackedString -> Maybe (PackedString)
betweenLinesPS start end ps
 = case break (start ==) (linesPS ps) of
       (_, _:rest@(PS ps1 s1 _:_)) ->
           case break (end ==) rest of
               (_, PS _ s2 _:_) -> Just $ PS ps1 s1 (s2 - s1)
               _ -> Nothing
       _ -> Nothing

-- | breakAfterNthNewline
breakAfterNthNewline :: Int -> PackedString -> Maybe (PackedString, PackedString)
breakAfterNthNewline 0 the_ps | nullPS the_ps = Just (nilPS, nilPS)
breakAfterNthNewline n the_ps@(PS fp the_s l)
 = unsafePerformIO $ withForeignPtr fp $ \p ->
   do let findit 0 s | s == end = return $ Just (the_ps, nilPS)
          findit _ s | s == end = return Nothing
          findit 0 s = let left_l = s - the_s
                       in return $ Just (PS fp the_s left_l,
                                         PS fp s (l - left_l))
          findit i s = do w <- peekElemOff p s
                          if w == nl then findit (i-1) (s+1)
                                     else findit i (s+1)
          nl = c2w '\n'
          end = the_s + l
      findit n the_s

-- | breakBeforeNthNewline
breakBeforeNthNewline :: Int -> PackedString -> (PackedString, PackedString)
breakBeforeNthNewline 0 the_ps | nullPS the_ps = (nilPS, nilPS)
breakBeforeNthNewline n the_ps@(PS fp the_s l)
 = unsafePerformIO $ withForeignPtr fp $ \p ->
   do let findit _ s | s == end = return (the_ps, nilPS)
          findit i s = do w <- peekElemOff p s
                          if w == nl
                            then if i == 0
                                 then let left_l = s - the_s
                                      in return (PS fp the_s left_l,
                                                 PS fp s (l - left_l))
                                 else findit (i-1) (s+1)
                            else findit i (s+1)
          nl = c2w '\n'
          end = the_s + l
      findit n the_s

-- -----------------------------------------------------------------------------

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptyList :: String -> a
errorEmptyList fun = error ("FastPackedString." ++ fun ++ ": empty list")

------------------------------------------------------------------------
--
-- The definition of @_substrPS@ is essentially:
-- @take (end - begin + 1) (drop begin str)@.

{-
substrPS :: PackedString -> Int -> Int -> PackedString
substrPS (PS ps s _) begin end = PS ps (s+begin) (1+end-begin)
-}

----------------------------------------------------------------------------

-- | A way of creating ForeignPtrs outside the IO monad (although it
-- still isn't entirely "safe", but at least it's convenient.
createPS :: Int -> (Ptr Word8 -> IO ()) -> PackedString
createPS l write_ptr = unsafePerformIO $ do 
    fp <- mallocForeignPtr l
    withForeignPtr fp $ \p -> write_ptr p
    return $ PS fp 0 l

------------------------------------------------------------------------

-- (internal) GC wrapper of mallocForeignPtrArray
mallocForeignPtr :: Int -> IO (ForeignPtr Word8)
mallocForeignPtr l = when (l > 1000000) performGC >> mallocForeignPtrArray l

-- -----------------------------------------------------------------------------
-- I\/O functions

-- | Outputs a 'PackedString' to the specified 'Handle'.
--
-- NOTE: the representation of the 'PackedString' in the file is assumed to
-- be in the ISO-8859-1 encoding.  In other words, only the least signficant
-- byte is taken from each character in the 'PackedString'.
--
hPutPS :: Handle -> PackedString -> IO ()
hPutPS _ (PS _ _ 0)  = return ()
hPutPS h (PS ps 0 l) = withForeignPtr ps $ \p-> hPutBuf h p l
hPutPS h (PS ps s l) = withForeignPtr ps $ \p-> hPutBuf h (p `plusPtr` s) l

-- | Read a 'PackedString' directly from the specified 'Handle'.  This
-- is far more efficient than reading the characters into a 'String'
-- and then using 'packString'.
--
-- NOTE: as with 'hPutPS', the string representation in the file is
-- assumed to be ISO-8859-1.
--
hGetPS :: Handle -> Int -> IO PackedString
hGetPS _ 0 = return nilPS
hGetPS h i = do fp <- mallocForeignPtr i
                l  <- withForeignPtr fp $ \p-> hGetBuf h p i
                return $ PS fp 0 l

-- | Read entire handle contents into a 'PackedString'.
--
-- NOTE: as with 'hGetPS', the string representation in the file is
-- assumed to be ISO-8859-1.
--
hGetContentsPS :: Handle -> IO PackedString
hGetContentsPS h = do 
    let start_size = 1024
    p <- mallocArray start_size
    i <- hGetBuf h p start_size
    if i < start_size
        then do p' <- reallocArray p i
                fp <- newForeignPtr c_free p'
                return $ PS fp 0 i
        else f p start_size
    where 
        f p s = do 
        let s' = 2 * s
        p' <- reallocArray p s'
        i  <- hGetBuf h (p' `plusPtr` s) s
        if i < s 
            then do let i' = s + i
                    p'' <- reallocArray p' i'
                    fp  <- newForeignPtr c_free p''
                    return $ PS fp 0 i'
            else f p' s'

-- | Read an entire file directly into a 'PackedString'.  This is far more
-- efficient than reading the characters into a 'String' and then using
-- 'packString'.  It also may be more efficient than opening the file and
-- reading it using hGetPS.
--
-- NOTE: as with 'hGetPS', the string representation in the file is
-- assumed to be ISO-8859-1.
--
readFilePS :: FilePath -> IO PackedString
readFilePS f = do 
    h <- openBinaryFile f ReadMode
    l <- hFileSize h
    s <- hGetPS h $ fromIntegral l
    hClose h
    return s

-- | Write a 'PackedString' to a file.
--
writeFilePS :: FilePath -> PackedString -> IO ()
writeFilePS f ps = do 
    h <- openBinaryFile f WriteMode
    hPutPS h ps
    hClose h

-- | Like readFilePS, this reads an entire file directly into a
-- 'PackedString', but it is even more efficient.  It involves directly
-- mapping the file to memory.  This has the advantage that the contents
-- of the file never need to be copied.  Also, under memory pressure the
-- page may simply be discarded, wile in the case of readFilePS it would
-- need to be written to swap.  If you read many small files, mmapFilePS
-- will be less memory-efficient than readFilePS, since each mmapFilePS
-- takes up a separate page of memory.  Also, you can run into bus
-- errors if the file is modified.  NOTE: as with 'readFilePS', the
-- string representation in the file is assumed to be ISO-8859-1.
--
mmapFilePS :: FilePath -> IO PackedString
mmapFilePS f = 
#if defined(USE_MMAP)
   mmap f >>= \(fp,l) -> return $ PS fp 0 l
#else
   readFilePS f
#endif

use_mmap :: Bool
#if defined(USE_MMAP)
use_mmap = True
#else
use_mmap = False
#endif

#if defined(USE_MMAP)
mmap :: FilePath -> IO (ForeignPtr Word8, Int)
mmap f = do
    h <- openBinaryFile f ReadMode
    l <- fromIntegral `liftM` hFileSize h
    -- Don't bother mmaping small files because each mmapped file takes up
    -- at least one full VM block.
    if l < mmap_limit
       then do thefp <- mallocForeignPtr l
               withForeignPtr thefp $ \p-> hGetBuf h p l
               hClose h
               return (thefp, l)
       else do
#if defined(__GLASGOW_HASKELL__)
               fd <- fromIntegral `liftM` handleToFd h
               p <- my_mmap l fd
               fp <- if p == nullPtr
                     then
#else
               fp <-
#endif
                          do thefp <- mallocForeignPtr l
                             withForeignPtr thefp $ \p' -> hGetBuf h p' l
                             return thefp
#if defined(__GLASGOW_HASKELL__)
                     else do
                          -- The munmap leads to crashes on OpenBSD.
                          -- maybe there's a use after unmap in there somewhere?
#if !defined(__OpenBSD__)
                             fp <- FC.newForeignPtr p (c_munmap p l >> return ())
#else
                             fp <- FC.newForeignPtr p (return ())
#endif
                             return fp
               c_close fd
#endif
               hClose h
               return (fp, l)
    where mmap_limit = 16*1024

#endif /* USE_MMAP */

-- -----------------------------------------------------------------------------

data LazyFile = LazyString String
              | MMappedPackedString PackedString
              | LazyPackedStrings [PackedString]
    deriving Eq

readFileLazily :: FilePath -> IO LazyFile
readFileLazily f =
#if defined(__GLASGOW_HASKELL__)
    if use_mmap
      then liftM MMappedPackedString (mmapFilePS f)
      else
#endif
           do h <- openBinaryFile f ReadMode
              liftM LazyPackedStrings $ readHandleLazily h

readHandleLazily :: Handle -> IO [PackedString]
readHandleLazily h
 = do let read_rest = do
              -- We might be making too big a fp here
              fp <- mallocForeignPtr blocksize
              lread <- withForeignPtr fp
                     $ \p -> hGetBuf h p blocksize
              case lread of
                  0 -> return []
                  l -> do rest <- unsafeInterleaveIO read_rest
                          return (PS fp 0 l:rest)
      unsafeInterleaveIO read_rest
    where blocksize = 1024

-- -----------------------------------------------------------------------------
-- gzReadFilePS

-- | Read an entire file, which may or may not be gzip compressed, directly
-- into a 'PackedString'.

#if defined(USE_ZLIB)

gzReadFilePS :: FilePath -> IO PackedString
gzReadFilePS f = do
    h <- openBinaryFile f ReadMode
    header <- hGetPS h 2
    if header /= packString "\31\139"
       then do hClose h
               mmapFilePS f
       else do hSeek h SeekFromEnd (-4)
               len <- hGetLittleEndInt h
               hClose h
               withCString f $ \fstr-> withCString "rb" $ \rb-> do
                 gzf <- c_gzopen fstr rb
                 when (gzf == nullPtr) $ fail $ "problem opening file "++f
                 fp <- mallocForeignPtr len
                 lread <- withForeignPtr fp $ \p -> c_gzread gzf p len
                 c_gzclose gzf
                 when (lread /= len) $ fail $ "problem gzreading file "++f
                 return $ PS fp 0 len

gzReadFileLazily :: FilePath -> IO LazyFile
gzReadFileLazily f = do
    h <- openBinaryFile f ReadMode
    header <- hGetPS h 2
    if header == packString "\31\139" then
        do hClose h
           withCString f $ \fstr-> withCString "rb" $ \rb-> do
               gzf <- c_gzopen fstr rb
               when (gzf == nullPtr) $ fail $ "problem opening file "++f
               let read_rest = do
                       -- We might be making too big a fp here
                       fp <- mallocForeignPtr blocksize
                       lread <- withForeignPtr fp
                              $ \p -> c_gzread gzf p blocksize
                       case lread of
                           0 -> do c_gzclose gzf
                                   return []
                           -1 -> fail $ "problem gzreading file "++f
                           l -> do rest <- unsafeInterleaveIO read_rest
                                   return (PS fp 0 l:rest)
               liftM LazyPackedStrings read_rest
#if defined(__GLASGOW_HASKELL__)
        else if use_mmap then
            do hClose h
               liftM MMappedPackedString (mmapFilePS f)
#endif
        else liftM (LazyPackedStrings . (header:)) $ readHandleLazily h
    where blocksize = 1024

hGetLittleEndInt :: Handle -> IO Int
hGetLittleEndInt h = do
    b1 <- ord `liftM` hGetChar h
    b2 <- ord `liftM` hGetChar h
    b3 <- ord `liftM` hGetChar h
    b4 <- ord `liftM` hGetChar h
    return $ b1 + 256*b2 + 65536*b3 + 16777216*b4

gzWriteFilePS :: FilePath -> PackedString -> IO ()
gzWriteFilePS f ps = gzWriteFilePSs f [ps]

gzWriteFilePSs :: FilePath -> [PackedString] -> IO ()
gzWriteFilePSs f pss  =
    withCString f $ \fstr -> withCString "wb" $ \wb -> do
    gzf <- c_gzopen fstr wb
    when (gzf == nullPtr) $ fail $ "problem gzopening file for write: "++f
    mapM_ (gzWriteToGzf gzf) pss `catch`
              \_ -> fail $ "problem gzwriting file: "++f
    c_gzclose gzf

gzWriteToGzf :: Ptr () -> PackedString -> IO ()
gzWriteToGzf gzf (PS x s l) = do
    lw <- withForeignPtr x $ \p -> c_gzwrite gzf (p `plusPtr` s) l
    when (lw /= l) $ fail $ "problem in gzWriteToGzf"

#endif /* USE_ZLIB */

------------------------------------------------------------------------
-- TODO reduce the number of foreign imports if possible

foreign import ccall unsafe "static stdlib.h malloc" c_malloc
    :: CInt -> IO (Ptr Word8)

foreign import ccall unsafe "static stdio.h &free" c_free
    :: FunPtr (Ptr Word8 -> IO ())

foreign import ccall unsafe "static stdlib.h free" free_cstring
    :: CString -> IO ()

foreign import ccall unsafe "static string.h memcmp" c_memcmp
    :: Ptr Word8 -> Ptr Word8 -> Int -> IO Int

foreign import ccall unsafe "static string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()

foreign import ccall unsafe "string.h memchr" memchr
    :: Ptr Word8 -> CInt -> CSize -> Ptr Word8

foreign import ccall unsafe "static string.h strlen" c_strlen
    :: CString -> IO CInt

foreign import ccall unsafe "static stdlib.h strtol" c_strtol
    :: Ptr Word8 -> Ptr (Ptr Word8) -> Int -> IO CLong

------------------------------------------------------------------------

foreign import ccall unsafe "static fpstring.h utf8_to_ints" utf8_to_ints
    :: Ptr Int -> Ptr Word8 -> Int -> IO Int

foreign import ccall unsafe "fpstring.h first_nonwhite" first_nonwhite
    :: Ptr Word8 -> Int -> IO Int

foreign import ccall unsafe "fpstring.h first_white" first_white
    :: Ptr Word8 -> Int -> IO Int

foreign import ccall unsafe "static fpstring.h conv_to_hex" conv_to_hex
    :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()

foreign import ccall unsafe "static fpstring.h conv_from_hex" conv_from_hex
    :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()

foreign import ccall unsafe "static fpstring.h reverse" c_reverse
    :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()

foreign import ccall unsafe "static fpstring.h my_qsort" c_qsort
    :: Ptr Word8 -> Int -> IO ()

------------------------------------------------------------------------

#if defined(USE_MMAP)
foreign import ccall unsafe "static fpstring.h my_mmap" my_mmap
    :: Int -> Int -> IO (Ptr Word8)

foreign import ccall unsafe "static sys/mman.h munmap" c_munmap
    :: Ptr Word8 -> Int -> IO Int

foreign import ccall unsafe "static unistd.h close" c_close
    :: Int -> IO Int
#endif

#if defined(USE_ZLIB)
foreign import ccall unsafe "static zlib.h gzopen" c_gzopen
    :: CString -> CString -> IO (Ptr ())

foreign import ccall unsafe "static zlib.h gzclose" c_gzclose
    :: Ptr () -> IO ()

foreign import ccall unsafe "static zlib.h gzread" c_gzread
    :: Ptr () -> Ptr Word8 -> Int -> IO Int

foreign import ccall unsafe "static zlib.h gzwrite" c_gzwrite
    :: Ptr () -> Ptr Word8 -> Int -> IO Int
#endif
