{-# OPTIONS_HADDOCK hide #-}
-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Testing the correctness of bounded encodings. See the file 'test/TestAll.hs'
-- for an example on how to use the functions provided here.
-- 
module Codec.Bounded.Encoding.Internal.Test (
    EncodingFailure
  , evalEncoding
  , showEncoding
  , testEncoding
  , cmpEncoding
  , cmpEncoding_
  , cmpEncodingErr

  ) where

import Data.Maybe
import Foreign
import Numeric (showHex)

import Codec.Bounded.Encoding.Internal
import qualified Codec.Bounded.Encoding.Internal.Region as R

------------------------------------------------------------------------------
-- Testing Encodings
------------------------------------------------------------------------------

-- Representing failures
------------------------

-- | A failure of an 'Encoding'.
data EncodingFailure = EncodingFailure  String  EncodingResult  EncodingResult
       deriving( Eq )

type EncodingResult = ( [Word8]         -- full list
                   , [[Word8]]       -- split list
                   , [Ptr Word8] )   -- in-write pointers

instance Show EncodingFailure where
    show (EncodingFailure cause res1 res2) = unlines $
            [ ""
            , "Encoding violated post-condition: " ++ cause ++ "!"
            ] ++
            (map ("  " ++) $ lines $ unlines
                [ "String based result comparison:"
                , showEncodingResult stringLine 1 res1
                , showEncodingResult stringLine 2 res2
                , "Hex based result comparison:"
                , showEncodingResult hexLine 1 res1
                , showEncodingResult hexLine 2 res2 
                ] )
      where
        hexLine = concatMap (\x -> pad2 $ showHex x "")
        pad2 [ ] = '0':'0':[]
        pad2 [x] = '0':x  :[]
        pad2 xs  = xs

        stringLine = map (toEnum . fromIntegral)

        showEncodingResult line i (full, splits, ptrs) =
            unlines $ zipWith (++) names 
                    $ map (quotes . line) (full : splits) ++ [ppPtrs]
          where
            names = [ show (i::Int) ++ " total result:   "
                    , "  front slack:    "
                    , "  write result:   "
                    , "  reserved space: "
                    , "  back slack:     "
                    , "  pointers/diffs: "
                    ]
            quotes xs = "'" ++ xs ++ "'"
            ppPtrs = show (head ptrs) ++ do
                (p1,p2) <- zip ptrs (tail ptrs)
                "|" ++ show (p2 `minusPtr` p1) ++ "|" ++ show p2

 
-- Execution a write and testing its invariants
-----------------------------------------------

-- | Execute an 'Encoding' and return the written list of bytes.
evalEncoding :: Encoding a -> a -> [Word8]
evalEncoding w x = case testEncoding w x of
    Left err  -> error $ "evalEncoding: " ++ show err
    Right res -> res

-- | Execute an 'Encoding' and return the written list of bytes interpreted as
-- Unicode codepoints.
showEncoding :: Encoding a -> a -> [Char]
showEncoding w = map (toEnum . fromEnum) . evalEncoding w

-- | Execute an 'Encoding' twice and check that all post-conditions hold and the
-- written values are identical. In case of success, a list of the written
-- bytes is returned.
testEncoding :: Encoding a -> a -> Either EncodingFailure [Word8]
testEncoding = testEncodingWith (5, 11)

testEncodingWith :: (Int, Int) -> Encoding a -> a -> Either EncodingFailure [Word8]
testEncodingWith (slackF, slackB) w x = unsafePerformIO $ do
    res1@(xs1, _, _) <- execEncoding (replicate (slackF + slackB + bound) 40)
    res2             <- execEncoding (invert xs1)
    return $ check res1 res2
  where
    bound = getBound w

    invert = map complement

    check res1@(xs1, [frontSlack1, written1, reserved1, backSlack1], ptrs1)
          res2@(_  , [frontSlack2, written2, reserved2, backSlack2], ptrs2)
      -- test properties of first write
      | length frontSlack1 /= slackF                = err "front slack length"
      | length backSlack1   /= slackB               = err "back slack length"
      | length xs1 /= slackF + slackB + bound       = err "total length"
      | not (ascending ptrs1)                       = err "pointers 1"
      -- test remaining properties of second write
      | not (ascending ptrs2)                       = err "pointers 2"
      -- compare encodings
      | frontSlack1      /= invert frontSlack2      = err "front over-write"
      | backSlack1       /= invert backSlack2       = err "back over-write"
      | written1         /= written2                = err "different encodings"
      | length reserved1 /= length reserved2        = err "different reserved lengths"
      | any (\(a,b) -> a /= complement b) untouched = err "different reserved usage"
      | otherwise                                   = Right written1
      where
        (_, untouched) = break (uncurry (/=)) $ zip reserved1 reserved2
        err info = Left (EncodingFailure info res1 res2)
        ascending xs = all (uncurry (<=)) $ zip xs (tail xs)
    check _ _ = error "impossible"

    -- list-to-memory, run write, memory-to-list, report results
    execEncoding ys0 = do
      r@(buf, size) <- R.fromList ys0
      withForeignPtr buf $ \sp -> do
          let ep      = sp `plusPtr` size
              op      = sp `plusPtr` slackF
              opBound = op `plusPtr` bound
          op' <- runEncoding w x op
          ys1 <- R.toList r
          touchForeignPtr buf
          -- cut the written list into: front slack, written, reserved, back slack
          case splitAt (op `minusPtr` sp) ys1 of
              (frontSlack, ys2) -> case splitAt (op' `minusPtr` op) ys2 of
                  (written, ys3) -> case splitAt (opBound `minusPtr` op') ys3 of
                      (reserved, backSlack) -> return $
                          (ys1, [frontSlack, written, reserved, backSlack], [sp, op, op', opBound, ep])

-- | Compare an 'Encoding' against a reference implementation. @cmpEncoding f e x@
-- returns 'Nothing' iff the encoding @e@ and the function @f@ yield the same
-- result when applied to @x@.
cmpEncoding :: (a -> [Word8]) -> Encoding a -> a 
         -> Maybe (a, [Word8], Either EncodingFailure [Word8])
cmpEncoding f w x 
  | result == Right (f x) = Nothing
  | otherwise             = Just (x, f x, result)
  where
    result = testEncoding w x

-- | Like 'cmpEncoding', but return only whether the write yielded the same result
-- as the reference implementation.
cmpEncoding_ :: Show a => (a -> [Word8]) -> Encoding a -> a -> Bool
cmpEncoding_ f w = isNothing . cmpEncoding f w

-- | Like 'cmpEncoding', but return an error using @error . show@. This is a
-- convenient way to get a QuickCheck test to output debug information about
-- what went wrong.
cmpEncodingErr :: Show a => (a -> [Word8]) -> Encoding a -> a -> Bool
cmpEncodingErr f w = maybe True (error . show) . cmpEncoding f w


