{-# LANGUAGE PackageImports, BangPatterns, ScopedTypeVariables #-}

import           Control.Arrow (first)
import           Control.Monad
import           Data.Char (ord, chr)
import           Data.Monoid
import           Numeric (showHex, readHex)
import           Foreign
import           System.ByteOrder 
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck (Arbitrary)
import           Unsafe.Coerce (unsafeCoerce)

import qualified "new-bytestring" Data.ByteString                                      as S
import qualified "new-bytestring" Data.ByteString.Lazy                                 as L
import qualified "new-bytestring" Data.ByteString.Lazy.Internal                        as L
import qualified "new-bytestring" Data.ByteString.Lazy.Builder.BasicEncoding           as BE
import           "new-bytestring" Data.ByteString.Lazy.Builder.BasicEncoding.Internal
import           "new-bytestring" Data.ByteString.Lazy.Builder
import           "new-bytestring" Data.ByteString.Lazy.Builder.Internal
import           "new-bytestring" Data.ByteString.Lazy.Builder.Extras


-- Testbed for 'chunkedEncoding' and 'sizePrefixedEncoding'.


-- Variable length encodings
----------------------------

-- | Variable length encoding.
genVar_list :: (Ord a, Num a, Bits a, Integral a) => a -> [Word8]
genVar_list x
  | x <= 0x7f = sevenBits            : []
  | otherwise = (sevenBits .|. 0x80) : genVar_list (x `shiftR` 7)
  where
    sevenBits = fromIntegral x .&. 0x7f

int8Var_list :: Int8 -> [Word8]
int8Var_list  = genVar_list . (fromIntegral :: Int8 -> Word8)

int16Var_list :: Int16 -> [Word8]
int16Var_list = genVar_list . (fromIntegral :: Int16 -> Word16)

int32Var_list :: Int32 -> [Word8]
int32Var_list = genVar_list . (fromIntegral :: Int32 -> Word32)

int64Var_list :: Int64 -> [Word8]
int64Var_list = genVar_list . (fromIntegral :: Int64 -> Word64)

intVar_list :: Int -> [Word8]
intVar_list = genVar_list . (fromIntegral :: Int -> Word)

-- | Parse a variable length encoding
parseVar :: (Num a, Bits a) => [Word8] -> (a, [Word8])
parseVar = 
    go 
  where
    go []    = error "parseVar: unterminated variable length int"
    go (w:ws) 
      | w .&. 0x80 == 0 = (fromIntegral w, ws)
      | otherwise       = first add (go ws)
      where
        add x = (x `shiftL` 7) .|. (fromIntegral w .&. 0x7f)

{-

-- | The so-called \"zig-zag\" encoding from Google's protocol buffers.
-- It maps integers of small magnitude to naturals of small
-- magnitude by encoding negative integers as odd naturals and positive
-- integers as even naturals.
--
-- For example: @0 -> 0,  -1 -> 1, 1 -> 2, -2 -> 3, 2 -> 4, ...@
--
-- PRE: 'a' must be a signed integer type.
zigZag :: (Storable a, Bits a) => a -> a
zigZag x = (x `shiftL` 1) `xor` (x `shiftR` (8 * sizeOf x - 1))


-- | Reversing the zigZag encoding.
--
-- PRE: 'a' must be an unsigned integer type.
--
-- forall x. fromIntegral x == 
--           unZigZag ((fromIntegral :: IntX -> WordX) (zigZag x))
--
unZigZag :: (Storable a, Bits a) => a -> a
unZigZag x = (x `shiftR` 1) `xor` negate (x .&. 1)

unZigZagInt8 :: Int8 -> Int8
unZigZagInt8 = (fromIntegral :: Word8 -> Int8) . unZigZag . fromIntegral

unZigZagInt16 :: Int16 -> Int16
unZigZagInt16 = (fromIntegral :: Word16 -> Int16) . unZigZag . fromIntegral

unZigZagInt32 :: Int32 -> Int32
unZigZagInt32 = (fromIntegral :: Word32 -> Int32) . unZigZag . fromIntegral

unZigZagInt64 :: Int64 -> Int64
unZigZagInt64 = (fromIntegral :: Word64 -> Int64) . unZigZag . fromIntegral

unZigZagInt :: Int -> Int
unZigZagInt = (fromIntegral :: Word -> Int) . unZigZag . fromIntegral

-- | Check that the 'intVarSigned' encodings are parseable.
prop_zigZag_parseable :: (Arbitrary t, Bits b, Show t, Eq t) 
    => String -> (b -> t) -> BE.BoundedEncoding t -> Test
prop_zigZag_parseable name unZig be = 
  compareImpls name (\x -> (x, [])) (first unZig . parseVar . BE.evalB be)
   
-- | Variable length encoding to a fixed number of bytes (pad / truncate).
genVarFixedBound_list :: (Ord a, Num a, Bits a, Integral a) 
                 => Int 
                 -> a -> [Word8]
genVarFixedBound_list n x
  | n <= 1    = sevenBits            : []
  | otherwise = (sevenBits .|. 0x80) : genVarFixedBound_list (n - 1) (x `shiftR` 7)
  where
    sevenBits = fromIntegral x .&. 0x7f

wordVarFixedBound_list :: Word -> Word -> [Word8]
wordVarFixedBound_list bound = genVarFixedBound_list (length $ genVar_list bound)

word64VarFixedBound_list :: Word64 -> Word64 -> [Word8]
word64VarFixedBound_list bound = genVarFixedBound_list (length $ genVar_list bound)

-- Somehow this function doesn't really make sense, as the bound must be
-- greater when interpreted as an unsigned integer.
--
-- intVarFixedBound_list :: Int -> Int -> [Word8]
-- intVarFixedBound_list bound = wordVarFixedBound_list (fromIntegral bound) . fromIntegral
-}



------------------------------------------------------------------------------
-- Creating Builders from basic encodings
------------------------------------------------------------------------------

main = defaultMain testsChunked

testsChunked :: [Test]
testsChunked =
  [ test_encodeChunked ]

test_encodeChunked :: Test
test_encodeChunked = 
   testProperty "encodeChunked . stringUtf8" $ \cs ->
        (testBuilder id cs) == (parseChunks parseVar $ testBuilder encodeVar cs)

encodeVar :: Builder -> Builder
encodeVar = 
    (`mappend` BE.encodeWithF BE.word8 0)
  . (BE.encodeChunked 1 BE.word64VarFixedBound BE.emptyB)

encodeHex :: Builder -> Builder
encodeHex = 
    (`mappend` BE.encodeWithF (hexLen 0) 0) 
  . (BE.encodeChunked 1 hexLen BE.emptyB)

hexLen :: Word64 -> BE.FixedEncoding Word64
hexLen bound = 
  (\x -> (x, ' ')) BE.>$< (BE.word64HexFixedBound '0' bound BE.>*< BE.char8)

prefixHexSize :: Builder -> Builder
prefixHexSize =
    encodeWithSize hexLen

testBuilder f cs = 
    -- toLazyByteStringWith strategy L.empty $ f $ stringUtf8 cs
    toLazyByteString $ f $ stringUtf8 cs
  where
    bufSize      = length cs `div` 5
    firstBufSize = bufSize `div` 3
    strategy     = safeStrategy firstBufSize bufSize

parseHexLen :: [Word8] -> (Int, [Word8])
parseHexLen ws = case span (/= 32) ws of
  (lenWS, 32:ws') -> case readHex (map (chr . fromIntegral) lenWS) of
    [(len, [])] -> (len, ws')
    _          -> error $ "hex parse failed: " ++ show ws
  (_,   _) -> error $ "unterminated hex-length:" ++ show ws

parseChunks :: ([Word8] -> (Int, [Word8])) -> L.ByteString -> L.ByteString
parseChunks parseLen =
    L.pack . go . L.unpack
  where
    go ws
      | chunkLen == 0 && null ws' = []
      | chunkLen == 0             = error $ "trailing bytes: " ++ show ws
      | chunkLen <= length ws'    = chunk ++ go rest
      | otherwise                 = error $ "too few bytes: " ++ show ws
      where
        (chunkLen, ws') = parseLen ws
        (chunk, rest)   = splitAt chunkLen ws'




str = "hello world! This is an encoding test <<?-!-?>> *arrh*"
        
test1 = testBuilder id str
test2 = testBuilder encodeVar str
test3 = testBuilder encodeHex str
test4 = (test3, parseChunks parseHexLen test3)
test5 = testBuilder prefixHexSize "1"
         

------------------------------------------------------------------------------
-- Testing it
------------------------------------------------------------------------------


{-# INLINE encodeWithSize #-}
encodeWithSize
    :: (Word64 -> FixedEncoding Word64)   
    -- ^ Given a bound on the maximal size to encode, this function must return
    -- a fixed-size encoding for all smaller sizes.
    -> Builder
    -- ^ 'Put' to prefix with the length of its sequence of bytes.
    -> Builder
encodeWithSize mkSizeFE = 
    fromPut . putWithSize mkSizeFE . putBuilder

-- | Prefix a 'Put' with the size of its written data.
{-# INLINE putWithSize #-}
putWithSize 
    :: forall a.
       (Word64 -> FixedEncoding Word64)   
    -- ^ Encoding the size for the fallback case.
    -> Put a
    -- ^ 'Put' to prefix with the length of its sequence of bytes.
    -> Put a
putWithSize mkSizeFE innerP =
    put $ encodingStep
  where
    -- | The minimal free-size must be at least the small chunk-size to ensure
    -- that the slow-mode inserts large enough chunks.
    minFree = L.smallChunkSize

    encodingStep :: (forall r. (a -> BuildStep r) -> BuildStep r)
    encodingStep k = 
        fill (runPut innerP)
      where
        fill :: BuildStep a -> BufferRange -> IO (BuildSignal r)
        fill innerStep !br@(BufferRange op ope)
          | outRemaining < minBufferSize = 
              return $! bufferFull minBufferSize op (fill innerStep)
          | otherwise = do
              fillWithBuildStep innerStep doneH fullH insertChunksH brInner
          where
            outRemaining  = ope `minusPtr` op
            sizeFE        = mkSizeFE $ fromIntegral outRemaining
            reserved      = size sizeFE
            minBufferSize = minFree + reserved
           
            opInner       = op  `plusPtr` reserved
            brInner       = BufferRange opInner ope

            fastPrefixSize :: Ptr Word8 -> IO (Ptr Word8)
            fastPrefixSize !opInner'
              | innerSize == 0 = do runB (toB $ mkSizeFE 0) 0         op
              | otherwise      = do runB (toB $ sizeFE)     innerSize op
              where
                innerSize = fromIntegral $ opInner' `minusPtr` opInner

            slowPrefixSize :: Ptr Word8 -> Builder -> BuildStep a -> IO (BuildSignal r)
            slowPrefixSize opInner' replaySignal nextStep = do
                (x, lbsC, l) <- toLBS $ runBuilderWith bInner nextStep
                let lU = fromIntegral l
                -- TODO: We could get rid of one more chunk-boundary
                -- when encoding large messages by reserving space for the 
                -- size and modifying the returned bytestring afterwards.
                runBuilderWith (BE.encodeWithF (mkSizeFE lU) lU `mappend` 
                                -- inserting the continuation is crucial to
                                -- avoid quadratic runtime when nesting
                                -- 'encodeWithSize'.
                                lazyByteStringC l lbsC)
                               (k x) br
              where
                toLBS  = runCIOSWithLength <=< buildStepToCIOSUntrimmed
                -- Note that there's no 'unsafePerformIO' involved at this
                -- level. This is crucial to ensure a total ordering of all
                -- actions and, therefore, the execution of the 'bytesCopy'
                -- builder before returning a signal to the driver.
                bInner = bytesCopy (BufferRange opInner opInner') `mappend` 
                         replaySignal

            doneH :: Ptr Word8 -> a -> IO (BuildSignal r)
            doneH opInner' x = do
                op' <- fastPrefixSize opInner'
                let !br' = BufferRange op' ope
                k x br'

            fullH :: Ptr Word8 -> Int -> BuildStep a -> IO (BuildSignal r)
            fullH opInner' minSize nextInnerStep =
                slowPrefixSize opInner' (ensureFree minSize) nextInnerStep

            insertChunksH :: Ptr Word8 -> Int64 -> LazyByteStringC 
                          -> BuildStep a -> IO (BuildSignal r)
            insertChunksH opInner' n lbsC nextInnerStep =
                slowPrefixSize opInner' (lazyByteStringC n lbsC) nextInnerStep


-- | Run a 'ChunkIOStream' and gather its results and their length.
runCIOSWithLength :: ChunkIOStream a -> IO (a, LazyByteStringC, Int64)
runCIOSWithLength = 
    go 0 id
  where
    go !l lbsC (Finished x)        = return (x, lbsC, l)
    go !l lbsC (YieldC n lbsC' io) = io >>= go (l + n) (lbsC . lbsC')
    go !l lbsC (Yield1 bs io)      = 
        io >>= go (l + fromIntegral (S.length bs)) (lbsC . L.Chunk bs)

