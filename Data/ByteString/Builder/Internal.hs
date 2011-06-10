{-# LANGUAGE CPP, BangPatterns, Rank2Types, MonoPatBinds #-}
-- |
-- Copyright   : (c) 2010 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Core types and functions for the 'Builder' monoid and the 'Put' monad.
--
module Data.ByteString.Builder.Internal (

  -- * Build Steps
    BufRange(..)
  , BuildSignal
  , BuildStep
  , done
  , bufferFull
  , insertByteString

  -- * Builder
  , Builder
  , fromBuildStepCont
  , fromPut
  , flush
  , empty
  , append
  , unfusableAppend

  -- * Put
  , Put
  , putBuilder
  , putBuildStepCont
  , putLiftIO
  , runPut

  -- * Execution
  , toLazyByteString
  , toLazyByteStringUntrimmed
  , toLazyByteStringWith

  -- * Default Sizes
  , defaultFirstBufferSize
  , defaultMinimalBufferSize
  , defaultBufferSize
  , defaultMaximalCopySize 
) where

#ifdef APPLICATIVE_IN_BASE
import Control.Applicative
#endif

import Data.Monoid
import qualified Data.ByteString               as S
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy.Internal as L

import Data.ByteString.Builder.Internal.Buffer

import Foreign

import System.IO


------------------------------------------------------------------------------
-- The core: BuildSteps
------------------------------------------------------------------------------

data BufRange = BufRange {-# UNPACK #-} !(Ptr Word8) {-# UNPACK #-} !(Ptr Word8)

data BuildSignal a =
    Done {-# UNPACK #-} !(Ptr Word8) a
  | BufferFull
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !(Ptr Word8)
                     !(BuildStep a)
  | InsertByteString
      {-# UNPACK #-} !(Ptr Word8) 
                     !S.ByteString
                     !(BuildStep a)

newtype BuildStep a =  
    BuildStep { runBuildStep :: BufRange -> IO (BuildSignal a) }

-- Hiding the implementation of 'BuildStep's

done :: Ptr Word8 -> a -> BuildSignal a
done = Done

bufferFull :: Int -> Ptr Word8 -> (BufRange -> IO (BuildSignal a)) -> BuildSignal a
bufferFull size op step = BufferFull size op (buildStep step)

insertByteString :: Ptr Word8 -> S.ByteString -> (BufRange -> IO (BuildSignal a)) -> BuildSignal a
insertByteString op bs step = InsertByteString op bs (buildStep step)

buildStep :: (BufRange -> IO (BuildSignal a)) -> BuildStep a
buildStep = BuildStep

------------------------------------------------------------------------------
-- The 'Builder' Monoid and the 'Put' Monad
------------------------------------------------------------------------------

newtype Builder = Builder (forall r. BuildStep r -> BuildStep r)

{-# INLINE[1] empty #-}
empty :: Builder
empty = Builder id

-- Retain the append markers in phase 0 such that the boundary checks of
-- fromWrite can be fused. The unfusableAppend is just the append that
-- no rule matches on.
{-# INLINE[1] append #-}
append :: Builder -> Builder -> Builder
append = unfusableAppend

{-# INLINE unfusableAppend #-}
unfusableAppend :: Builder -> Builder -> Builder
unfusableAppend (Builder b1) (Builder b2) = Builder $ b1 . b2

instance Monoid Builder where
  {-# INLINE mempty #-}
  mempty = empty
  {-# INLINE mappend #-}
  mappend = append
  {-# INLINE mconcat #-}
  mconcat = foldr mappend mempty

newtype Put a = Put {
    unPut :: forall r. (a -> BuildStep r) -> BuildStep r
  }

instance Functor Put where
  fmap f (Put put) = Put $ \k -> put (\x -> k (f x))
  {-# INLINE fmap #-}

#ifdef APPLICATIVE_IN_BASE
instance Applicative Put where
  pure x = Put $ \k -> k x
  {-# INLINE pure #-}
  f <*> a = Put $ \k -> unPut f (\f' -> unPut a (\a' -> k (f' a')))
  {-# INLINE (<*>) #-}
  a <* b = Put $ \k -> unPut a (\a' -> unPut b (\_ -> k a'))
  {-# INLINE (<*) #-}
  a *> b = Put $ \k -> unPut a (\_ -> unPut b k)
  {-# INLINE (*>) #-}
#endif

instance Monad Put where
  return x = Put $ \k -> k x
  {-# INLINE return #-}
  m >>= f  = Put $ \k -> unPut m (\m' -> unPut (f m') k)
  {-# INLINE (>>=) #-}
  m >>  n  = Put $ \k -> unPut m (\_ -> unPut n k)
  {-# INLINE (>>) #-}


-- Creation from concrete 'BuildStep's
------------------------------------------------------------------------------

putBuildStepCont :: (forall r. (a -> BufRange -> IO (BuildSignal r)) -> 
                               (     BufRange -> IO (BuildSignal r))
                    ) -> Put a
putBuildStepCont step = Put step'
  where
    step' k = BuildStep $ step (\x -> runBuildStep (k x))


fromBuildStepCont :: (forall r. (BufRange -> IO (BuildSignal r)) -> 
                                (BufRange -> IO (BuildSignal r))
                     ) -> Builder
fromBuildStepCont step = Builder step'
  where
    step' k = BuildStep $ step (runBuildStep k)



-- Conversion between Put and Builder
------------------------------------------------------------------------------

-- | Put the given builder.
putBuilder :: Builder -> Put ()
putBuilder (Builder build) = Put $ \k -> build (k ())


-- | Ignore the value of a put and only exploit its output side effect.
fromPut :: Put a -> Builder
fromPut (Put put) = Builder $ \k -> put (\_ -> k)

-- Lifting IO actions
---------------------

-- | Lift the given IO action.
{-# INLINE putLiftIO #-}
putLiftIO :: IO a -> Put a
putLiftIO io = putBuildStepCont $ \k br -> io >>= (`k` br)

------------------------------------------------------------------------------
-- Internal global constants.
------------------------------------------------------------------------------

-- | Default size (~32kb) for the buffer that becomes a chunk of the output
-- stream once it is filled.
--
-- TODO: Use constant from Data.ByteString.Lazy.Internal
--
defaultBufferSize :: Int
defaultBufferSize = 32 * 1024 - overhead -- Copied from Data.ByteString.Lazy.
    where overhead = 2 * sizeOf (undefined :: Int)

-- | The minimal length (~4kb) a buffer must have before filling it and
-- outputting it as a chunk of the output stream. 
--
-- This size determines when a buffer is spilled after a 'flush' or a direct
-- bytestring insertion. It is also the size of the first chunk generated by
-- 'toLazyByteString'.
defaultMinimalBufferSize :: Int
defaultMinimalBufferSize = 4 * 1024 - overhead
    where overhead = 2 * sizeOf (undefined :: Int)

-- | The default length (64) for the first buffer to be allocated when
-- converting a 'Builder' to a lazy bytestring. 
--
-- See 'toLazyByteStringWith' for further explanation.
defaultFirstBufferSize :: Int
defaultFirstBufferSize = 64

-- | The maximal number of bytes for that copying is cheaper than direct
-- insertion into the output stream. This takes into account the fragmentation
-- that may occur in the output buffer due to the early 'flush' implied by the
-- direct bytestring insertion.
--
-- @'defaultMaximalCopySize' = 2 * 'defaultMinimalBufferSize'@
--
defaultMaximalCopySize :: Int
defaultMaximalCopySize = 2 * defaultMinimalBufferSize

------------------------------------------------------------------------------
-- Flushing and running a Builder
------------------------------------------------------------------------------
                    

-- | Output all data written in the current buffer and start a new chunk.
--
-- The use uf this function depends on how the resulting bytestrings are
-- consumed. 'flush' is possibly not very useful in non-interactive scenarios.
-- However, it is kept for compatibility with the builder provided by
-- Data.Binary.Builder.
--
-- When using 'toLazyByteString' to extract a lazy 'L.ByteString' from a
-- 'Builder', this means that a new chunk will be started in the resulting lazy
-- 'L.ByteString'. The remaining part of the buffer is spilled, if the
-- reamining free space is smaller than the minimal desired buffer size.
--
{-# INLINE flush #-}
flush :: Builder
flush = fromBuildStepCont step
  where
    step k !(BufRange op _) = return $ insertByteString op S.empty k



------------------------------------------------------------------------------
-- Executing puts on a buffer
------------------------------------------------------------------------------

-- TODO: Use the handle's associated buffer if possible!
hPutPut :: Handle -> Put a -> IO a
hPutPut h put = do
    firstBuf      <- allocBuffer defaultMinimalBufferSize
    (x, finalBuf) <- runPut id outputBuf (S.hPut h) put firstBuf
    S.hPut h $ unsafeFreezeBuffer finalBuf
    return x
  where
    outputBuf minSize buf = do
        S.hPut h $ unsafeFreezeBuffer buf
        allocBuffer (max minSize defaultBufferSize)

hPutBuilder :: Handle -> Builder -> IO ()
hPutBuilder h = hPutPut h . putBuilder

-- | Execute a put on a buffer.
--
{-# INLINE runPut #-}
runPut :: Monad m 
       => (IO (BuildSignal a) -> m (BuildSignal a)) -- lifting of buildsteps
       -> (Int -> Buffer -> m Buffer) -- output function for a buffer, the returned buffer will be filled next
       -> (S.ByteString -> m ())    -- output function for guaranteedly non-empty bytestrings, that are inserted directly into the stream
       -> Put a                     -- put to execute
       -> Buffer                    -- initial buffer to be used
       -> m (a, Buffer)             -- result of put and remaining buffer
runPut liftIO outputBuf outputBS (Put put) =
    runStep (put (finalStep))
  where
    finalStep x = buildStep $ \(BufRange op _) -> return $ Done op x

    runStep step buf@(Buffer fpbuf p0 op ope) = do
        let !br = BufRange op ope
        signal <- liftIO $ runBuildStep step br
        case signal of 
            Done op' x ->         -- put completed, buffer partially filled
                return (x, Buffer fpbuf p0 op' ope)

            BufferFull minSize op' nextStep -> do
                buf' <- outputBuf minSize (Buffer fpbuf p0 op' ope)
                runStep nextStep buf'

            InsertByteString op' bs nextStep
              | S.null bs ->    -- flushing of buffer required
                  outputBuf 1 (Buffer fpbuf p0 op' ope) >>= runStep nextStep
              | p0 == op' -> do -- no bytes written: just insert bytestring
                  outputBS bs
                  runStep nextStep buf
              | otherwise -> do   -- bytes written, insert buffer and bytestring
                  buf' <- outputBuf 1 (Buffer fpbuf p0 op' ope)
                  outputBS bs
                  runStep nextStep buf'


------------------------------------------------------------------------------
-- To Lazy ByteString with allocation strategy
------------------------------------------------------------------------------

data AllocationStrategy = AllocationStrategy 
         {-# UNPACK #-} !Int  -- size of first buffer
         {-# UNPACK #-} !Int  -- size of successive buffers
         (Int -> Int -> Bool) -- trim

untrimmedStrategy :: Int -- ^ Size of the first buffer
                  -> Int -- ^ Size of successive buffers
                  -> AllocationStrategy
untrimmedStrategy firstSize bufSize = 
    AllocationStrategy firstSize bufSize (\_ _ -> False)


safeStrategy :: Int  -- ^ Size of first buffer
             -> Int  -- ^ Size of successive buffers
             -> AllocationStrategy
safeStrategy firstSize bufSize = 
    AllocationStrategy firstSize bufSize (\used size -> 2*used < size)

-- | Extract the lazy 'L.ByteString' from the builder by running it with default
-- buffer sizes. Use this function, if you do not have any special
-- considerations with respect to buffer sizes.
--
-- @ 'toLazyByteString' b = 'toLazyByteStringWith' 'defaultBufferSize' 'defaultMinimalBufferSize' 'defaultFirstBufferSize' b L.empty@
--
-- Note that @'toLazyByteString'@ is a 'Monoid' homomorphism.
--
-- > toLazyByteString mempty          == mempty
-- > toLazyByteString (x `mappend` y) == toLazyByteString x `mappend` toLazyByteString y
--
-- However, in the second equation, the left-hand-side is generally faster to
-- execute.
--
toLazyByteString :: Builder -> L.ByteString
toLazyByteString = toLazyByteStringWith
    (safeStrategy defaultMinimalBufferSize defaultBufferSize) L.Empty

toLazyByteStringUntrimmed :: Builder -> L.ByteString
toLazyByteStringUntrimmed = toLazyByteStringWith
    (untrimmedStrategy defaultMinimalBufferSize defaultBufferSize) L.Empty

{-# INLINE toLazyByteStringWith #-}
toLazyByteStringWith :: AllocationStrategy
                      -> L.ByteString 
                      -> Builder 
                      -> L.ByteString
toLazyByteStringWith (AllocationStrategy firstSize bufSize trim) k (Builder b) = 
    S.inlinePerformIO $ fillNew (b (buildStep finalStep)) firstSize 
  where
    finalStep (BufRange op _) = return $ Done op ()
                    
    fillNew !step0 !size = do
        S.mallocByteString size >>= fill step0
      where
        fill !step !fpbuf = do
            let op     = unsafeForeignPtrToPtr fpbuf -- safe due to mkbs
                pe     = op `plusPtr` size
                !br    = BufRange op pe
                
                mkbs !op' lbs
                  | trim filledSize size = do
                      fpbuf' <- S.mallocByteString filledSize
                      copyBytes (unsafeForeignPtrToPtr fpbuf') op filledSize
                      touchForeignPtr fpbuf
                      return $ L.Chunk (S.PS fpbuf' 0 filledSize) lbs
                  | otherwise                     = 
                      return $ L.Chunk (S.PS fpbuf 0 filledSize) lbs
                  where
                    filledSize = op' `minusPtr` op

            next <- runBuildStep step br
            case next of
                Done op' _
                  | op' == op -> return k
                  | otherwise -> mkbs op' k

                BufferFull minSize op' nextStep 
                  | op' == op -> fillNew nextStep (max minSize bufSize)

                  | otherwise -> 
                      mkbs op' $ S.inlinePerformIO
                               $ fillNew nextStep (max minSize bufSize)
                    
                InsertByteString op' bs nextStep
                  | op' == op ->
                      return $ nonEmptyChunk bs 
                             $ S.inlinePerformIO 
                             $ fill nextStep fpbuf

                  | otherwise ->
                      mkbs op' $ nonEmptyChunk bs 
                               $ S.inlinePerformIO 
                               $ fillNew nextStep bufSize

-- | Prepend the chunk if it is non-empty.
{-# INLINE nonEmptyChunk #-}
nonEmptyChunk :: S.ByteString -> L.ByteString -> L.ByteString
nonEmptyChunk bs lbs | S.null bs = lbs 
                     | otherwise = L.Chunk bs lbs
    -- fill a first very small buffer, if we need more space then copy it
    -- to the new buffer of size 'minBufSize'. This way we don't pay the
    -- allocation cost of the big 'bufSize' buffer, when outputting only
    -- small sequences.
    --
-- | Run a 'Builder' with the given buffer sizes.
--
-- Use this function for integrating the 'Builder' type with other libraries
-- that generate lazy bytestrings.
--
-- Note that the builders should guarantee that on average the desired chunk
-- size is attained. Builders may decide to start a new buffer and not
-- completely fill the existing buffer, if this is faster. However, they should
-- not spill too much of the buffer, if they cannot compensate for it.
--
-- A call @toLazyByteStringWith bufSize minBufSize firstBufSize@ will generate
-- a lazy bytestring according to the following strategy. First, we allocate
-- a buffer of size @firstBufSize@ and start filling it. If it overflows, we
-- allocate a buffer of size @minBufSize@ and copy the first buffer to it in
-- order to avoid generating a too small chunk. Finally, every next buffer will
-- be of size @bufSize@. This, slow startup strategy is required to achieve
-- good speed for short (<200 bytes) resulting bytestrings, as for them the
-- allocation cost is of a large buffer cannot be compensated. Moreover, this
-- strategy also allows us to avoid spilling too much memory for short
-- resulting bytestrings.
--
-- Note that setting @firstBufSize >= minBufSize@ implies that the first buffer
-- is no longer copied but allocated and filled directly. Hence, setting
-- @firstBufSize = bufSize@ means that all chunks will use an underlying buffer
-- of size @bufSize@. This is recommended, if you know that you always output
-- more than @minBufSize@ bytes.
{-
toLazyByteStringWith 
    :: Int           -- ^ Buffer size (upper-bounds the resulting chunk size).
    -> Int           -- ^ Minimal free buffer space for continuing filling
                     -- the same buffer after a 'flush' or a direct bytestring
                     -- insertion. This corresponds to the minimal desired
                     -- chunk size.
    -> Int           -- ^ Size of the first buffer to be used and copied for
                     -- larger resulting sequences
    -> Builder       -- ^ Builder to run.
    -> L.ByteString  -- ^ Lazy bytestring to output after the builder is
                     -- finished.
    -> L.ByteString  -- ^ Resulting lazy bytestring
-}
