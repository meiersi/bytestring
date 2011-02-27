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

  -- * Put
  , Put
  , putBuilder
  , putBuildStepCont
  , putLiftIO
  , runPut

  -- * Execution
  , toLazyByteString
  , toLazyByteStringWith
  -- , toByteString
  -- , toByteStringIO
  -- , toByteStringIOWith

  -- * Deafult Sizes
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

instance Monoid Builder where
  mempty = Builder id
  {-# INLINE mempty #-}
  (Builder b1) `mappend` (Builder b2) = Builder $ b1 . b2
  {-# INLINE mappend #-}
  mconcat = foldr mappend mempty
  {-# INLINE mconcat #-}

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
                    
-- | Prepend the chunk if it is non-empty.
{-# INLINE nonEmptyChunk #-}
nonEmptyChunk :: S.ByteString -> L.ByteString -> L.ByteString
nonEmptyChunk bs lbs | S.null bs = lbs 
                     | otherwise = L.Chunk bs lbs


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
toLazyByteStringWith bufSize minBufSize firstBufSize (Builder b) k = 
    S.inlinePerformIO $ fillFirstBuffer (b (buildStep finalStep))
  where
    finalStep (BufRange pf _) = return $ Done pf ()
    -- fill a first very small buffer, if we need more space then copy it
    -- to the new buffer of size 'minBufSize'. This way we don't pay the
    -- allocation cost of the big 'bufSize' buffer, when outputting only
    -- small sequences.
    fillFirstBuffer !step0
      | minBufSize <= firstBufSize = fillNewBuffer firstBufSize step0
      | otherwise                  = do
          fpbuf <- S.mallocByteString firstBufSize
          withForeignPtr fpbuf $ \pf -> do
              let !pe      = pf `plusPtr` firstBufSize
                  mkbs pf' = S.PS fpbuf 0 (pf' `minusPtr` pf)
                  {-# INLINE mkbs #-}
              next <- runBuildStep step0 (BufRange pf pe)
              case next of
                  Done pf' _
                    | pf' == pf -> return k
                    | otherwise -> return $ L.Chunk (mkbs pf') k

                  BufferFull newSize pf' nextStep  -> do
                      let !l  = pf' `minusPtr` pf
                      fillNewBuffer (max (l + newSize) minBufSize) $ buildStep $
                          \(BufRange pfNew peNew) -> do
                              copyBytes pfNew pf l
                              let !br' = BufRange (pfNew `plusPtr` l) peNew
                              runBuildStep nextStep br'
                      
                  InsertByteString pf' bs nextStep 
                      | pf' == pf ->
                          return $ nonEmptyChunk bs (S.inlinePerformIO $ fillNewBuffer bufSize nextStep)
                      | otherwise ->
                          return $ L.Chunk (mkbs pf')
                              (nonEmptyChunk bs (S.inlinePerformIO $ fillNewBuffer bufSize nextStep))
                    
    -- allocate and fill a new buffer
    fillNewBuffer !size !step0 = do
        fpbuf <- S.mallocByteString size
        withForeignPtr fpbuf $ fillBuffer fpbuf
      where
        fillBuffer fpbuf !pbuf = fill pbuf step0
          where
            !pe = pbuf `plusPtr` size
            fill !pf !step = do
                next <- runBuildStep step (BufRange pf pe)
                let mkbs pf' = S.PS fpbuf (pf `minusPtr` pbuf) (pf' `minusPtr` pf)
                    {-# INLINE mkbs #-}
                case next of
                    Done pf' _
                      | pf' == pf -> return k
                      | otherwise -> return $ L.Chunk (mkbs pf') k

                    BufferFull newSize pf' nextStep 
                      | pf' == pf -> 
                          fillNewBuffer (max newSize bufSize) nextStep
                      | otherwise -> 
                          return $ L.Chunk (mkbs pf')
                              (S.inlinePerformIO $ 
                                  fillNewBuffer (max newSize bufSize) nextStep)
                        
                    InsertByteString  pf' bs nextStep
                      | pf' == pf                      ->
                          return $ nonEmptyChunk bs (S.inlinePerformIO $ fill pf' nextStep)
                      | minBufSize < pe `minusPtr` pf' ->
                          return $ L.Chunk (mkbs pf')
                              (nonEmptyChunk bs (S.inlinePerformIO $ fill pf' nextStep))
                      | otherwise                      ->
                          return $ L.Chunk (mkbs pf')
                              (nonEmptyChunk bs (S.inlinePerformIO $ fillNewBuffer bufSize nextStep))


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
toLazyByteString b = toLazyByteStringWith 
    defaultBufferSize defaultMinimalBufferSize defaultFirstBufferSize b L.Empty
{-# INLINE toLazyByteString #-}

{- TODO: Move to Data.ByteString.Lazy
 
-- | Pack the chunks of a lazy bytestring into a single strict bytestring.
packChunks :: L.ByteString -> S.ByteString
packChunks lbs = do
    S.unsafeCreate (fromIntegral $ L.length lbs) (copyChunks lbs)
  where
    copyChunks !L.Empty                         !_pf = return ()
    copyChunks !(L.Chunk (S.PS fpbuf o l) lbs') !pf  = do
        withForeignPtr fpbuf $ \pbuf ->
            copyBytes pf (pbuf `plusPtr` o) l
        copyChunks lbs' (pf `plusPtr` l)

-}

{- TODO: Think if we should really provide this by default. In most cases
 - performance will not meet the expectations of the user.
 
-- | Run the builder to construct a strict bytestring containing the sequence
-- of bytes denoted by the builder. This is done by first serializing to a lazy bytestring and then packing its
-- chunks to a appropriately sized strict bytestring.
--
-- > toByteString = packChunks . toLazyByteString
--
-- Note that @'toByteString'@ is a 'Monoid' homomorphism.
--
-- > toByteString mempty          == mempty
-- > toByteString (x `mappend` y) == toByteString x `mappend` toByteString y
--
-- However, in the second equation, the left-hand-side is generally faster to
-- execute.
--
toByteString :: Builder -> S.ByteString
toByteString = packChunks . toLazyByteString
-}


{- TODO: Replace with 'hPut' method for Builders.
  
-- | @toByteStringIOWith bufSize io b@ runs the builder @b@ with a buffer of
-- at least the size @bufSize@ and executes the 'IO' action @io@ whenever the
-- buffer is full.
--
-- Compared to 'toLazyByteStringWith' this function requires less allocation,
-- as the output buffer is only allocated once at the start of the
-- serialization and whenever something bigger than the current buffer size has
-- to be copied into the buffer, which should happen very seldomly for the
-- default buffer size of 32kb. Hence, the pressure on the garbage collector is
-- reduced, which can be an advantage when building long sequences of bytes.
--
toByteStringIOWith :: Int                      -- ^ Buffer size (upper bounds
                                               -- the number of bytes forced
                                               -- per call to the 'IO' action).
                   -> (S.ByteString -> IO ())  -- ^ 'IO' action to execute per
                                               -- full buffer, which is
                                               -- referenced by a strict
                                               -- 'S.ByteString'.
                   -> Builder                  -- ^ 'Builder' to run.
                   -> IO ()                    -- ^ Resulting 'IO' action.
toByteStringIOWith bufSize io (Builder b) = 
    fillBuffer bufSize (b (buildStep finalStep))
  where
    finalStep !(BufRange pf _) = return $ Done pf ()

    fillBuffer !size step = do
        S.mallocByteString size >>= fill
      where
        fill fpbuf = do
            let !pf = unsafeForeignPtrToPtr fpbuf
                !br = BufRange pf (pf `plusPtr` size)
                -- safe due to later reference of fpbuf
                -- BETTER than withForeignPtr, as we lose a tail call otherwise
            signal <- runBuildStep step br
            case signal of
                Done pf' _ -> io $ S.PS fpbuf 0 (pf' `minusPtr` pf)

                BufferFull minSize pf' nextStep  -> do
                    io $ S.PS fpbuf 0 (pf' `minusPtr` pf)
                    fillBuffer (max bufSize minSize) nextStep
                    
                InsertByteString pf' bs nextStep  -> do
                    io $ S.PS fpbuf 0 (pf' `minusPtr` pf)
                    unless (S.null bs) (io bs)
                    fillBuffer bufSize nextStep

-- | Run the builder with a 'defaultBufferSize'd buffer and execute the given
-- 'IO' action whenever the buffer is full or gets flushed.
--
-- @ 'toByteStringIO' = 'toByteStringIOWith' 'defaultBufferSize'@
--
-- This is a 'Monoid' homomorphism in the following sense.
--
-- > toByteStringIO io mempty          == return ()
-- > toByteStringIO io (x `mappend` y) == toByteStringIO io x >> toByteStringIO io y
--
toByteStringIO :: (S.ByteString -> IO ()) -> Builder -> IO ()
toByteStringIO = toByteStringIOWith defaultBufferSize
{-# INLINE toByteStringIO #-}

-}

------------------------------------------------------------------------------
-- Draft of new builder/put execution code
------------------------------------------------------------------------------

{- FIXME: Generalize this code such that it can replace the above clunky
 - implementations.
              
-- | A monad for lazily composing lazy bytestrings using continuations.
newtype LBSM a = LBSM { unLBSM :: (a, L.ByteString -> L.ByteString) }

instance Monad LBSM where
    return x                       = LBSM (x, id)
    (LBSM (x,k)) >>= f             = let LBSM (x',k') = f x in LBSM (x', k . k')
    (LBSM (_,k)) >> (LBSM (x',k')) = LBSM (x', k . k')

-- | Execute a put and return the written buffers as the chunks of a lazy
-- bytestring.
toLazyByteString :: Put a -> (a, L.ByteString)
toLazyByteString put = 
    (fst result, k (bufToLBSCont (snd result) L.empty))
  where

    -- FIXME: Check with ByteString guys why allocation in inlinePerformIO is
    -- bad.

    -- initial buffer
    buf0 = S.inlinePerformIO $ allocBuffer defaultBufferSize
    -- run put, but don't force result => we're lazy enough
    LBSM (result, k) = runPut liftIO outputBuf outputBS put buf0
    -- convert a buffer to a lazy bytestring continuation
    bufToLBSCont = maybe id L.Chunk . unsafeFreezeNonEmptyBuffer
    -- lifting an io putsignal to a lazy bytestring monad
    liftIO io = LBSM (S.inlinePerformIO io, id)
    -- add buffer as a chunk prepare allocation of new one
    outputBuf minSize buf = LBSM
        ( S.inlinePerformIO $ allocBuffer (max minSize defaultBufferSize)
        , bufToLBSCont buf )
    -- add bytestring directly as a chunk; exploits postcondition of runPut
    -- that bytestrings are non-empty
    outputBS bs = LBSM ((), L.Chunk bs)
-}

{-
-- | A Builder that traces a message
traceBuilder :: String -> Builder 
traceBuilder msg = fromBuildStepCont $ \k br@(BufRange op ope) -> do
    putStrLn $ "traceBuilder " ++ show (op, ope) ++ ": " ++ msg
    k br

test2 :: Word8 -> [S.ByteString]
test2 x = L.toChunks $ toLazyByteString2 $ fromBuilder $ mconcat
  [ traceBuilder "before flush" 
  , fromWord8 48
  , flushBuilder
  , flushBuilder
  , traceBuilder "after flush"
  , fromWord8 x
  ]

-}
------------------------------------------------------------------------------
-- Executing puts on a buffer
------------------------------------------------------------------------------

{-
-- | Execute a build step on the given buffer.
{-# INLINE execBuildStep #-}
execBuildStep :: BuildStep a
              -> Buffer  
              -> IO (BuildSignal a)
execBuildStep step (Buffer _ _ op ope) = runBuildStep step (BufRange op ope)
-}

-- | Execute a put on a buffer.
--
-- TODO: Generalize over buffer allocation strategy.
{-# INLINE runPut #-}
runPut :: Monad m 
       => (IO (BuildSignal a) -> m (BuildSignal a)) -- lifting of buildsteps
       -> (Int -> Buffer -> m Buffer) -- output function for a guaranteedly non-empty buffer, the returned buffer will be filled next
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
            Done op' x ->         -- put completed, buffer partially runSteped
                return (x, Buffer fpbuf p0 op' ope)

            BufferFull minSize op' nextStep -> do
                buf' <- outputBuf minSize (Buffer fpbuf p0 op' ope)
                runStep nextStep buf'

            InsertByteString op' bs nextStep
              | S.null bs ->   -- flushing of buffer required
                  outputBuf 1 (Buffer fpbuf p0 op' ope) >>= runStep nextStep
              | p0 == op' -> do -- no bytes written: just insert bytestring
                  outputBS bs
                  runStep nextStep buf
              | otherwise -> do   -- bytes written, insert buffer and bytestring
                  buf' <- outputBuf 1 (Buffer fpbuf p0 op' ope)
                  outputBS bs
                  runStep nextStep buf'
