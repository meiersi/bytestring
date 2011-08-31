{-# LANGUAGE ScopedTypeVariables, CPP, BangPatterns, Rank2Types, MonoPatBinds #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Copyright   : (c) 2010 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Core types and functions for the 'Builder' monoid and its generalization,
-- the 'Put' monad.
--
-- The design of the 'Builder' monoid is optimized such that 
--
--   1. buffers of arbitrary size can be filled as efficiently as possible and
--
--   2. sequencing of 'Builder's is as cheap as possible.
--
-- We achieve (1) by completely handing over control over writing to the buffer
-- to the 'BuildStep' implementing the 'Builder'. This 'BuildStep' is just told
-- the start and the end of the buffer (represented as a 'BufferRange'). Then,
-- the 'BuildStep' can write to as big a prefix of this 'BufferRange' in any
-- way it desires. If the 'BuildStep' is done, the 'BufferRange' is full, or a
-- long sequence of bytes should be inserted directly, then the 'BuildStep'
-- signals this to its caller using a 'BuildSignal'. 
--
-- We achieve (2) by requiring that every 'Builder' is implemented by a
-- 'BuildStep' that takes a continuation 'BuildStep', which it calls with the
-- updated 'BufferRange' after it is done. Therefore, only two pointers have 
-- to be passed in a function call to implement concatentation of 'Builder's.
-- Moreover, many 'Builder's are completely inlined, which enables the compiler
-- to sequence them without a function call and with no boxing at all.
--
-- This design gives the implementation of a 'Builder' full access to the 'IO'
-- monad. Therefore, utmost care has to be taken to not overwrite anything
-- outside the given 'BufferRange's. Moreover, further care has to be taken to
-- ensure that 'Builder's and 'Put's are referentially transparent. See the
-- comments of the 'builder' and 'put' functions for further information.
-- Note that there are /no safety belts/ at all, when implementing a 'Builder'
-- using an 'IO' action: you are writing code that might enable the next
-- buffer-overlow attack on a Haskell server!
--
module Data.ByteString.Lazy.Builder.Internal (

  -- * Build signals and steps
    BufferRange(..)

  , BuildSignal
  , BuildStep

  , done
  , bufferFull
  , insertChunk

  , fillWithBuildStep

  -- * The Builder monoid
  , Builder
  , builder
  , runBuilder
  , runBuilderWith

  -- ** Primitive combinators
  , empty
  , append
  , flush

  , byteStringCopy
  , byteStringInsert
  , byteStringThreshold

  , lazyByteStringCopy
  , lazyByteStringInsert
  , lazyByteStringThreshold

  , maximalCopySize
  , byteString
  , lazyByteString

  -- ** Execution strategies
  , toLazyByteStringWith
  , AllocationStrategy
  , safeStrategy
  , untrimmedStrategy
  , L.smallChunkSize
  , L.defaultChunkSize

  -- * The Put monad
  , Put
  , put
  , runPut
  , hPut

  -- ** Conversion to and from Builders
  , putBuilder
  , fromPut

  -- ** Lifting IO actions
  , putLiftIO

) where

-- TODO: Check if we still require conditional compilation for Applicative

-- #ifdef APPLICATIVE_IN_BASE
import Control.Applicative (Applicative(..))
-- #endif
import Control.Applicative ((<$>))

import Data.Monoid
import qualified Data.ByteString               as S
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy.Internal as L

import GHC.IO.Buffer (Buffer(..), newByteBuffer)
import GHC.IO.Handle.Internals (wantWritableHandle, flushWriteBuffer)
import GHC.IO.Handle.Types (Handle__, haByteBuffer) 
import GHC.IORef

import Foreign

import System.IO (Handle)


-- | A range of bytes in a buffer represented by the pointer to the first byte
-- of the range and the pointer to the first byte /after/ the range.
data BufferRange = BufferRange {-# UNPACK #-} !(Ptr Word8)  -- First byte of range
                               {-# UNPACK #-} !(Ptr Word8)  -- First byte /after/ range


------------------------------------------------------------------------------
-- Build signals
------------------------------------------------------------------------------

type BuildStep a = BufferRange -> IO (BuildSignal a)

-- | 'BuildSignal's abstract signals to the caller of a 'BuildStep'. There are
-- exactly three signals: 'done', 'bufferFull', and 'insertChunk'.
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

-- | Signal that the current 'BuildStep' is done and has computed a value.
{-# INLINE done #-}
done :: Ptr Word8      -- ^ Next free byte in current 'BufferRange'
     -> a              -- ^ Computed value
     -> BuildSignal a
done = Done

-- | Signal that the current buffer is full.
{-# INLINE bufferFull #-}
bufferFull :: Int                              
           -- ^ Minimal size of next 'BufferRange'. 
           -> Ptr Word8                        
           -- ^ Next free byte in current 'BufferRange'. 
           -> BuildStep a
           -- ^ 'BuildStep' to run on the next 'BufferRange'. This 'BuildStep'
           -- may assume that it is called with a 'BufferRange' of at least the
           -- required minimal size; i.e., the caller of this 'BuildStep' must
           -- guarantee this.
           -> BuildSignal a
bufferFull = BufferFull

-- TODO: Decide whether we should inline the bytestring constructor.
-- Therefore, making builders independent of strict bytestrings.

-- | Signal that a chunk should be inserted directly.
{-# INLINE insertChunk #-}
insertChunk :: Ptr Word8                        
            -- ^ Next free byte in current 'BufferRange'
            -> S.ByteString                     
            -- ^ Chunk to insert
            -> BuildStep a
            -- ^ 'BuildStep' to run on next 'BufferRange'
            -> BuildSignal a
insertChunk = InsertByteString

-- | Fill a 'BufferRange' using a 'BuildStep'.
{-# INLINE fillWithBuildStep #-}
fillWithBuildStep 
    :: BuildStep a 
    -- ^ Build step to use for filling the 'BufferRange'.
    -> (Ptr Word8 -> a -> IO b) 
    -- ^ Handling the 'done' signal
    -> (Ptr Word8 -> Int -> BuildStep a -> IO b)
    -- ^ Handling the 'bufferFull' signal
    -> (Ptr Word8 -> S.ByteString -> BuildStep a -> IO b)
    -- ^ Handling the 'insertChunk' signal
    -> BufferRange 
    -- ^ Buffer range to fill.
    -> IO b
    -- ^ Value computed by filling this 'BufferRange'.
fillWithBuildStep step fDone fFull fChunk !br = do
    signal <- step br
    case signal of
        Done op x                       -> fDone op x
        BufferFull minSize op nextStep  -> fFull op minSize nextStep
        InsertByteString op bs nextStep -> fChunk op bs nextStep 
                  


------------------------------------------------------------------------------
-- The 'Builder' monoid
------------------------------------------------------------------------------

-- | A 'Builder' denotes a stream of bytes that can be efficiently converted to
-- a sequence of byte arrays. 'Builder's can be concatenated with /O(1)/ cost
-- using 'mappend'. The zero-length 'Builder' is denoted by 'mempty'.
newtype Builder = Builder (forall r. BuildStep r -> BuildStep r)

-- | Construct a 'Builder'. In contrast to 'BuildStep's, 'Builder's are
-- referentially transparent. 
{-# INLINE builder #-}
builder :: (forall r. BuildStep r -> BuildStep r)
        -- ^ A function that fills a 'BufferRange', calls the continuation with
        -- the updated 'BufferRange' once its done, and signals its caller how
        -- to proceed using 'done', 'bufferFull', or 'insertChunk'.
        --
        -- This function must be referentially transparent; i.e., calling it
        -- multiple times must result in the same sequence of bytes being
        -- written. If you need mutable state, then you must allocate it newly
        -- upon each call of this function. Moroever, this function must call
        -- the continuation once its done. Otherwise, concatenation of
        -- 'Builder's does not work. Finally, this function must write to all
        -- bytes that it claims it has written. Otherwise, the resulting
        -- 'Builder' is not guaranteed to be referentially transparent and
        -- sensitive data might leak.
        -> Builder
builder = Builder

-- | Run a 'Builder'.
{-# INLINE runBuilder #-}
runBuilder :: Builder      -- ^ 'Builder' to run
           -> BuildStep () -- ^ 'BuildStep' that writes the byte stream of this
                           -- 'Builder' and signals 'done' upon completion.
runBuilder (Builder b) = b $ \(BufferRange op _) -> return $ done op ()

-- | Run a 'Builder'.
{-# INLINE runBuilderWith #-}
runBuilderWith :: Builder      -- ^ 'Builder' to run
               -> BuildStep a -- ^ Continuation 'BuildStep'
               -> BuildStep a 
runBuilderWith (Builder b) = b

-- | The 'Builder' denoting a zero-length sequence of bytes. This function is
-- only exported for use in rewriting rules. Use 'mempty' otherwise.
{-# INLINE[1] empty #-}
empty :: Builder
empty = Builder id

-- | Concatenate two 'Builder's. This function is only exported for use in rewriting
-- rules. Use 'mappend' otherwise.
{-# INLINE[1] append #-}
append :: Builder -> Builder -> Builder
append (Builder b1) (Builder b2) = Builder $ b1 . b2

instance Monoid Builder where
  {-# INLINE mempty #-}
  mempty = empty
  {-# INLINE mappend #-}
  mappend = append
  {-# INLINE mconcat #-}
  mconcat = foldr mappend mempty

-- | Flush the current buffer and start a new chunk. 
--
{-# INLINE flush #-}
flush :: Builder
flush = builder step
  where
    step k !(BufferRange op _) = return $ insertChunk op S.empty k


------------------------------------------------------------------------------
-- Put
------------------------------------------------------------------------------

-- | A 'Put' action denotes a computation of a value that writes a stream of
-- bytes as a side-effect. 'Put's are strict in their side-effect; i.e., the
-- stream of bytes will always be written before the computed value is
-- returned.
--
-- 'Put's are a generalization of 'Builder's. They are used when values need to
-- be returned during the computation of a stream of bytes. For example, when
-- performing a block-based encoding of 'S.ByteString's like Base64 encoding,
-- there might be a left-over partial block. Using the 'Put' monad, this
-- partial block can be returned after the complete blocks have been encoded.
-- Then, in a later step when more input is known, this partial block can be
-- completed and also encoded.
--
-- @Put ()@ actions are isomorphic to 'Builder's. The functions 'putBuilder'
-- and 'fromPut' convert between these two types. Where possible, you should
-- use 'Builder's, as they are slightly cheaper than 'Put's because they do not
-- carry a computed value.
newtype Put a = Put { unPut :: forall r. (a -> BuildStep r) -> BuildStep r }

-- | Construct a 'Put' action. In contrast to 'BuildStep's, 'Put's are
-- referentially transparent in the sense that sequencing the same 'Put'
-- multiple times yields every time the same value with the same side-effect.
{-# INLINE put #-}
put :: (forall r. (a -> BuildStep r) -> BuildStep r)
       -- ^ A function that fills a 'BufferRange', calls the continuation with
       -- the updated 'BufferRange' and its computed value once its done, and
       -- signals its caller how to proceed using 'done', 'bufferFull', or
       -- 'insertChunk'.
       --
       -- This function must be referentially transparent; i.e., calling it
       -- multiple times must result in the same sequence of bytes being
       -- written and the same value being computed. If you need mutable state,
       -- then you must allocate it newly upon each call of this function.
       -- Moroever, this function must call the continuation once its done.
       -- Otherwise, monadic sequencing of 'Put's does not work. Finally, this
       -- function must write to all bytes that it claims it has written.
       -- Otherwise, the resulting 'Put' is not guaranteed to be referentially
       -- transparent and sensitive data might leak.
       -> Put a
put = Put 

-- | Run a 'Put'.
{-# INLINE runPut #-}
runPut :: Put a       -- ^ Put to run
       -> BuildStep a -- ^ 'BuildStep' that first writes the byte stream of
                      -- this 'Put' and then yields the computed value using
                      -- the 'done' signal.
runPut (Put p) = p $ \x (BufferRange op _) -> return $ Done op x

instance Functor Put where
  fmap f p = Put $ \k -> unPut p (\x -> k (f x))
  {-# INLINE fmap #-}

-- #ifdef APPLICATIVE_IN_BASE
instance Applicative Put where
  {-# INLINE pure #-}
  pure x = Put $ \k -> k x
  {-# INLINE (<*>) #-}
  Put f <*> Put a = Put $ \k -> f (\f' -> a (\a' -> k (f' a')))
  {-# INLINE (<*) #-}
  Put a <* Put b = Put $ \k -> a (\a' -> b (\_ -> k a'))
  {-# INLINE (*>) #-}
  Put a *> Put b = Put $ \k -> a (\_ -> b k)
-- #endif

instance Monad Put where
  {-# INLINE return #-}
  return x = Put $ \k -> k x
  {-# INLINE (>>=) #-}
  Put m >>= f = Put $ \k -> m (\m' -> unPut (f m') k)
  {-# INLINE (>>) #-}
  Put m >> Put n = Put $ \k -> m (\_ -> n k)


-- Conversion between Put and Builder
-------------------------------------

-- | Run a 'Builder' as a side-effect of a @Put ()@ action.
{-# INLINE putBuilder #-}
putBuilder :: Builder -> Put ()
putBuilder (Builder b) = Put $ \k -> b (k ())

-- | Convert a @Put ()@ action to a 'Builder'.
{-# INLINE fromPut #-}
fromPut :: Put () -> Builder
fromPut (Put p) = Builder $ \k -> p (\_ -> k)


-- Lifting IO actions
---------------------

-- | Lift an 'IO' action to a 'Put' action.
{-# INLINE putLiftIO #-}
putLiftIO :: IO a -> Put a
putLiftIO io = put $ \k br -> io >>= (`k` br)


------------------------------------------------------------------------------
-- Executing a Put directly on a buffered Handle
------------------------------------------------------------------------------

-- | Run a 'Put' action redirecting the produced output to a 'Handle'.
--
-- The output is buffered using the 'Handle's associated buffer. If this
-- buffer is too small to execute one step of the 'Put' action, then
-- it is replaced with a large enough buffer.
hPut :: forall a. Handle -> Put a -> IO a
hPut h b =
    fillHandle 1 (runPut b)
  where
    fillHandle :: Int -> BuildStep a -> IO a
    fillHandle !minFree step = do
        next <- wantWritableHandle "hPut" h fillHandle_
        next
      where
        -- | We need to return an inner IO action that is executed outside
        -- the lock taken on the Handle for two reasons:
        --
        --   1. GHC.IO.Handle.Internals mentions in "Note [async]" that
        --      we should never do any side-effecting operations before
        --      an interuptible operation that may raise an async. exception
        --      as long as we are inside 'wantWritableHandle' and the like.
        --      We possibly run the interuptible 'flushWriteBuffer' right at
        --      the start of 'fillHandle', hence entering it a second time is
        --      not safe, as it could lead to a 'BuildStep' being run twice.
        --
        --   2. We use the 'S.hPut' function to also write to the handle.
        --      This function tries to take the same lock taken by
        --      'wantWritableHandle'. Therefore, we cannot call 'S.hPut'
        --      inside 'wantWritableHandle'.
        --
        fillHandle_ :: Handle__ -> IO (IO a)
        fillHandle_ h_ = do
            makeSpace  =<< readIORef refBuf 
            fillBuffer =<< readIORef refBuf
          where
            refBuf        = haByteBuffer h_
            freeSpace buf = bufSize buf - bufR buf

            makeSpace buf
              | bufSize buf < minFree = do
                  flushWriteBuffer h_                   
                  s <- bufState <$> readIORef refBuf
                  newByteBuffer minFree s >>= writeIORef refBuf

              | freeSpace buf < minFree = flushWriteBuffer h_
              | otherwise               = return ()

            fillBuffer buf 
              | freeSpace buf < minFree = 
                  error $ unlines
                    [ "Data.ByteString.Lazy.Builder.Internal.hPut: internal error."
                    , "  Not enough space after flush."
                    , "    required: " ++ show minFree 
                    , "    free: "     ++ show (freeSpace buf)
                    ]
              | otherwise = do
                  let !br = BufferRange op (pBuf `plusPtr` bufSize buf)
                  res <- fillWithBuildStep step doneH fullH insertChunkH br
                  touchForeignPtr fpBuf
                  return res
              where
                fpBuf = bufRaw buf
                pBuf  = unsafeForeignPtrToPtr fpBuf
                op    = pBuf `plusPtr` bufR buf

                {-# INLINE updateBufR #-}
                updateBufR op' next = do
                    let !off' = op' `minusPtr` pBuf
                        !buf' = buf {bufR = off'}
                    writeIORef refBuf buf'
                    return next

                doneH op' x = updateBufR op' $ return x

                fullH op' minSize nextStep = updateBufR op' $
                    -- 'fillHandle' will flush the buffer (provided there is
                    -- really less than 'minSize' space left) before executing
                    -- the 'nextStep'.
                    fillHandle minSize nextStep

                insertChunkH op' bs nextStep = updateBufR op' $ do
                    S.hPut h bs
                    fillHandle 1 nextStep


------------------------------------------------------------------------------
-- ByteString insertion / controlling chunk boundaries
------------------------------------------------------------------------------


-- Strict ByteStrings
------------------------------------------------------------------------------


-- | Create a 'Builder' denoting the same sequence of bytes as a strict
-- 'S.ByteString'.
--
-- A 'Builder' defined as @'byteStringThreshold' maxCopySize bs@ copies @bs@, if
-- it is shorter than @maxCopySize@, and inserts it directly, otherwise.
--
{-# INLINE byteStringThreshold #-}
byteStringThreshold :: Int -> S.ByteString -> Builder     
byteStringThreshold maxCopySize = 
    \bs -> builder $ step bs
  where
    step !bs !k br@(BufferRange !op _)
      | maxCopySize < S.length bs = return $ insertChunk op bs k
      | otherwise                 = byteStringCopyStep bs k br

-- | The created 'Builder' always copies the 'S.ByteString'. Use this function
-- to create 'Builder's from smallish (@<= 4kb@) 'S.ByteString's or if you need
-- to guarantee that the 'S.ByteString' is not shared with the chunks generated
-- by the 'Builder'.
--
{-# INLINE byteStringCopy #-}
byteStringCopy :: S.ByteString -> Builder
byteStringCopy = \bs -> builder $ byteStringCopyStep bs

{-# INLINE byteStringCopyStep #-}
byteStringCopyStep :: S.ByteString 
                   -> (BufferRange -> IO (BuildSignal a))
                   -> (BufferRange -> IO (BuildSignal a))
byteStringCopyStep (S.PS ifp ioff isize) !k = 
    goBS (unsafeForeignPtrToPtr ifp `plusPtr` ioff)
  where
    !ipe = unsafeForeignPtrToPtr ifp `plusPtr` (ioff + isize)
    goBS !ip !(BufferRange op ope)
      | inpRemaining <= outRemaining = do
          copyBytes op ip inpRemaining
          touchForeignPtr ifp -- input consumed: OK to release from here
          let !br' = BufferRange (op `plusPtr` inpRemaining) ope
          k br'
      | otherwise = do
          copyBytes op ip outRemaining
          let !ip' = ip `plusPtr` outRemaining
          return $ bufferFull 1 ope (goBS ip')
      where
        outRemaining = ope `minusPtr` op
        inpRemaining = ipe `minusPtr` ip

-- | The created 'Builder' always inserts the 'S.ByteString' directly as a chunk. 
-- This implies flushing the output buffer; even if it contains just
-- a single byte! Hence, you should use 'byteStringInsert' only for large (@>
-- 8kb@) 'S.ByteString's. Otherwise, the generated chunks are too fragmented to
-- be processed efficiently.
--
{-# INLINE byteStringInsert #-}
byteStringInsert :: S.ByteString -> Builder
byteStringInsert = 
    \bs -> builder $ step bs
  where
    step !bs !k !(BufferRange op _) = return $ insertChunk op bs k


-- Lazy bytestrings
------------------------------------------------------------------------------

-- | Chunk-wise application of 'byteStringThreshold' to a lazy 'L.ByteString'.
--
{-# INLINE lazyByteStringThreshold #-}
lazyByteStringThreshold :: Int -> L.ByteString -> Builder
lazyByteStringThreshold maxCopySize = 
  L.foldrChunks (\bs b -> byteStringThreshold maxCopySize bs `mappend` b) mempty

-- | Chunk-wise application of 'byteStringCopy' to a lazy 'L.ByteString'.
--
{-# INLINE lazyByteStringCopy #-}
lazyByteStringCopy :: L.ByteString -> Builder
lazyByteStringCopy = 
  L.foldrChunks (\bs b -> byteStringCopy bs `mappend` b) mempty

-- This function costs /O(n)/ where /n/ is the number of chunks of the lazy
-- 'L.ByteString'. The design of the 'Builder' could be changed to support an
-- /O(1)/ insertion of a difference-list style lazy bytestring. Please contact
-- me, if you have a use case for that.

-- | Chunk-wise application of 'byteStringInsert' to a lazy 'L.ByteString'.
--
{-# INLINE lazyByteStringInsert #-}
lazyByteStringInsert :: L.ByteString -> Builder
lazyByteStringInsert =
  L.foldrChunks (\bs b -> byteStringInsert bs `mappend` b) mempty

-- | Create a 'Builder' denoting the same sequence of bytes as a strict
-- 'S.ByteString'.
--
-- The 'Builder' copies short 'S.ByteString's and inserts long 'S.ByteString's
-- directly. This way the 'Builder' ensures that chunks are large on average,
-- which is important for the efficiency of consumers of the generated chunks.
-- See the "Data.ByteString.Lazy.Builder.Extras" module, if you need more
-- control over chunk sizes.
--
{-# INLINE byteString #-}
byteString :: S.ByteString -> Builder
byteString = byteStringThreshold maximalCopySize

-- | Chunk-wise application of 'byteString' to a lazy 'L.ByteString'.
--
{-# INLINE lazyByteString #-}
lazyByteString :: L.ByteString -> Builder
lazyByteString = lazyByteStringThreshold maximalCopySize

-- | The maximal size of a 'S.ByteString' that is copied. 
-- @2 * 'L.smallChunkSize'@ to guarantee that on average a chunk is of
-- 'L.smallChunkSize'.
maximalCopySize :: Int
maximalCopySize = 2 * L.smallChunkSize

------------------------------------------------------------------------------
-- Builder execution
------------------------------------------------------------------------------

-- | A buffer allocation strategy for executing 'Builder's. 

-- The strategy
--
-- > 'AllocationStrategy' firstBufSize bufSize trim
--
-- states that the first buffer is of size @firstBufSize@, all following buffers
-- are of size @bufSize@, and a buffer of size @n@ filled with @k@ bytes should
-- be trimmed iff @trim k n@ is 'True'.
data AllocationStrategy = AllocationStrategy 
         {-# UNPACK #-} !Int  -- size of first buffer
         {-# UNPACK #-} !Int  -- size of successive buffers
         (Int -> Int -> Bool) -- trim

-- | Sanitize a buffer size; i.e., make it at least the size of a 'Int'.
sanitize :: Int -> Int
sanitize = max (sizeOf (undefined :: Int))

-- | Use this strategy for generating lazy 'L.ByteString's whose chunks are
-- discarded right after they are generated. For example, if you just generate
-- them to write them to a network socket.
untrimmedStrategy :: Int -- ^ Size of the first buffer
                  -> Int -- ^ Size of successive buffers
                  -> AllocationStrategy 
                  -- ^ An allocation strategy that does not trim any of the
                  -- filled buffers before converting it to a chunk.
untrimmedStrategy firstSize bufSize = 
    AllocationStrategy (sanitize firstSize) (sanitize bufSize) (\_ _ -> False)


-- | Use this strategy for generating lazy 'L.ByteString's whose chunks are
-- likely to survive one garbage collection. Note that
--
-- > toLazyByteString = 
-- >   toLazyByteStringWith (safeStrategy smallChunkSize defaultChunkSize) empty
--
-- where @empty@ is the zero-length lazy 'L.ByteString'.
safeStrategy :: Int  -- ^ Size of first buffer
             -> Int  -- ^ Size of successive buffers
             -> AllocationStrategy
             -- ^ An allocation strategy that guarantees that at least half
             -- of the allocated memory is used for live data
safeStrategy firstSize bufSize = 
    AllocationStrategy (sanitize firstSize) (sanitize bufSize) 
                       (\used size -> 2*used < size)

-- | Execute a 'Builder' with custom execution parameters.
--
-- In most cases, the parameters used by 'toLazyByteString' give good
-- performance. A slightly sub-performing case is generating lots of short
-- (<128 bytes) 'L.ByteString's using 'Builder's. In this case, you might gain
-- additional performance by executing the 'Builder's using
--
-- > toLazyByteStringWith (safeStrategy 128 smallChunkSize) empty
--
-- This reduces the allocation and trimming overhead, as all generated
-- 'L.ByteString's fit into the first allocated buffer and chances are better
-- that the buffer doesn't have to be trimmed.
--
{-# INLINE toLazyByteStringWith #-}
toLazyByteStringWith 
    :: AllocationStrategy
       -- ^ Buffer allocation strategy to use
    -> L.ByteString  
       -- ^ Lazy 'L.ByteString' to use as the tail of the generated lazy
       -- 'L.ByteString'
    -> Builder 
       -- ^ Builder to execute
    -> L.ByteString
       -- ^ Resulting lazy 'L.ByteString'
toLazyByteStringWith (AllocationStrategy firstSize bufSize trim) k b = 
    S.inlinePerformIO $ fillNew (runBuilder b) firstSize 
  where
    fillNew !step0 !size = do
        S.mallocByteString size >>= fill step0
      where
        fill !step !fpbuf =
            fillWithBuildStep step doneH fullH insertChunkH br
          where
            op  = unsafeForeignPtrToPtr fpbuf -- safe due to mkbs
            pe  = op `plusPtr` size
            !br = BufferRange op pe
            
            -- we are done: return lazy bytestring continuation
            doneH op' _ 
              | op' == op = return k
              | otherwise = mkbs op' k

            -- buffer full: add chunk if it is non-empty and fill next buffer
            fullH op' minSize nextStep 
              | op' == op = fillNew nextStep (max minSize bufSize)

              | otherwise = 
                  mkbs op' $ S.inlinePerformIO
                           $ fillNew nextStep (max minSize bufSize)
            
            -- insert a chunk: prepend current chunk, if there is one
            insertChunkH op' bs nextStep
              | op' == op =
                  return $ nonEmptyChunk bs 
                         $ S.inlinePerformIO 
                         $ fill nextStep fpbuf

              | otherwise =
                  mkbs op' $ nonEmptyChunk bs 
                           $ S.inlinePerformIO 
                           $ fillNew nextStep bufSize

            -- add a chunk to a lazy bytestring, trimming the chunk if necesary
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

                    

-- | Prepend the chunk if it is non-empty.
{-# INLINE nonEmptyChunk #-}
nonEmptyChunk :: S.ByteString -> L.ByteString -> L.ByteString
nonEmptyChunk bs lbs | S.null bs = lbs 
                     | otherwise = L.Chunk bs lbs

