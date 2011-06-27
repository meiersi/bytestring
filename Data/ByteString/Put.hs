-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) 2010-2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-----------------------------------------------------------------------------
module Data.ByteString.Put
    ( 
      -- * The Put type
      Put

      -- * Conversion between Puts and Builders
    , fromPut
    , putBuilder

      -- * Executing @Put@s
      --
      -- | @Put ()@ computations can be executed by converting it to a
      -- 'Builder' and using 'toLazyByteString'.
      --
      -- > runPut :: Put () -> L.ByteString
      -- > runPut = toLazyByteString . fromPut
      --
      -- Support for running general @'Put' a@ computations and outputting its
      -- side-effect to a file or a network socket is planned. The enumerator
      -- <http://hackage.haskell.org/package/enumerator> and iteratee
      -- <http://hackage.haskell.org/package/iteratee> libraries are likely to
      -- include support for running @'Put' a@ computations.

    ) where

import Data.ByteString.Builder.Internal

{-

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
    runStep step buf@(Buffer fpbuf p0 op ope) = do
        let !br = BufferRange op ope
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

-}

