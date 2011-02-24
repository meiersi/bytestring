-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) 2010 Jasper Van der Jeugt & Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-----------------------------------------------------------------------------

module Data.ByteString.Builder
    ( 
      -- * The 'Builder' type
      Builder

      -- * Creating builders
    -- , module Blaze.ByteString.Builder.Int
    -- , module Blaze.ByteString.Builder.Word
    -- , module Blaze.ByteString.Builder.ByteString
    , flush

      -- * Executing builders
    , toLazyByteString
    , toLazyByteStringWith
    , toByteString
    , toByteStringIO
    , toByteStringIOWith

    {-
    -- * 'Write's
    , Write
    , fromWrite
    , fromWriteSingleton
    , fromWriteList
    -}

    ) where

import Data.ByteString.Builder.Internal
-- import Blaze.ByteString.Builder.Write

