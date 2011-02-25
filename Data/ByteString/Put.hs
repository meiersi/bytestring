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
      -- * The @Put@ type
      Put

      -- * Conversion between @Put@s and @Builder@s
    , fromPut
    , putBuilder

      -- * Executing @Put@s
    , runPut

    ) where

import Data.ByteString.Builder.Internal


