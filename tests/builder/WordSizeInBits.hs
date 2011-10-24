{-# LANGUAGE CPP #-}
-- | Detect the true value of WORD_SIZE_IN_BITS
module Main where

-- #include "MachDeps.h"

-- size = show WORD_SIZE_IN_BITS
size = "0"

main = do
  putStrLn size
  putStrLn test

-- wordSize = bitSize (undefined :: Word)


-- intSize = bitSize (undefined :: Int)

-- !!! WORD_SIZE_IN_BITS undefined is NOT reported.

#if WORD_SIZE_IN_BITS == 64
test = "== 64"
#else
test = "not == 64"
#endif
