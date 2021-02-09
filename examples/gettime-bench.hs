{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}


{-| gettimeofday bench

This benchmark confirms an unneglectable overhead in common FFI bindings: the overhead of doing pinned allocation.

'getSystemTime' from  time package: ~50ns
'getSystemTime'' from Z-IO package: ~35ns

Theoretically things will get worse under high concurrent load since pinned allocation sometime requires lock GHC's sm.

-}
import  Criterion.Main    (Benchmark, bench, bgroup, defaultMain, nfIO)

import qualified Data.Time.Clock.System as T
import qualified Z.IO.Time              as Z

main :: IO ()
main = do
    defaultMain
      [ bgroup "Examples"
          [ bench "gettimeofday-z" $ nfIO Z.getSystemTime'
          , bench "gettimeofday-time" $ nfIO T.getSystemTime
          ]
      ]
