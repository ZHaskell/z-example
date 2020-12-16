{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Benchmark JSON encoding and decoding speed

On my MBP13 2020, 2 GHz Quad-Core Intel Core i5

benchmarking Examples/encode/twitter100-aeson
time                 231.0 μs   (229.7 μs .. 232.7 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 233.1 μs   (231.9 μs .. 235.5 μs)
std dev              5.407 μs   (3.076 μs .. 9.648 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking Examples/encode/twitter100-z
time                 162.8 μs   (162.4 μs .. 163.2 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 162.5 μs   (162.2 μs .. 163.0 μs)
std dev              1.276 μs   (982.4 ns .. 1.876 μs)

benchmarking Examples/encode/jp100-aeson
time                 234.9 μs   (234.1 μs .. 235.8 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 235.7 μs   (235.0 μs .. 236.7 μs)
std dev              2.794 μs   (2.192 μs .. 3.543 μs)

benchmarking Examples/encode/jp100-z
time                 166.2 μs   (165.5 μs .. 166.9 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 166.2 μs   (165.8 μs .. 166.7 μs)
std dev              1.445 μs   (1.152 μs .. 1.827 μs)

benchmarking Examples/encode/geometry-aeson
time                 2.030 ms   (2.024 ms .. 2.036 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 2.049 ms   (2.039 ms .. 2.077 ms)
std dev              49.84 μs   (24.08 μs .. 97.48 μs)
variance introduced by outliers: 12% (moderately inflated)

benchmarking Examples/encode/geometry-z
time                 1.455 ms   (1.447 ms .. 1.466 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.450 ms   (1.447 ms .. 1.454 ms)
std dev              11.45 μs   (7.887 μs .. 16.20 μs)

benchmarking Examples/encode/github-aeson
time                 238.7 μs   (238.1 μs .. 239.7 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 241.0 μs   (239.6 μs .. 245.1 μs)
std dev              7.017 μs   (3.573 μs .. 14.63 μs)
variance introduced by outliers: 24% (moderately inflated)

benchmarking Examples/encode/github-z
time                 181.1 μs   (180.1 μs .. 182.1 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 180.3 μs   (179.9 μs .. 180.9 μs)
std dev              1.614 μs   (1.301 μs .. 2.196 μs)

benchmarking Examples/encode/buffer-builder-aeson
time                 544.2 μs   (542.7 μs .. 545.8 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 545.9 μs   (543.9 μs .. 548.4 μs)
std dev              7.687 μs   (5.543 μs .. 11.64 μs)

benchmarking Examples/encode/buffer-builder-z
time                 426.7 μs   (424.2 μs .. 430.2 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 441.7 μs   (435.8 μs .. 449.3 μs)
std dev              22.23 μs   (18.53 μs .. 27.14 μs)
variance introduced by outliers: 46% (moderately inflated)

benchmarking Examples/decode/twitter100-aeson
time                 1.142 ms   (1.137 ms .. 1.148 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.155 ms   (1.148 ms .. 1.163 ms)
std dev              25.86 μs   (22.05 μs .. 31.19 μs)
variance introduced by outliers: 11% (moderately inflated)

benchmarking Examples/decode/twitter100-z
time                 420.9 μs   (417.4 μs .. 425.9 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 419.7 μs   (417.8 μs .. 423.2 μs)
std dev              8.482 μs   (5.988 μs .. 12.09 μs)
variance introduced by outliers: 12% (moderately inflated)

benchmarking Examples/decode/jp100-aeson
time                 1.570 ms   (1.557 ms .. 1.593 ms)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 1.584 ms   (1.572 ms .. 1.609 ms)
std dev              60.46 μs   (38.63 μs .. 101.6 μs)
variance introduced by outliers: 25% (moderately inflated)

benchmarking Examples/decode/jp100-z
time                 514.7 μs   (512.5 μs .. 517.5 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 514.0 μs   (512.6 μs .. 515.9 μs)
std dev              5.428 μs   (4.139 μs .. 8.436 μs)

benchmarking Examples/decode/geometry-aeson
time                 2.275 ms   (2.254 ms .. 2.308 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 2.262 ms   (2.246 ms .. 2.283 ms)
std dev              60.43 μs   (44.70 μs .. 79.65 μs)
variance introduced by outliers: 14% (moderately inflated)

benchmarking Examples/decode/geometry-z
time                 838.0 μs   (831.3 μs .. 845.6 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 837.0 μs   (832.3 μs .. 844.0 μs)
std dev              19.39 μs   (14.14 μs .. 29.82 μs)
variance introduced by outliers: 13% (moderately inflated)

benchmarking Examples/decode/github-aeson
time                 1.427 ms   (1.423 ms .. 1.431 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.423 ms   (1.419 ms .. 1.426 ms)
std dev              11.88 μs   (9.815 μs .. 14.86 μs)

benchmarking Examples/decode/github-z
time                 477.5 μs   (473.3 μs .. 481.6 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 473.1 μs   (471.4 μs .. 475.3 μs)
std dev              6.533 μs   (4.399 μs .. 10.19 μs)

benchmarking Examples/decode/buffer-builder-aeson
time                 2.823 ms   (2.760 ms .. 2.890 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 2.805 ms   (2.779 ms .. 2.843 ms)
std dev              95.62 μs   (59.34 μs .. 144.1 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Examples/decode/buffer-builder-z
time                 1.164 ms   (1.145 ms .. 1.184 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 1.166 ms   (1.157 ms .. 1.179 ms)
std dev              38.11 μs   (29.67 μs .. 47.40 μs)
variance introduced by outliers: 21% (moderately inflated)
-}

import  Criterion.Main    (Benchmark, bench, bgroup, defaultMain, nf)

import  Data.Aeson as A
import  Z.Data.JSON as J
import qualified Data.ByteString  as B (readFile)
import qualified Z.IO.FileSystem  as Z (quickReadFile)

main :: IO ()
main = do
    !twitter100 <- B.readFile "asset/json-data/twitter100.json"
    !twitter100' <- Z.quickReadFile "asset/json-data/twitter100.json"
    let Just !v_twitter100 = A.decodeStrict @A.Value twitter100
        Right !v_twitter100' = J.decode' @J.Value twitter100'

    !jp100 <- B.readFile "asset/json-data/jp100.json"
    !jp100' <- Z.quickReadFile "asset/json-data/jp100.json"
    let Just !v_jp100 = A.decodeStrict @A.Value jp100
        Right !v_jp100' = J.decode' @J.Value jp100'

    !geometry <- B.readFile "asset/json-data/geometry.json"
    !geometry' <- Z.quickReadFile "asset/json-data/geometry.json"
    let Just !v_geometry = A.decodeStrict @A.Value geometry
        Right !v_geometry' = J.decode' @J.Value geometry'

    !github <- B.readFile "asset/json-data/github-issues.json"
    !github' <- Z.quickReadFile "asset/json-data/github-issues.json"
    let Just !v_github = A.decodeStrict @A.Value github
        Right !v_github' = J.decode' @J.Value github'

    !bb <- B.readFile "asset/json-data/buffer-builder.json"
    !bb' <- Z.quickReadFile "asset/json-data/buffer-builder.json"
    let Just !v_bb = A.decodeStrict @A.Value bb
        Right !v_bb' = J.decode' @J.Value bb'

    defaultMain
      [ bgroup "Examples"
        [ bgroup "encode"
          [ bench "twitter100-aeson" $ nf A.encode v_twitter100
          , bench "twitter100-z" $ nf J.encode v_twitter100'
          , bench "jp100-aeson" $ nf A.encode v_jp100
          , bench "jp100-z" $ nf J.encode v_jp100'
          , bench "geometry-aeson" $ nf A.encode v_geometry
          , bench "geometry-z" $ nf J.encode v_geometry'
          , bench "github-aeson" $ nf A.encode v_github
          , bench "github-z" $ nf J.encode v_github'
          , bench "buffer-builder-aeson" $ nf A.encode v_bb
          , bench "buffer-builder-z" $ nf J.encode v_bb'
          ]
        , bgroup "decode"
          [ bench "twitter100-aeson" $ nf (A.decodeStrict @A.Value) twitter100
          , bench "twitter100-z" $ nf (J.decode' @J.Value) twitter100'
          , bench "jp100-aeson" $ nf (A.decodeStrict @A.Value) jp100
          , bench "jp100-z" $ nf (J.decode' @J.Value) jp100'
          , bench "geometry-aeson" $ nf (A.decodeStrict @A.Value) geometry
          , bench "geometry-z" $ nf (J.decode' @J.Value) geometry'
          , bench "github-aeson" $ nf (A.decodeStrict @A.Value) github
          , bench "github-z" $ nf (J.decode' @J.Value) github'
          , bench "buffer-builder-aeson" $ nf (A.decodeStrict @A.Value) bb
          , bench "buffer-builder-z" $ nf (J.decode' @J.Value) bb'
          ]
        ]
      ]
