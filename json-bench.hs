{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Benchmark JSON encoding and decoding speed

On my MBP13 2020, 2 GHz Quad-Core Intel Core i5

benchmarking Examples/encode/twitter100-aeson
time                 218.3 μs   (215.5 μs .. 221.6 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 221.3 μs   (219.0 μs .. 223.2 μs)
std dev              6.993 μs   (5.984 μs .. 8.325 μs)
variance introduced by outliers: 27% (moderately inflated)

benchmarking Examples/encode/twitter100-z
time                 175.1 μs   (172.7 μs .. 177.4 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 176.2 μs   (174.7 μs .. 177.7 μs)
std dev              5.131 μs   (4.114 μs .. 6.462 μs)
variance introduced by outliers: 25% (moderately inflated)

benchmarking Examples/encode/jp100-aeson
time                 225.9 μs   (222.8 μs .. 229.2 μs)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 226.5 μs   (224.2 μs .. 228.9 μs)
std dev              7.876 μs   (6.725 μs .. 9.671 μs)
variance introduced by outliers: 31% (moderately inflated)

benchmarking Examples/encode/jp100-z
time                 174.5 μs   (172.2 μs .. 176.7 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 173.4 μs   (171.6 μs .. 174.9 μs)
std dev              5.706 μs   (4.813 μs .. 6.699 μs)
variance introduced by outliers: 30% (moderately inflated)

benchmarking Examples/encode/geometry-aeson
time                 1.984 ms   (1.954 ms .. 2.014 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 1.990 ms   (1.967 ms .. 2.011 ms)
std dev              70.41 μs   (54.98 μs .. 89.17 μs)
variance introduced by outliers: 22% (moderately inflated)

benchmarking Examples/encode/geometry-z
time                 2.133 ms   (2.106 ms .. 2.158 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 2.169 ms   (2.146 ms .. 2.202 ms)
std dev              89.36 μs   (66.87 μs .. 158.1 μs)
variance introduced by outliers: 26% (moderately inflated)

benchmarking Examples/encode/github-aeson
time                 229.6 μs   (225.5 μs .. 233.2 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 230.1 μs   (227.3 μs .. 232.5 μs)
std dev              8.806 μs   (7.538 μs .. 10.53 μs)
variance introduced by outliers: 36% (moderately inflated)

benchmarking Examples/encode/github-z
time                 195.0 μs   (193.0 μs .. 196.9 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 194.9 μs   (193.1 μs .. 196.9 μs)
std dev              6.108 μs   (5.187 μs .. 7.466 μs)
variance introduced by outliers: 27% (moderately inflated)

benchmarking Examples/encode/buffer-builder-aeson
time                 522.6 μs   (514.9 μs .. 531.2 μs)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 527.9 μs   (522.4 μs .. 533.3 μs)
std dev              18.79 μs   (16.01 μs .. 22.74 μs)
variance introduced by outliers: 28% (moderately inflated)

benchmarking Examples/encode/buffer-builder-z
time                 449.8 μs   (443.9 μs .. 456.0 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 449.3 μs   (445.4 μs .. 453.2 μs)
std dev              14.10 μs   (11.97 μs .. 16.43 μs)
variance introduced by outliers: 24% (moderately inflated)

benchmarking Examples/decode/twitter100-aeson
time                 1.144 ms   (1.100 ms .. 1.181 ms)
                     0.993 R²   (0.990 R² .. 0.997 R²)
mean                 1.081 ms   (1.064 ms .. 1.099 ms)
std dev              56.53 μs   (44.92 μs .. 72.27 μs)
variance introduced by outliers: 42% (moderately inflated)

benchmarking Examples/decode/twitter100-z
time                 417.4 μs   (416.0 μs .. 419.1 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 416.7 μs   (415.5 μs .. 417.8 μs)
std dev              3.826 μs   (3.113 μs .. 5.183 μs)

benchmarking Examples/decode/jp100-aeson
time                 1.502 ms   (1.478 ms .. 1.526 ms)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 1.500 ms   (1.488 ms .. 1.513 ms)
std dev              41.38 μs   (35.58 μs .. 49.78 μs)
variance introduced by outliers: 15% (moderately inflated)

benchmarking Examples/decode/jp100-z
time                 555.7 μs   (525.6 μs .. 580.2 μs)
                     0.986 R²   (0.977 R² .. 0.993 R²)
mean                 506.1 μs   (494.6 μs .. 522.8 μs)
std dev              44.67 μs   (33.47 μs .. 68.95 μs)
variance introduced by outliers: 71% (severely inflated)

benchmarking Examples/decode/geometry-aeson
time                 2.563 ms   (2.405 ms .. 2.771 ms)
                     0.968 R²   (0.954 R² .. 0.989 R²)
mean                 2.346 ms   (2.272 ms .. 2.445 ms)
std dev              275.8 μs   (202.2 μs .. 372.0 μs)
variance introduced by outliers: 75% (severely inflated)

benchmarking Examples/decode/geometry-z
time                 839.8 μs   (833.7 μs .. 847.5 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 847.8 μs   (842.6 μs .. 856.4 μs)
std dev              21.45 μs   (12.52 μs .. 32.41 μs)
variance introduced by outliers: 15% (moderately inflated)

benchmarking Examples/decode/github-aeson
time                 1.476 ms   (1.457 ms .. 1.503 ms)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 1.464 ms   (1.456 ms .. 1.485 ms)
std dev              39.26 μs   (19.88 μs .. 74.96 μs)
variance introduced by outliers: 15% (moderately inflated)

benchmarking Examples/decode/github-z
time                 485.9 μs   (475.1 μs .. 500.2 μs)
                     0.995 R²   (0.991 R² .. 0.999 R²)
mean                 487.3 μs   (481.7 μs .. 495.2 μs)
std dev              21.16 μs   (13.97 μs .. 30.04 μs)
variance introduced by outliers: 37% (moderately inflated)

benchmarking Examples/decode/buffer-builder-aeson
time                 2.621 ms   (2.586 ms .. 2.650 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 2.590 ms   (2.567 ms .. 2.613 ms)
std dev              78.66 μs   (67.06 μs .. 95.65 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking Examples/decode/buffer-builder-z
time                 1.165 ms   (1.139 ms .. 1.186 ms)
                     0.997 R²   (0.995 R² .. 0.998 R²)
mean                 1.119 ms   (1.105 ms .. 1.133 ms)
std dev              46.01 μs   (38.82 μs .. 55.35 μs)
variance introduced by outliers: 30% (moderately inflated)
-}

import  Criterion.Main    (Benchmark, bench, bgroup, defaultMain, nf)

import  Data.Aeson as A
import  Z.Data.JSON as J
import  Z.Data.Builder (buildBytesList)
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
          , bench "twitter100-z" $ nf J.encodeBytes v_twitter100'
          , bench "jp100-aeson" $ nf A.encode v_jp100
          , bench "jp100-z" $ nf J.encodeBytes v_jp100'
          , bench "geometry-aeson" $ nf A.encode v_geometry
          , bench "geometry-z" $ nf J.encodeBytes v_geometry'
          , bench "github-aeson" $ nf A.encode v_github
          , bench "github-z" $ nf J.encodeBytes v_github'
          , bench "buffer-builder-aeson" $ nf A.encode v_bb
          , bench "buffer-builder-z" $ nf J.encodeBytes v_bb'
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
