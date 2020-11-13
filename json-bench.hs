{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

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
        [ bgroup "decode"
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
        ,
          bgroup "encode"
          [ bench "twitter100-aeson" $ nf A.encode v_twitter100
          , bench "twitter100-z" $ nf (buildBytesList . J.encodeJSON) v_twitter100'
          , bench "jp100-aeson" $ nf A.encode v_jp100
          , bench "jp100-z" $ nf (buildBytesList . J.encodeJSON) v_jp100'
          , bench "geometry-aeson" $ nf A.encode v_geometry
          , bench "geometry-z" $ nf (buildBytesList . J.encodeJSON) v_geometry'
          , bench "github-aeson" $ nf A.encode v_github
          , bench "github-z" $ nf (buildBytesList . J.encodeJSON) v_github'
          , bench "buffer-builder-aeson" $ nf A.encode v_bb
          , bench "buffer-builder-z" $ nf (buildBytesList . J.encodeJSON) v_bb'
          ]
        ]
      ]

