{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion.Main
import Data.ByteString                (ByteString)
import qualified Data.ByteString as B (replicate)

import qualified Crypto.Hash.BLAKE2.BLAKE2b as B2b
import qualified Crypto.Hash.BLAKE2.BLAKE2bp as B2bp
import qualified Crypto.Hash.BLAKE2.BLAKE2s as B2s
import qualified Crypto.Hash.BLAKE2.BLAKE2sp as B2sp

kb :: ByteString
kb = B.replicate 1024 3

mb :: ByteString
mb = B.replicate (1024^2) 3

gb :: ByteString
gb = B.replicate (1024^3) 3

main :: IO ()
main = defaultMain
  [ bgroup "blake2s" [ bench "1 kb" $ nf (B2s.hash 32 "") kb
                     , bench "1 mb" $ nf (B2s.hash 32 "") mb
                     , bench "1 gb" $ nf (B2s.hash 32 "") gb
                     ]
  , bgroup "blake2sp" [ bench "1 kb" $ nf (B2sp.hash 32 "") kb
                      , bench "1 mb" $ nf (B2sp.hash 32 "") mb
                      , bench "1 gb" $ nf (B2sp.hash 32 "") gb
                      ]
  , bgroup "blake2b" [ bench "1 kb" $ nf (B2b.hash 64 "") kb
                     , bench "1 mb" $ nf (B2b.hash 64 "") mb
                     , bench "1 gb" $ nf (B2b.hash 64 "") gb
                     ]
  , bgroup "blake2bp" [ bench "1 kb" $ nf (B2bp.hash 64 "") kb
                      , bench "1 mb" $ nf (B2bp.hash 64 "") mb
                      , bench "1 gb" $ nf (B2bp.hash 64 "") gb
                      ]
  ]
