{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Hex
import Prelude hiding (compare)

import qualified Crypto.Hash.BLAKE2.BLAKE2b as B2b
import qualified Crypto.Hash.BLAKE2.BLAKE2bp as B2bp
import qualified Crypto.Hash.BLAKE2.BLAKE2s as B2s
import qualified Crypto.Hash.BLAKE2.BLAKE2sp as B2sp

import Imports

key32 :: ByteString
key32 = fst . Hex.decode $ "000102030405060708090a0b0c0d0e0f\
                           \101112131415161718191a1b1c1d1e1f"

key64 :: ByteString
key64 = fst . Hex.decode $ "000102030405060708090a0b0c0d0e0f\
                           \101112131415161718191a1b1c1d1e1f\
                           \202122232425262728292a2b2c2d2e2f\
                           \303132333435363738393a3b3c3d3e3f"

compare :: (ByteString -> ByteString)
        -> ByteString
        -> ByteString
        -> Property
compare f input expectation = property $ result === expectation
  where
    result = Hex.encode . f . fst . Hex.decode $ input

test0s :: Property
test0s = compare (B2s.hash 32 key32) "" "48a8997da407876b3d79c0d92325ad3b89cbb754d86ab71aee047ad345fd2c49"

test1s :: Property
test1s = compare (B2s.hash 32 key32) "00" "40d15fee7c328830166ac3f918650f807e7e01e177258cdc0a39b11f598066f1"

test2s :: Property
test2s = compare (B2s.hash 32 key32) "0001" "6bb71300644cd3991b26ccd4d274acd1adeab8b1d7914546c1198bbe9fc9d803"

test3s :: Property
test3s = compare (B2s.hash 32 key32) "000102" "1d220dbe2ee134661fdf6d9e74b41704710556f2f6e5a091b227697445dbea6b"

test0sp :: Property
test0sp = compare (B2sp.hash 32 key32) "" "715cb13895aeb678f6124160bff21465b30f4f6874193fc851b4621043f09cc6"

test1sp :: Property
test1sp = compare (B2sp.hash 32 key32) "00" "40578ffa52bf51ae1866f4284d3a157fc1bcd36ac13cbdcb0377e4d0cd0b6603"

test2sp :: Property
test2sp = compare (B2sp.hash 32 key32) "0001" "67e3097545bad7e852d74d4eb548eca7c219c202a7d088db0efeac0eac304249"

test3sp :: Property
test3sp = compare (B2sp.hash 32 key32) "000102" "8dbcc0589a3d17296a7a58e2f1eff0e2aa4210b58d1f88b86d7ba5f29dd3b583"

test0b :: Property
test0b = compare (B2b.hash 64 key64) "" "10ebb67700b1868efb4417987acf4690ae9d972fb7a590c2f02871799aaa4786b5e996e8f0f4eb981fc214b005f42d2ff4233499391653df7aefcbc13fc51568"

test1b :: Property
test1b = compare (B2b.hash 64 key64) "00" "961f6dd1e4dd30f63901690c512e78e4b45e4742ed197c3c5e45c549fd25f2e4187b0bc9fe30492b16b0d0bc4ef9b0f34c7003fac09a5ef1532e69430234cebd"

test2b :: Property
test2b = compare (B2b.hash 64 key64) "0001" "da2cfbe2d8409a0f38026113884f84b50156371ae304c4430173d08a99d9fb1b983164a3770706d537f49e0c916d9f32b95cc37a95b99d857436f0232c88a965"

test3b :: Property
test3b = compare (B2b.hash 64 key64) "000102" "33d0825dddf7ada99b0e7e307104ad07ca9cfd9692214f1561356315e784f3e5a17e364ae9dbb14cb2036df932b77f4b292761365fb328de7afdc6d8998f5fc1"

test0bp :: Property
test0bp = compare (B2bp.hash 64 key64) "" "9d9461073e4eb640a255357b839f394b838c6ff57c9b686a3f76107c1066728f3c9956bd785cbc3bf79dc2ab578c5a0c063b9d9c405848de1dbe821cd05c940a"

test1bp :: Property
test1bp = compare (B2bp.hash 64 key64) "00" "ff8e90a37b94623932c59f7559f26035029c376732cb14d41602001cbb73adb79293a2dbda5f60703025144d158e2735529596251c73c0345ca6fccb1fb1e97e"

test2bp :: Property
test2bp = compare (B2bp.hash 64 key64) "0001" "d6220ca195a0f356a4795e071cee1f5412ecd95d8a5e01d7c2b86750ca53d7f64c29cbb3d289c6f4ecc6c01e3ca9338971170388e3e40228479006d1bbebad51"

test3bp :: Property
test3bp = compare (B2bp.hash 64 key64) "000102" "30302c3fc999065d10dc982c8feef41bbb6642718f624af6e3eabea083e7fe785340db4b0897efff39cee1dc1eb737cd1eea0fe75384984e7d8f446faa683b80"

main :: IO ()
main = defaultMain $ testGroup "blake2"
  [ testGroup "blake2s" [ testProperty "0" test0s
                        , testProperty "1" test1s
                        , testProperty "2" test2s
                        , testProperty "3" test3s
                        ]
  , testGroup "blake2sp" [ testProperty "0" test0sp
                         , testProperty "1" test1sp
                         , testProperty "2" test2sp
                         , testProperty "3" test3sp
                         ]
  , testGroup "blake2b" [ testProperty "0" test0b
                        , testProperty "1" test1b
                        , testProperty "2" test2b
                        , testProperty "3" test3b
                        ]
  , testGroup "blake2bp" [ testProperty "0" test0bp
                         , testProperty "1" test1bp
                         , testProperty "2" test2bp
                         , testProperty "3" test3bp
                         ]
  ]
