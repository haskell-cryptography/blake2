----------------------------------------------------------------
-- |
-- Module      : Crypto.Hash.BLAKE2.Internal
-- Maintainer  : John Galt <jgalt@centromere.net>
-- Stability   : experimental
-- Portability : POSIX

module Crypto.Hash.BLAKE2.Internal
  ( -- * Types
    InitFunc,
    InitKeyFunc,
    UpdateFunc,
    FinalFunc,
    HashFunc,
    -- * Functions
    initializer,
    initializer',
    updater,
    finalizer,
    hasher
  ) where

import Control.Monad            (void)
import Data.ByteString          (ByteString)
import Data.ByteString.Internal (create, toForeignPtr)
import Data.ByteString.Unsafe   (unsafeUseAsCStringLen)
import Data.Word                (Word8, Word64)
import Foreign.C.Types          (CInt(..), CChar)
import Foreign.ForeignPtr       (ForeignPtr, mallocForeignPtr, withForeignPtr)
import Foreign.Marshal.Array    (copyArray)
import Foreign.Ptr              (Ptr)
import Foreign.Storable         (Storable(..))
import System.IO.Unsafe         (unsafePerformIO)

-- int blake2X_init( blake2s_state *S, const uint8_t outlen );
type InitFunc a = Ptr a -> Int -> IO CInt

-- int blake2X_init_key( blake2s_state *S, const uint8_t outlen, const void
-- *key, const uint8_t keylen );
type InitKeyFunc a = Ptr a -> Word8 -> Ptr CChar -> Word8 -> IO CInt

-- int blake2X_update( blake2s_state *S, const uint8_t *in,
-- uint64_t inlen );
type UpdateFunc a = Ptr a -> Ptr Word8 -> Word64 -> IO CInt

-- int blake2X_final( blake2s_state *S, uint8_t *out, uint8_t outlen );
type FinalFunc a = Ptr a -> Ptr Word8 -> Word8 -> IO CInt

-- int blake2X( uint8_t *out, const void *in, const void *key, const
-- uint8_t outlen, const uint64_t inlen, uint8_t keylen );
type HashFunc = Ptr Word8
             -> Ptr CChar
             -> Ptr CChar
             -> Word8
             -> Word64
             -> Word8
             -> IO CInt

initializer :: Storable a
            => InitFunc a
            -> Int
            -> ForeignPtr a
initializer f outlen = unsafePerformIO $ do
  fptr <- mallocForeignPtr
  withForeignPtr fptr $ \ptr -> do
    ret <- f ptr outlen
    if ret == 0
    then return fptr
    else error "initialization failure"

initializer' :: Storable a
             => InitKeyFunc a
             -> Int
             -> ByteString
             -> ForeignPtr a
initializer' f outlen key = unsafePerformIO $ do
  fptr <- mallocForeignPtr
  withForeignPtr fptr $ \ptr ->
    unsafeUseAsCStringLen key $ \(kptr, klen) -> do
      let klen' = fromIntegral klen
          outlen' = fromIntegral outlen
      ret <- f ptr outlen' kptr klen'
      if ret == 0
      then return fptr
      else error "initialization failure"

updater :: Storable a
        => UpdateFunc a
        -> ByteString
        -> ForeignPtr a
        -> ForeignPtr a
updater f d state = unsafePerformIO $ do
  newState <- mallocForeignPtr
  withForeignPtr newState $ \nsptr -> do
    let (dfp, _, dlen) = toForeignPtr d
        dlen' = fromIntegral dlen
    withForeignPtr dfp $ \dptr ->
      withForeignPtr state $ \sptr -> do
        copyArray nsptr sptr 1
        void $ f nsptr dptr dlen'
  return newState

finalizer :: Storable a
          => FinalFunc a
          -> Int
          -> ForeignPtr a
          -> ByteString
finalizer f outlen state = unsafePerformIO $ do
  newState <- mallocForeignPtr
  withForeignPtr newState $ \nsptr ->
    create outlen $ \optr ->
      withForeignPtr state $ \sptr -> do
        let outlen' = fromIntegral outlen
        copyArray nsptr sptr 1
        void $ f nsptr optr outlen'

hasher :: HashFunc
       -> Int
       -> ByteString
       -> ByteString
       -> ByteString
hasher h outlen key input =
  unsafePerformIO . create outlen $ \out ->
    unsafeUseAsCStringLen key $ \(kstr, klen) ->
      unsafeUseAsCStringLen input $ \(istr, ilen) ->
        let outlen' = fromIntegral outlen
            ilen'   = fromIntegral ilen
            klen'   = fromIntegral klen
        in void $ h out istr kstr outlen' ilen' klen'
{-# INLINE hasher #-}
