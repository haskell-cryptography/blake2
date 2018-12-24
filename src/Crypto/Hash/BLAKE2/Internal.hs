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
import Foreign.C.Types          (CInt, CSize)
import Foreign.ForeignPtr       (ForeignPtr, mallocForeignPtr, withForeignPtr)
import Foreign.Marshal.Array    (copyArray)
import Foreign.Ptr              (Ptr, castPtr)
import Foreign.Storable         (Storable)
import System.IO.Unsafe         (unsafePerformIO)

-- int blake2X_init( blake2X_state *S, size_t outlen );
type InitFunc a = Ptr a -> CSize -> IO CInt

-- int blake2X_init_key( blake2X_state *S, size_t outlen, const void *key, size_t keylen );
type InitKeyFunc a = Ptr a -> CSize -> Ptr () -> CSize -> IO CInt

-- int blake2X_update( blake2X_state *S, const void *in, size_t inlen );
type UpdateFunc a = Ptr a -> Ptr () -> CSize -> IO CInt

-- int blake2X_final( blake2X_state *S, void *out, size_t outlen );
type FinalFunc a = Ptr a -> Ptr () -> CSize -> IO CInt

-- int blake2s( void *out, size_t outlen, const void *in, size_t inlen, const void *key, size_t keylen );
type HashFunc = Ptr ()
             -> CSize
             -> Ptr ()
             -> CSize
             -> Ptr ()
             -> CSize
             -> IO CInt

initializer :: Storable a
            => InitFunc a
            -> Int
            -> ForeignPtr a
initializer f outlen = unsafePerformIO $ do
  fptr <- mallocForeignPtr
  withForeignPtr fptr $ \ptr -> do
    ret <- f (castPtr ptr) (fromIntegral outlen)
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
      let klen'   = fromIntegral klen
          outlen' = fromIntegral outlen
      ret <- f ptr outlen' (castPtr kptr) klen'
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
        void $ f (castPtr nsptr) (castPtr dptr) dlen'
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
        void $ f (castPtr nsptr) (castPtr optr) outlen'

hasher :: HashFunc
       -> Int
       -> ByteString
       -> ByteString
       -> ByteString
hasher h olen key input =
  unsafePerformIO . create olen $ \ostr ->
    unsafeUseAsCStringLen key $ \(kstr, klen) ->
      unsafeUseAsCStringLen input $ \(istr, ilen) ->
        let olen' = fromIntegral olen
            ilen' = fromIntegral ilen
            klen' = fromIntegral klen
        in void $ h (castPtr ostr) olen'
                    (castPtr istr) ilen'
                    (castPtr kstr) klen'
{-# INLINE hasher #-}
