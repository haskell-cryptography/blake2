----------------------------------------------------------------
-- |
-- Module      : Crypto.Hash.BLAKE2.BLAKE2sp
-- Maintainer  : John Galt <jgalt@centromere.net>
-- Stability   : experimental
-- Portability : POSIX

module Crypto.Hash.BLAKE2.BLAKE2sp
  ( -- * Types
    BLAKE2spState,
    -- * Functions
    initialize,
    initialize',
    update,
    finalize,
    hash
  ) where

import Data.ByteString    (ByteString)
import Foreign.C.Types    (CInt(..))
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Storable   (Storable(..))

import Crypto.Hash.BLAKE2.Internal

#include "blake2.h"

data BLAKE2spStruct

instance Storable BLAKE2spStruct where
  sizeOf    _ = #{size blake2sp_state}
  alignment _ = #{alignment blake2sp_state}
  peek        = error "peek not implemented"
  poke        = error "poke not implemented"

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- | The hash state.
type BLAKE2spState = ForeignPtr BLAKE2spStruct

-- | Create a new hashing state.
initialize :: Int
           -- ^ Output length in bytes
           -> BLAKE2spState
initialize = initializer c_blake2sp_init
{-# INLINE initialize #-}

-- | Create a new keyed hashing state.
initialize' :: Int
            -- ^ Output length in bytes
            -> ByteString
            -- ^ Key
            -> BLAKE2spState
initialize' = initializer' c_blake2sp_init_key
{-# INLINE initialize' #-}

-- | Add data to the hashing state.
update :: ByteString
       -- ^ Data to hash
       -> BLAKE2spState
       -- ^ Hashing state
       -> BLAKE2spState
update = updater c_blake2sp_update
{-# INLINE update #-}

-- | Finalize the hashing state.
finalize :: Int
         -- ^ Output length in bytes
         -> BLAKE2spState
         -- ^ Hashing state
         -> ByteString
finalize = finalizer c_blake2sp_final
{-# INLINE finalize #-}

-- | Perform hashing all in one step. A common way of calling this function
--   is @hash 32 mempty dataToHash@ for applications which do not require
--   keying.
hash :: Int
     -- ^ Output length in bytes
     -> ByteString
     -- ^ Key
     -> ByteString
     -- ^ Data to hash
     -> ByteString
hash = hasher c_blake2sp
{-# INLINE hash #-}

foreign import ccall unsafe "blake2.h blake2sp"
  c_blake2sp :: HashFunc
foreign import ccall unsafe "blake2.h blake2sp_init"
  c_blake2sp_init :: InitFunc BLAKE2spStruct
foreign import ccall unsafe "blake2.h blake2sp_init_key"
  c_blake2sp_init_key :: InitKeyFunc BLAKE2spStruct
foreign import ccall unsafe "blake2.h blake2sp_update"
  c_blake2sp_update :: UpdateFunc BLAKE2spStruct
foreign import ccall unsafe "blake2.h blake2sp_final"
  c_blake2sp_final :: FinalFunc BLAKE2spStruct
