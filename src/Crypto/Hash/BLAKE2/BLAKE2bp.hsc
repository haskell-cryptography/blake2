----------------------------------------------------------------
-- |
-- Module      : Crypto.Hash.BLAKE2.BLAKE2bp
-- Maintainer  : John Galt <jgalt@centromere.net>
-- Stability   : experimental
-- Portability : POSIX

module Crypto.Hash.BLAKE2.BLAKE2bp
  ( -- * Types
    BLAKE2bpState,
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

data BLAKE2bpStruct

instance Storable BLAKE2bpStruct where
  sizeOf    _ = #{size blake2bp_state}
  alignment _ = #{alignment blake2bp_state}
  peek        = error "peek not implemented"
  poke        = error "poke not implemented"

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- | The hash state.
type BLAKE2bpState = ForeignPtr BLAKE2bpStruct

-- | Create a new hashing state.
initialize :: Int
           -- ^ Output length in bytes
           -> BLAKE2bpState
initialize = initializer c_blake2bp_init
{-# INLINE initialize #-}

-- | Create a new keyed hashing state.
initialize' :: Int
            -- ^ Output length in bytes
            -> ByteString
            -- ^ Key
            -> BLAKE2bpState
initialize' = initializer' c_blake2bp_init_key
{-# INLINE initialize' #-}

-- | Add data to the hashing state.
update :: ByteString
       -- ^ Data to hash
       -> BLAKE2bpState
       -- ^ Hashing state
       -> BLAKE2bpState
update = updater c_blake2bp_update
{-# INLINE update #-}

-- | Finalize the hashing state.
finalize :: Int
         -- ^ Output length in bytes
         -> BLAKE2bpState
         -- ^ Hashing state
         -> ByteString
finalize = finalizer c_blake2bp_final
{-# INLINE finalize #-}

-- | Perform hashing all in one step.
hash :: Int
     -- ^ Output length in bytes
     -> ByteString
     -- ^ Key
     -> ByteString
     -- ^ Data to hash
     -> ByteString
hash = hasher c_blake2bp
{-# INLINE hash #-}

foreign import ccall unsafe "blake2.h blake2bp"
  c_blake2bp :: HashFunc
foreign import ccall unsafe "blake2.h blake2bp_init"
  c_blake2bp_init :: InitFunc BLAKE2bpStruct
foreign import ccall unsafe "blake2.h blake2bp_init_key"
  c_blake2bp_init_key :: InitKeyFunc BLAKE2bpStruct
foreign import ccall unsafe "blake2.h blake2bp_update"
  c_blake2bp_update :: UpdateFunc BLAKE2bpStruct
foreign import ccall unsafe "blake2.h blake2bp_final"
  c_blake2bp_final :: FinalFunc BLAKE2bpStruct
