----------------------------------------------------------------
-- |
-- Module      : Crypto.Hash.BLAKE2.BLAKE2b
-- Maintainer  : John Galt <jgalt@centromere.net>
-- Stability   : experimental
-- Portability : POSIX

module Crypto.Hash.BLAKE2.BLAKE2b
  ( -- * Types
    BLAKE2bState,
    -- * Functions
    initialize,
    initialize',
    update,
    finalize,
    hash
  ) where

import Data.ByteString    (ByteString)
import Foreign.C.Types    (CInt(..), CSize(..))
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Storable   (Storable(..))

import Crypto.Hash.BLAKE2.Internal

#include "blake2.h"

data BLAKE2bStruct

instance Storable BLAKE2bStruct where
  sizeOf    _ = #{size blake2b_state}
  alignment _ = #{alignment blake2b_state}
  peek        = error "peek not implemented"
  poke        = error "poke not implemented"

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- | The hash state.
type BLAKE2bState = ForeignPtr BLAKE2bStruct

-- | Create a new hashing state.
initialize :: Int
           -- ^ Output length in bytes
           -> BLAKE2bState
initialize = initializer c_blake2b_init
{-# INLINE initialize #-}

-- | Create a new keyed hashing state.
initialize' :: Int
            -- ^ Output length in bytes
            -> ByteString
            -- ^ Key
            -> BLAKE2bState
initialize' = initializer' c_blake2b_init_key
{-# INLINE initialize' #-}

-- | Add data to the hashing state.
update :: ByteString
       -- ^ Data to hash
       -> BLAKE2bState
       -- ^ Hashing state
       -> BLAKE2bState
update = updater c_blake2b_update
{-# INLINE update #-}

-- | Finalize the hashing state.
finalize :: Int
         -- ^ Output length in bytes
         -> BLAKE2bState
         -- ^ Hashing state
         -> ByteString
finalize = finalizer c_blake2b_final
{-# INLINE finalize #-}

-- | Perform hashing all in one step. A common way of calling this function
--   is @hash 64 mempty dataToHash@ for applications which do not require
--   keying.
hash :: Int
     -- ^ Output length in bytes
     -> ByteString
     -- ^ Key
     -> ByteString
     -- ^ Data to hash
     -> ByteString
hash = hasher c_blake2b
{-# INLINE hash #-}

foreign import ccall unsafe "blake2.h blake2b"
  c_blake2b :: HashFunc
foreign import ccall unsafe "blake2.h blake2b_init"
  c_blake2b_init :: InitFunc BLAKE2bStruct
foreign import ccall unsafe "blake2.h blake2b_init_key"
  c_blake2b_init_key :: InitKeyFunc BLAKE2bStruct
foreign import ccall unsafe "blake2.h blake2b_update"
  c_blake2b_update :: UpdateFunc BLAKE2bStruct
foreign import ccall unsafe "blake2.h blake2b_final"
  c_blake2b_final :: FinalFunc BLAKE2bStruct
