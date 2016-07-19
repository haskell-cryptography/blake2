----------------------------------------------------------------
-- |
-- Module      : Crypto.Hash.BLAKE2.BLAKE2s
-- Maintainer  : John Galt <jgalt@centromere.net>
-- Stability   : experimental
-- Portability : POSIX

module Crypto.Hash.BLAKE2.BLAKE2s
  ( -- * Types
    BLAKE2sState,
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

data BLAKE2sStruct

instance Storable BLAKE2sStruct where
  sizeOf    _ = #{size blake2s_state}
  alignment _ = #{alignment blake2s_state}
  peek        = error "peek not implemented"
  poke        = error "poke not implemented"

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- | The hash state.
type BLAKE2sState = ForeignPtr BLAKE2sStruct

-- | Create a new hashing state.
initialize :: Int
           -- ^ Output length in bytes
           -> BLAKE2sState
initialize = initializer c_blake2s_init
{-# INLINE initialize #-}

-- | Create a new keyed hashing state.
initialize' :: Int
            -- ^ Output length in bytes
            -> ByteString
            -- ^ Key
            -> BLAKE2sState
initialize' = initializer' c_blake2s_init_key
{-# INLINE initialize' #-}

-- | Add data to the hashing state.
update :: ByteString
       -- ^ Data to hash
       -> BLAKE2sState
       -- ^ Hashing state
       -> BLAKE2sState
update = updater c_blake2s_update
{-# INLINE update #-}

-- | Finalize the hashing state.
finalize :: Int
         -- ^ Output length in bytes
         -> BLAKE2sState
         -- ^ Hashing state
         -> ByteString
finalize = finalizer c_blake2s_final
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
hash = hasher c_blake2s
{-# INLINE hash #-}

foreign import ccall unsafe "blake2.h blake2s"
  c_blake2s :: HashFunc
foreign import ccall unsafe "blake2.h blake2s_init"
  c_blake2s_init :: InitFunc BLAKE2sStruct
foreign import ccall unsafe "blake2.h blake2s_init_key"
  c_blake2s_init_key :: InitKeyFunc BLAKE2sStruct
foreign import ccall unsafe "blake2.h blake2s_update"
  c_blake2s_update :: UpdateFunc BLAKE2sStruct
foreign import ccall unsafe "blake2.h blake2s_final"
  c_blake2s_final :: FinalFunc BLAKE2sStruct
