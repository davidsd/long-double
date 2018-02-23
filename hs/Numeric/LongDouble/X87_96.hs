{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.LongDouble.X87_128
-- Copyright   :  (C) 2018 Claude Heiland-Allen
-- License     :  BSD3
-- Maintainer  :  Claude Heiland-Allen <claude@mathr.co.uk>
-- Stability   :  experimental
-- Portability :  non-portable (assumes x87 with sizeof(long double) == 12)
--
-- This module contains a LongDouble type that can be used if you need that
-- extra precision or range from the x87 FPU extended precision type.
-- It has 15bit signed exponent (compared to 11bit signed exponent for Double)
-- and 64bit mantissa (compared to 53bit mantissa for Double).
--
-- Performance is likely to be poor, as the instances are implemented using
-- FFI with Ptr LongDouble, copying to and from memory around each operation.
-- If you need to bind to functions taking/returning long double you need to
-- write wrapper functions expecting pointers to long double instead, as GHC
-- does not expose a CLDouble FFI type.
-- See <https://ghc.haskell.org/trac/ghc/ticket/3353>.
--
-- Most code should import Numeric.LongDouble instead, unless a specific ABI
-- is needed.  This module is for x87 with sizeof(long double) == 12.
module Numeric.LongDouble.X87_96
  ( 
  -- * long double data type
    LongDouble(..)
  -- * RealFrac alternatives
  , truncate'
  , round'
  , ceiling'
  , floor'
  -- * Conversions
  , fromDouble
  , toDouble
  , fromInt
  , toInt
  ) where

import Data.Bits (bit, testBit, (.&.), shiftL, shiftR)
import Data.Ratio ((%), numerator, denominator)
import Data.Word (Word64, Word32)
import Foreign (Ptr, castPtr, with, alloca)
import Foreign.C.Types (CIntMax(..), CInt(..), CDouble(..))
import Foreign.Storable (Storable(..))
import Numeric (showFloat, readFloat, readSigned)
import System.IO.Unsafe (unsafePerformIO)
import GHC.Exts (Int(..))
import GHC.Integer.Logarithms (integerLog2#)

-- | The long double type: 80bits of x87 FPU data in 96bits of space.
data LongDouble = LD !Word64 !Word32

instance Storable LongDouble where
  sizeOf _ = sizeOf (0 :: Word64) + sizeOf (0 :: Word32)
  alignment _ = alignment (0 :: Word32)
  peek p = do
    let q :: Ptr Word64
        q = castPtr p
        r :: Ptr Word32
        r = castPtr p
    a <- peekElemOff q 0
    b <- peekElemOff r 2
    return $ LD a b
  poke p (LD a b) = do
    let q :: Ptr Word64
        q = castPtr p
        r :: Ptr Word32
        r = castPtr p
    pokeElemOff q 0 a
    pokeElemOff r 2 b

#include "X87.hs"
