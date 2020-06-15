{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.LongDouble.ARM_128
-- Copyright   :  (C) 2020 Claude Heiland-Allen
-- License     :  BSD3
-- Maintainer  :  Claude Heiland-Allen <claude@mathr.co.uk>
-- Stability   :  experimental
-- Portability :  non-portable (assumes long double == _Float128)
--
-- This module contains a LongDouble type that is the same as Float128, as on aarch64.
--
-- Most code should import Numeric.LongDouble instead, unless a specific ABI
-- is needed.  This module is for sizeof(long double) == 16, with the type being
-- a simple alias for _Float128.
module Numeric.LongDouble.ARM_128
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

import Data.Coerce (coerce)
import Foreign (Ptr, with, alloca)
import qualified Numeric.Float128 as F
import Foreign.Storable (Storable(..))
import Numeric (showFloat, readFloat, readSigned)
import System.IO.Unsafe (unsafePerformIO)

-- | The long double type on aarch64: 128 bits of _Float128 in 128bits of space.
newtype LongDouble = LD F.Float128
  deriving (Storable, Eq, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat)

instance Read LongDouble where
  readsPrec p = coerce . (readsPrec p :: ReadS F.Float128)

instance Show LongDouble where
  showsPrec p (LD x) = showsPrec p x

fromInt :: Int -> LongDouble
fromInt = LD . F.fromInt

toInt :: LongDouble -> Int
toInt (LD f) = F.toInt f

fromDouble :: Double -> LongDouble
fromDouble = LD . F.fromDouble

toDouble :: LongDouble -> Double
toDouble (LD f) = F.toDouble f

-- | Alternate versions of RealFrac methods that
--   keep the value as a long double.
truncate', round', ceiling', floor' :: LongDouble -> LongDouble
truncate' = coerce F.truncate'
round'    = coerce F.round'
ceiling'  = coerce F.ceiling'
floor'    = coerce F.floor'
