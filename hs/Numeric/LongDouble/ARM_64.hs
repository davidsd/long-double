{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.LongDouble.ARM_64
-- Copyright   :  (C) 2018 Claude Heiland-Allen
-- License     :  BSD3
-- Maintainer  :  Claude Heiland-Allen <claude@mathr.co.uk>
-- Stability   :  experimental
-- Portability :  non-portable (assumes sizeof(long double) == 8)
--
-- This module contains a LongDouble type that is the same as Double, as on ARM.
--
-- Most code should import Numeric.LongDouble instead, unless a specific ABI
-- is needed.  This module is for sizeof(long double) == 8, with the type being
-- a simple alias for double.
module Numeric.LongDouble.ARM_64
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
import Foreign.C.Types (CDouble(..))
import Foreign.Storable (Storable(..))
import Numeric (showFloat, readFloat, readSigned)
import System.IO.Unsafe (unsafePerformIO)

-- | The long double type on ARM: 64bits of double in 64bits of space.
newtype LongDouble = LD Double
  deriving (Storable, Eq, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat)

instance Read LongDouble where
  readsPrec p = coerce . (readsPrec p :: ReadS Double)

instance Show LongDouble where
  showsPrec p (LD x) = showsPrec p x

fromInt :: Int -> LongDouble
fromInt = fromIntegral

toInt :: LongDouble -> Int
toInt = truncate

fromDouble :: Double -> LongDouble
fromDouble = coerce

toDouble :: LongDouble -> Double
toDouble = coerce

-- | Alternate versions of RealFrac methods that
--   keep the value as a long double.
truncate', round', ceiling', floor' :: LongDouble -> LongDouble
truncate' = f1 d_trunc
round'    = f1 d_round
ceiling'  = f1 d_ceil
floor'    = f1 d_floor

f1 :: (CDouble -> IO CDouble) -> LongDouble -> LongDouble
f1 f a = unsafePerformIO $ do
  r <- f (coerce a)
  return (coerce r)

foreign import ccall unsafe "math.h floor" d_floor :: CDouble -> IO CDouble
foreign import ccall unsafe "math.h ceil"  d_ceil  :: CDouble -> IO CDouble
foreign import ccall unsafe "math.h round" d_round :: CDouble -> IO CDouble
foreign import ccall unsafe "math.h trunc" d_trunc :: CDouble -> IO CDouble
