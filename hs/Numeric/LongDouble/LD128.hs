{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.LongDouble.LD128
-- Copyright   :  (C) 2018 Claude Heiland-Allen
-- License     :  BSD3
-- Maintainer  :  Claude Heiland-Allen <claude@mathr.co.uk>
-- Stability   :  experimental
-- Portability :  non-portable (assumes x87 with sizeof(long double) == 16)
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
module Numeric.LongDouble.LD128
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
import Data.Word (Word64)
import Foreign (Ptr, castPtr, with, alloca)
import Foreign.C.Types (CIntMax(..), CInt(..), CDouble(..))
import Foreign.Storable (Storable(..))
import Numeric (showFloat, readFloat, readSigned)
import System.IO.Unsafe (unsafePerformIO)
import GHC.Exts (Int(..))
import GHC.Integer.Logarithms (integerLog2#)

-- | The long double type: 80bits of x87 FPU data in 128bits of space.
data LongDouble = LD !Word64 !Word64

instance Storable LongDouble where
  sizeOf _ = 2 * sizeOf (0 :: Word64)
  alignment _ = alignment (0 :: Word64)
  peek p = do
    let q :: Ptr Word64
        q = castPtr p
    a <- peekElemOff q 0
    b <- peekElemOff q 1
    return $ LD a b
  poke p (LD a b) = do
    let q :: Ptr Word64
        q = castPtr p
    pokeElemOff q 0 a
    pokeElemOff q 1 b

instance Eq LongDouble where
  (==) = cmp ld128_eq
  (/=) = cmp ld128_ne

instance Ord LongDouble where
  (<=) = cmp ld128_le
  (< ) = cmp ld128_lt
  (>=) = cmp ld128_ge
  (> ) = cmp ld128_gt
  min  = f2 ld128_min
  max  = f2 ld128_max

instance Num LongDouble where
  fromInteger z = encodeFloat z 0
  negate = f1 ld128_neg
  (+) = f2 ld128_add
  (-) = f2 ld128_sub
  (*) = f2 ld128_mul
  abs = f1 ld128_abs
  signum = f1 ld128_sgn

instance Real LongDouble where
  toRational l = case decodeFloat l of
    (m, e)
      | e >= 0 -> m `shiftL` e % 1
      | otherwise -> m % bit (negate e)

instance Fractional LongDouble where
  fromRational q = -- FIXME accuracy?
    let a = fromInteger (numerator q) / fromInteger (denominator q)
        r = q - toRational a
        b = fromInteger (numerator r) / fromInteger (denominator r)
    in  a + b
  (/) = f2 ld128_div
  recip = f1 ld128_recip

instance RealFrac LongDouble where
  properFraction l
    | l >= 0 = let n' = floor' l
                   f = l - n'
               in  (fromInteger . toInteger' $ n', f)
    
    | l <  0 = let n' = ceiling' l
                   f = l - n'
               in  (fromInteger . toInteger' $ n', f)
    | otherwise = (0, l) -- NaN
  truncate = fromInteger . toInteger' . truncate'
  round    = fromInteger . toInteger' . round'
  ceiling  = fromInteger . toInteger' . ceiling'
  floor    = fromInteger . toInteger' . floor'

toInteger' :: LongDouble -> Integer
toInteger' l = case decodeFloat l of
  (m, e)
    | e >= 0 -> m `shiftL` e
    | otherwise -> m `shiftR` negate e

-- | Alternate versions of RealFrac methods that
--   keep the value as a long double.
truncate', round', ceiling', floor' :: LongDouble -> LongDouble
truncate' = f1 ld128_trunc
round'    = f1 ld128_round
ceiling'  = f1 ld128_ceil
floor'    = f1 ld128_floor

instance Floating LongDouble where
  pi = unsafePerformIO $ do
    alloca $ \lp -> do
      ld128_pi lp
      peek lp
  exp = f1 ld128_exp
  log = f1 ld128_log
  sqrt = f1 ld128_sqrt
  (**) = f2 ld128_pow
  -- logBase
  sin = f1 ld128_sin
  cos = f1 ld128_cos
  tan = f1 ld128_tan
  sinh = f1 ld128_sinh
  cosh = f1 ld128_cosh
  tanh = f1 ld128_tanh
  asin = f1 ld128_asin
  acos = f1 ld128_acos
  atan = f1 ld128_atan
  asinh = f1 ld128_asinh
  acosh = f1 ld128_acosh
  atanh = f1 ld128_atanh

instance RealFloat LongDouble where
  floatRadix _ = 2
  floatDigits _ = 64
  floatRange _ = (-16381,16384) -- FIXME verify?

  decodeFloat l@(LD a b)
    | isNaN l = (0, 0)
    | isInfinite l = (0, 0)
    | l == 0 = (0, 0)
    | isDenormalized l = case decodeFloat (scaleFloat 128 l) of
        (m, e) -> (m, e - 128)
    | otherwise =
        ( (if s then negate else id) (fromIntegral a)
        , fromIntegral e0 - 16383 - 63
        )
    where
      s = b `testBit` 15
      e0 = b .&. (bit 15 - 1)

  encodeFloat m e
    | m == 0 = LD 0 0
    | m <  0 = negate (encodeFloat (negate m) e)
    | b >= bit 15 - 1 = LD (bit 63) (bit 15 - 1) -- overflow to infinity
    | b <= 0 = scaleFloat (b - 128) (encodeFloat m (e - b + 128)) -- denormal
    | otherwise = LD a (fromIntegral b) -- normal
    where
      l = I# (integerLog2# m)
      t = l - 63
      a | t >= 0    = fromInteger (m `shiftR`        t)
        | otherwise = fromInteger (m `shiftL` negate t)
      b = e + t + 16383 + 63

  exponent l@(LD _ b)
    | isNaN l = 0
    | isInfinite l = 0
    | l == 0 = 0
    | isDenormalized l = snd (decodeFloat l) + 64
    | otherwise = fromIntegral e0 - 16383 - 63 + 64
    where
      e0 = b .&. (bit 15 - 1)

  significand l = unsafePerformIO $ do
    with l $ \lp -> alloca $ \ep -> do
      ld128_frexp lp lp ep
      peek lp

  scaleFloat e l = unsafePerformIO $ do
    with l $ \lp -> do
      ld128_ldexp lp lp (fromIntegral e)
      peek lp

  isNaN = tst ld128_isnan
  isInfinite = tst ld128_isinf
  isDenormalized = tst ld128_isdenorm
  isNegativeZero = tst ld128_isnegzero
  isIEEE _= True

  atan2 = f2 ld128_atan2

instance Read LongDouble where
  readsPrec _ = readSigned readFloat

instance Show LongDouble where
  showsPrec _ = showFloat

fromInt :: Int -> LongDouble
fromInt i = unsafePerformIO $ do
  alloca $ \lp -> do
    ld128_set_i lp (fromIntegral i)
    peek lp

toInt :: LongDouble -> Int
toInt l = unsafePerformIO $ with l ld128_get_i

fromDouble :: Double -> LongDouble
fromDouble i = unsafePerformIO $ do
  alloca $ \lp -> do
    ld128_set_d lp i
    peek lp

toDouble :: LongDouble -> Double
toDouble l = unsafePerformIO $ with l ld128_get_d

f2 :: F2 -> LongDouble -> LongDouble -> LongDouble
f2 f a b = unsafePerformIO $ do
  with a $ \ap -> with b $ \bp -> alloca $ \rp -> do
    f rp ap bp
    peek rp

type F2 = Ptr LongDouble -> Ptr LongDouble -> Ptr LongDouble -> IO ()

foreign import ccall unsafe "ld128_add"   ld128_add   :: F2
foreign import ccall unsafe "ld128_sub"   ld128_sub   :: F2
foreign import ccall unsafe "ld128_mul"   ld128_mul   :: F2
foreign import ccall unsafe "ld128_div"   ld128_div   :: F2
foreign import ccall unsafe "ld128_pow"   ld128_pow   :: F2
foreign import ccall unsafe "ld128_min"   ld128_min   :: F2
foreign import ccall unsafe "ld128_max"   ld128_max   :: F2
foreign import ccall unsafe "ld128_atan2" ld128_atan2 :: F2

f1 :: F1 -> LongDouble -> LongDouble
f1 f a = unsafePerformIO $ do
  with a $ \ap -> alloca $ \rp -> do
    f rp ap
    peek rp

type F1 = Ptr LongDouble -> Ptr LongDouble -> IO ()

foreign import ccall unsafe "ld128_abs"   ld128_abs   :: F1
foreign import ccall unsafe "ld128_sgn"   ld128_sgn   :: F1
foreign import ccall unsafe "ld128_neg"   ld128_neg   :: F1
foreign import ccall unsafe "ld128_sqrt"  ld128_sqrt  :: F1
foreign import ccall unsafe "ld128_recip" ld128_recip :: F1
foreign import ccall unsafe "ld128_exp"   ld128_exp   :: F1
foreign import ccall unsafe "ld128_log"   ld128_log   :: F1
foreign import ccall unsafe "ld128_sin"   ld128_sin   :: F1
foreign import ccall unsafe "ld128_cos"   ld128_cos   :: F1
foreign import ccall unsafe "ld128_tan"   ld128_tan   :: F1
foreign import ccall unsafe "ld128_sinh"  ld128_sinh  :: F1
foreign import ccall unsafe "ld128_cosh"  ld128_cosh  :: F1
foreign import ccall unsafe "ld128_tanh"  ld128_tanh  :: F1
foreign import ccall unsafe "ld128_asin"  ld128_asin  :: F1
foreign import ccall unsafe "ld128_acos"  ld128_acos  :: F1
foreign import ccall unsafe "ld128_atan"  ld128_atan  :: F1
foreign import ccall unsafe "ld128_asinh" ld128_asinh :: F1
foreign import ccall unsafe "ld128_acosh" ld128_acosh :: F1
foreign import ccall unsafe "ld128_atanh" ld128_atanh :: F1
foreign import ccall unsafe "ld128_floor" ld128_floor :: F1
foreign import ccall unsafe "ld128_ceil"  ld128_ceil  :: F1
foreign import ccall unsafe "ld128_round" ld128_round :: F1
foreign import ccall unsafe "ld128_trunc" ld128_trunc :: F1

type CMP = Ptr LongDouble -> Ptr LongDouble -> IO CInt

cmp :: CMP -> LongDouble -> LongDouble -> Bool
cmp f a b = unsafePerformIO $ do
  with a $ \ap -> with b $ \bp -> do
    r <- f ap bp
    return (r /= 0)

foreign import ccall unsafe "ld128_eq" ld128_eq :: CMP
foreign import ccall unsafe "ld128_ne" ld128_ne :: CMP
foreign import ccall unsafe "ld128_lt" ld128_lt :: CMP
foreign import ccall unsafe "ld128_le" ld128_le :: CMP
foreign import ccall unsafe "ld128_gt" ld128_gt :: CMP
foreign import ccall unsafe "ld128_ge" ld128_ge :: CMP

type TST = Ptr LongDouble -> IO CInt

tst :: TST -> LongDouble -> Bool
tst f a = unsafePerformIO $ do
  with a $ \ap -> do
    r <- f ap
    return (r /= 0)

foreign import ccall unsafe "ld128_isnan" ld128_isnan :: TST
foreign import ccall unsafe "ld128_isinf" ld128_isinf :: TST
foreign import ccall unsafe "ld128_isdenorm" ld128_isdenorm :: TST
foreign import ccall unsafe "ld128_isnegzero" ld128_isnegzero :: TST

foreign import ccall unsafe "ld128_get_d" ld128_get_d :: Ptr LongDouble -> IO Double
foreign import ccall unsafe "ld128_get_i" ld128_get_i :: Ptr LongDouble -> IO Int

foreign import ccall unsafe "ld128_set_d" ld128_set_d :: Ptr LongDouble -> Double -> IO ()
foreign import ccall unsafe "ld128_set_i" ld128_set_i :: Ptr LongDouble -> Int -> IO ()

foreign import ccall unsafe "ld128_ldexp" ld128_ldexp :: Ptr LongDouble -> Ptr LongDouble -> CInt -> IO ()
foreign import ccall unsafe "ld128_frexp" ld128_frexp :: Ptr LongDouble -> Ptr LongDouble -> Ptr CInt -> IO ()

foreign import ccall unsafe "ld128_pi"    ld128_pi    :: Ptr LongDouble -> IO ()
