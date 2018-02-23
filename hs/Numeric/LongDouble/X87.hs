instance Eq LongDouble where
  (==) = cmp ld_eq
  (/=) = cmp ld_ne

instance Ord LongDouble where
  (<=) = cmp ld_le
  (< ) = cmp ld_lt
  (>=) = cmp ld_ge
  (> ) = cmp ld_gt
  min  = f2 ld_min
  max  = f2 ld_max

instance Num LongDouble where
  fromInteger z = encodeFloat z 0
  negate = f1 ld_neg
  (+) = f2 ld_add
  (-) = f2 ld_sub
  (*) = f2 ld_mul
  abs = f1 ld_abs
  signum = f1 ld_sgn

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
  (/) = f2 ld_div
  recip = f1 ld_recip

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
truncate' = f1 ld_trunc
round'    = f1 ld_round
ceiling'  = f1 ld_ceil
floor'    = f1 ld_floor

instance Floating LongDouble where
  pi = unsafePerformIO $ do
    alloca $ \lp -> do
      ld_pi lp
      peek lp
  exp = f1 ld_exp
  log = f1 ld_log
  sqrt = f1 ld_sqrt
  (**) = f2 ld_pow
  -- logBase
  sin = f1 ld_sin
  cos = f1 ld_cos
  tan = f1 ld_tan
  sinh = f1 ld_sinh
  cosh = f1 ld_cosh
  tanh = f1 ld_tanh
  asin = f1 ld_asin
  acos = f1 ld_acos
  atan = f1 ld_atan
  asinh = f1 ld_asinh
  acosh = f1 ld_acosh
  atanh = f1 ld_atanh

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
      ld_frexp lp lp ep
      peek lp

  scaleFloat e l = unsafePerformIO $ do
    with l $ \lp -> do
      ld_ldexp lp lp (fromIntegral e)
      peek lp

  isNaN = tst ld_isnan
  isInfinite = tst ld_isinf
  isDenormalized = tst ld_isdenorm
  isNegativeZero = tst ld_isnegzero
  isIEEE _= True

  atan2 = f2 ld_atan2

instance Read LongDouble where
  readsPrec _ = readSigned readFloat

instance Show LongDouble where
  showsPrec _ = showFloat

fromInt :: Int -> LongDouble
fromInt i = unsafePerformIO $ do
  alloca $ \lp -> do
    ld_set_i lp (fromIntegral i)
    peek lp

toInt :: LongDouble -> Int
toInt l = unsafePerformIO $ with l ld_get_i

fromDouble :: Double -> LongDouble
fromDouble i = unsafePerformIO $ do
  alloca $ \lp -> do
    ld_set_d lp i
    peek lp

toDouble :: LongDouble -> Double
toDouble l = unsafePerformIO $ with l ld_get_d

f2 :: F2 -> LongDouble -> LongDouble -> LongDouble
f2 f a b = unsafePerformIO $ do
  with a $ \ap -> with b $ \bp -> alloca $ \rp -> do
    f rp ap bp
    peek rp

type F2 = Ptr LongDouble -> Ptr LongDouble -> Ptr LongDouble -> IO ()

foreign import ccall unsafe "ld_add"   ld_add   :: F2
foreign import ccall unsafe "ld_sub"   ld_sub   :: F2
foreign import ccall unsafe "ld_mul"   ld_mul   :: F2
foreign import ccall unsafe "ld_div"   ld_div   :: F2
foreign import ccall unsafe "ld_pow"   ld_pow   :: F2
foreign import ccall unsafe "ld_min"   ld_min   :: F2
foreign import ccall unsafe "ld_max"   ld_max   :: F2
foreign import ccall unsafe "ld_atan2" ld_atan2 :: F2

f1 :: F1 -> LongDouble -> LongDouble
f1 f a = unsafePerformIO $ do
  with a $ \ap -> alloca $ \rp -> do
    f rp ap
    peek rp

type F1 = Ptr LongDouble -> Ptr LongDouble -> IO ()

foreign import ccall unsafe "ld_abs"   ld_abs   :: F1
foreign import ccall unsafe "ld_sgn"   ld_sgn   :: F1
foreign import ccall unsafe "ld_neg"   ld_neg   :: F1
foreign import ccall unsafe "ld_sqrt"  ld_sqrt  :: F1
foreign import ccall unsafe "ld_recip" ld_recip :: F1
foreign import ccall unsafe "ld_exp"   ld_exp   :: F1
foreign import ccall unsafe "ld_log"   ld_log   :: F1
foreign import ccall unsafe "ld_sin"   ld_sin   :: F1
foreign import ccall unsafe "ld_cos"   ld_cos   :: F1
foreign import ccall unsafe "ld_tan"   ld_tan   :: F1
foreign import ccall unsafe "ld_sinh"  ld_sinh  :: F1
foreign import ccall unsafe "ld_cosh"  ld_cosh  :: F1
foreign import ccall unsafe "ld_tanh"  ld_tanh  :: F1
foreign import ccall unsafe "ld_asin"  ld_asin  :: F1
foreign import ccall unsafe "ld_acos"  ld_acos  :: F1
foreign import ccall unsafe "ld_atan"  ld_atan  :: F1
foreign import ccall unsafe "ld_asinh" ld_asinh :: F1
foreign import ccall unsafe "ld_acosh" ld_acosh :: F1
foreign import ccall unsafe "ld_atanh" ld_atanh :: F1
foreign import ccall unsafe "ld_floor" ld_floor :: F1
foreign import ccall unsafe "ld_ceil"  ld_ceil  :: F1
foreign import ccall unsafe "ld_round" ld_round :: F1
foreign import ccall unsafe "ld_trunc" ld_trunc :: F1

type CMP = Ptr LongDouble -> Ptr LongDouble -> IO CInt

cmp :: CMP -> LongDouble -> LongDouble -> Bool
cmp f a b = unsafePerformIO $ do
  with a $ \ap -> with b $ \bp -> do
    r <- f ap bp
    return (r /= 0)

foreign import ccall unsafe "ld_eq" ld_eq :: CMP
foreign import ccall unsafe "ld_ne" ld_ne :: CMP
foreign import ccall unsafe "ld_lt" ld_lt :: CMP
foreign import ccall unsafe "ld_le" ld_le :: CMP
foreign import ccall unsafe "ld_gt" ld_gt :: CMP
foreign import ccall unsafe "ld_ge" ld_ge :: CMP

type TST = Ptr LongDouble -> IO CInt

tst :: TST -> LongDouble -> Bool
tst f a = unsafePerformIO $ do
  with a $ \ap -> do
    r <- f ap
    return (r /= 0)

foreign import ccall unsafe "ld_isnan" ld_isnan :: TST
foreign import ccall unsafe "ld_isinf" ld_isinf :: TST
foreign import ccall unsafe "ld_isdenorm" ld_isdenorm :: TST
foreign import ccall unsafe "ld_isnegzero" ld_isnegzero :: TST

foreign import ccall unsafe "ld_get_d" ld_get_d :: Ptr LongDouble -> IO Double
foreign import ccall unsafe "ld_get_i" ld_get_i :: Ptr LongDouble -> IO Int

foreign import ccall unsafe "ld_set_d" ld_set_d :: Ptr LongDouble -> Double -> IO ()
foreign import ccall unsafe "ld_set_i" ld_set_i :: Ptr LongDouble -> Int -> IO ()

foreign import ccall unsafe "ld_ldexp" ld_ldexp :: Ptr LongDouble -> Ptr LongDouble -> CInt -> IO ()
foreign import ccall unsafe "ld_frexp" ld_frexp :: Ptr LongDouble -> Ptr LongDouble -> Ptr CInt -> IO ()

foreign import ccall unsafe "ld_pi"    ld_pi    :: Ptr LongDouble -> IO ()
