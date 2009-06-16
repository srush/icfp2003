module FPInt (FPInt, mul_fp, div_fp, pi_fp, pid2_fp, pi2_fp, sin_fp, cos_fp,
              sqr_fp, normAng_fp, int2fp, fp2int) where

import Data.Int
import Data.Bits

-- Fixed Point Int
type FPInt = Int64

pid2_fp :: FPInt
pid2_fp = 102944
pi_fp :: FPInt
pi_fp = 205887
pi2_fp :: FPInt
pi2_fp = 411775


largest_fp :: FPInt
largest_fp = 1 `shiftL` 32 - 1

-- inlines are pretty nasty, but they speed it up significantly (2x faster)

mul_fp :: FPInt -> FPInt -> FPInt
{-# INLINE mul_fp #-}
mul_fp x y = (x * y) `shiftR` 16

div_fp :: FPInt -> FPInt -> FPInt
{-# INLINE div_fp #-}
div_fp x y = (x `shiftL` 16) `div` y

sin_fp :: FPInt -> FPInt
{-# INLINE sin_fp #-}
sin_fp x | x < 0 = -sin_fp(-x)
         | x > pid2_fp = sin_fp (pi_fp - x)
         | otherwise = x - (x3 `div` 6) + (x5 `div` 120) - (x7 `div` 5040) 
  where
    x2 = x `mul_fp` x
    x3 = x `mul_fp` x2
    x5 = x3 `mul_fp` x2
    x7 = x5 `mul_fp` x2

cos_fp :: FPInt -> FPInt
{-# INLINE cos_fp #-}
cos_fp x | x' > pi_fp = sin_fp (x' - pi2_fp)
         | otherwise = sin_fp x'
  where
    x' = x + pid2_fp

sqr_fp :: FPInt -> FPInt
{-# INLINE sqr_fp #-}
sqr_fp x = x `mul_fp` x

normAng_fp :: FPInt -> FPInt
normAng_fp a | a < -pi_fp = normAng_fp (a + pi2_fp)
             | a > pi_fp = normAng_fp (a - pi2_fp)
             | otherwise = a

int2fp :: Int -> FPInt
int2fp i = fromIntegral (i * 65536)

fp2int :: FPInt -> Int
{-# INLINE fp2int #-}
fp2int f = fromIntegral (f `shiftR` 16)

fp2dbl :: FPInt -> Double
fp2dbl f = (fromIntegral f) / 65536



