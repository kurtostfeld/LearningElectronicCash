{-# LANGUAGE ScopedTypeVariables #-}

module MillerRabin where

import Modulo
import Data.List (find)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)

-- n = 2^k * q.
-- factorPowerOfTwo 0 48 -> (4, 3) -- 2^4 * 3 = 48
factorPowerOfTwo :: forall a. Integral a => a -> a -> (a, a)
factorPowerOfTwo k n
  | rem == 1  = (k, n)
  | otherwise = factorPowerOfTwo (k+1) quot
  where (quot, rem) = n `quotRem` 2


millerRabinWitnessLoop :: forall a. Integral a => a -> a -> a -> Bool
millerRabinWitnessLoop n ai count
  | ai == (n-1) = False -- Can be prime.
  | count == 0  = True -- Must be composite
  | otherwise   = millerRabinWitnessLoop n ((ai * ai) `mod` n) (count - 1)
  

millerRabinIsCompositeWitness :: forall a. Integral a => a -> a -> Bool
millerRabinIsCompositeWitness n witness
  | n `mod` 2 == 0     = True -- Even number. Composite.
  | (1 < d) && (d < n) = True
  | a0 == 1            = False -- This can be prime.
  | otherwise = millerRabinWitnessLoop n a0 k
  where d = gcd' n witness
        (k, q) = factorPowerOfTwo 0 (n-1)
        a0 = modPositiveExponentiation n witness q


millerRabinIsProbablyPrime :: forall a. Integral a => a -> Bool
millerRabinIsProbablyPrime n = isNothing $ find (\i -> millerRabinIsCompositeWitness n i) [5..25]

-- True
-- millerRabinIsCompositeWitness 561 2

-- False
-- millerRabinIsCompositeWitness 172947529 17
-- millerRabinIsCompositeWitness 172947529 3
-- True
-- millerRabinIsCompositeWitness 172947529 23

