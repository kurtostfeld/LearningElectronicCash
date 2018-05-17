{-# LANGUAGE ScopedTypeVariables #-}

module Modulo (
  gcd',
  egcd,
  modAdd,
  modMultiply,
  modMultInverse,
  modDivide,
  modPositiveExponentiation,
  modExponentiation,
  primeFactor,
  chineseRemainder,
  buildModSquareRootMap,
  findOrderBrute
) where

import Data.Maybe (fromJust, fromMaybe)
import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MultiMap (fromList)

-- Euclidean gcd.
-- g = gcd' a b
gcd' :: forall a. Integral a => a -> a -> a
gcd' a 0 = a
gcd' a b = let (q, r) = quotRem a b in gcd' b r

-- r_i = r_{i+1} \cdot q_{i+1} + r_{i+2}
-- r_{i+2} = r_i - r_{i+1} \cdot q_{i+1}
-- r_i = u_i \cdot a_i + v_i \cdot b_i
-- u_{i+2} = u_i - u_{i+1} \cdot q_{i+1}
egcd_int :: Integral a => a -> a -> a -> a -> (a, a)
egcd_int ui _ ri 0 = (ri, ui)
egcd_int ui ui1 ri ri1 =
  let (q, ri2) = quotRem ri ri1
      ui2 = ui - q * ui1
  in egcd_int ui1 ui2 ri1 ri2

-- Extended Euclidean gcd that provides coefficients
-- (g, u, v) = gcd' a b
-- such that a*u + b*v = g
egcd :: Integral a => a -> a -> (a, a, a)
egcd a 0 = (a, 1, 0)
egcd a b =
  let (g, u) = egcd_int 1 0 a b
      v = div (g - a*u) b
  in (g, u, v)

-- Simple modular addition and multiplication.
modAdd :: Integral a => a -> a -> a -> a
modAdd m x y = mod (x + y) m

modMultiply :: Integral a => a -> a -> a -> a
modMultiply m x y = mod (x * y) m

modMultInverse :: forall a. Integral a => a -> a -> Maybe a
modMultInverse m x =
  let (g, a, b) :: (a, a, a) = egcd m (x `mod` m)
  in if (g == 1) then Just $ mod b m else Nothing

modDivide :: Integral a => a -> a -> a -> Maybe a
modDivide m num denom =
  let dinv = modMultInverse m denom
  in (\x -> (x * num) `mod` m) <$> dinv

-- Calculates g^e mod n
-- Assumes n is positive.
-- Example: modPositiveExponentiation 73 13 247 -> 34
-- modPositiveExponentiation 839 13 347 -> 649
modPositiveExponentiation :: forall a. Integral a => a -> a -> a -> a
modPositiveExponentiation n g e = f e g 1
  where f :: a -> a -> a -> a
        f 0 _ acc = acc
        f expRemain gto2toi acc =
          let acc' = if mod expRemain 2 == 1 then modMultiply n acc gto2toi else acc
              gto2toi' = modMultiply n gto2toi gto2toi
              expRemain' = quot expRemain 2
          in f expRemain' gto2toi' acc'

-- Calculates g^e mod n
-- Will invert if e is a negative exponent.
modExponentiation :: forall a. Integral a => a -> a -> a -> a
modExponentiation n g e
  | e >= 0   = modPositiveExponentiation n g e
  | e < 0    = fromJust $ modMultInverse n $ modPositiveExponentiation n g (-e)

-- factorPower dividend -> divisor -> Maybe (exponent, quotient=dividend/divisor^exponent)
-- factorPower 8 3 -> Nothing
-- factorPower 3 3 -> Just (1,1)
-- factorPower 9 3 -> Just (2,1)
-- factorPower 27 3 -> Just (3,1)
-- factorPower 54 3 -> Just (3,2)
factorPower :: forall a. Integral a => a -> a -> Maybe (a, a)
factorPower n f =
  let (q,r) = n `quotRem` f
  in if (r == 0) then case factorPower q f of
    (Just (power, q')) -> Just (power + 1, q')
    Nothing -> Just (1, q)
    else Nothing

-- primeFactorInternal dividend -> minimumFactor -> [(factor, exponent)]
primeFactorInternal :: forall a. Integral a => a -> a -> [(a, a)]
primeFactorInternal 0 _ = []
primeFactorInternal 1 _ = []
primeFactorInternal n f = case factorPower n f of
  (Just (power, q)) -> (f, power) : primeFactorInternal q (f+1)
  Nothing -> primeFactorInternal n (f+1)

-- Simple, brute force prime factorization function.
-- primeFactor 54 -> [(2,1),(3,3)]
primeFactor :: forall a. Integral a => a -> [(a, a)]
primeFactor 0 = []
primeFactor 1 = [(1,1)]
primeFactor n = primeFactorInternal n 2


chineseRemainderInt :: forall a. Integral a => a -> a -> [(a, a)] -> a
chineseRemainderInt ci _ [] = ci
chineseRemainderInt ci mProduct ((a,m):xs) =
  let mProductInv = fromJust $ modMultInverse m mProduct
      y = modMultiply m mProductInv (a-ci)
      mProduct' = mProduct * m
      x = mProduct * y + ci `mod` mProduct'
  in chineseRemainderInt x mProduct' xs

chineseRemainder :: forall a. Integral a => [(a, a)] -> a
chineseRemainder xs = chineseRemainderInt 0 1 xs

-- Calculate Square Map
-- Square Root Map of (x^2 -> x) in Z/mZ.
buildModSquareRootMap :: Integer -> MultiMap Integer Integer
buildModSquareRootMap m =
  let ns = [0..(m-1)]
      nsqpairs = fmap (\x -> (x^2 `mod` m, x)) ns
  in MultiMap.fromList nsqpairs

-- Find the order of an integer in Z/mZ
findOrderBrute :: forall a. Integral a => a -> a -> a
findOrderBrute m n = f n 1
  where f :: a -> a -> a
        f 0 _ = 0
        f 1 k = k
        f kn k = f (kn * n `mod` m) (k + 1)
