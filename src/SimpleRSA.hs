{-# LANGUAGE ScopedTypeVariables #-}

module SimpleRSA where

import Data.List (find)
import Data.Maybe (fromJust)
import System.Random

import MillerRabin
import Modulo

-- 2310K + 1139
simplePrimeGen :: RandomGen g => g -> (Integer, g)
simplePrimeGen gen =
  let range :: (Integer, Integer) = (100000, 1000000)
      -- list of random numbers
      rlist = iterate (\(n, g) -> randomR range g) $ randomR range gen
      -- list of random prime candidates
      clist = fmap (\(r, g) -> (2310 * r + 1139, g)) rlist
  in fromJust $ find (\(c, gen') -> millerRabinIsProbablyPrime c) clist

-- (p,q,e)
type SimpleRsaPrivateKey = (Integer, Integer, Integer)
-- (n,e)
type SimpleRsaPublicKey = (Integer, Integer)

simpleRsaPrivateKeyGen :: RandomGen g => g -> (SimpleRsaPrivateKey, g)
simpleRsaPrivateKeyGen gen1 =
  let (p, gen2) = simplePrimeGen gen1
      (q, gen3) = simplePrimeGen gen2
      totient = (p-1)*(q-1)
      range :: (Integer, Integer) = (2, totient - 1)
      rlist = iterate (\(r, g) -> randomR range g) $ randomR range gen3
      (e, gen4) = fromJust $ find (\(r, g) -> gcd' r totient == 1) rlist
  in ((p, q, e), gen4)

simpleRsaCreatePublic :: SimpleRsaPrivateKey -> SimpleRsaPublicKey
simpleRsaCreatePublic (p,q,e) = (p*q,e)

simpleRsaEncrypt :: SimpleRsaPublicKey -> Integer -> Integer
simpleRsaEncrypt (n,e) m = modExponentiation n m e

simpleRsaDecrypt :: SimpleRsaPrivateKey -> Integer -> Integer
simpleRsaDecrypt (p,q,e) c =
  let d = fromJust $ modMultInverse ((p-1)*(q-1)) e
      n = p*q
  in modExponentiation n c d

simpleRsaCubeRoot :: SimpleRsaPrivateKey -> Integer -> Integer
simpleRsaCubeRoot (p,q,e) c =
  let d = fromJust $ modMultInverse ((p-1)*(q-1)) 3
      n = p*q
  in modExponentiation n c d
