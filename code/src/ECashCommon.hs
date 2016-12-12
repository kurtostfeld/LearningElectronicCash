{-# LANGUAGE ScopedTypeVariables #-}

module ECashCommon where

import BinaryUtilities
import Modulo
import RandomUtilities

import Data.Bits

calculateBlinds :: Int -> Int -> Integer -> [Int] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer]
calculateBlinds memberId transactionCounter bankRsaN indices0 ai ci di ri =
  let chaumF' = chaumF bankRsaN
      chaumG' = chaumG bankRsaN      
      xi = zipWith chaumG' ai ci
      -- (v + i)
      vplusi = fmap (+(transactionCounter + 1)) indices0
      -- (u || (v + i))
      ucatvi = fmap fromIntegral $ zipWith pack32x2To64 (repeat memberId) vplusi
      -- a_i `xor` (u || (v + i))
      ai_xor_ucatvi = zipWith xor ai ucatvi
      yi = zipWith chaumG' ai_xor_ucatvi di
      f_xi_yi = zipWith chaumF' xi yi
      ri_cubed = fmap (\x -> modExponentiation bankRsaN x 3) ri
      blindedCandidates = zipWith (modMultiply bankRsaN) ri_cubed f_xi_yi
  in blindedCandidates

