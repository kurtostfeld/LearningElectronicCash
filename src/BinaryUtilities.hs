{-# LANGUAGE ScopedTypeVariables #-}

module BinaryUtilities where

import Modulo
import SimpleRSA

import Data.Bits
import System.Random

       -- 32-bit Int packing/unpacking
minInt32 :: Int = -(shiftL 1 31)
maxInt32 :: Int = shiftL 1 31 - 1
maxInt64 :: Int = shiftL 1 63 - 1 

-- Not optimized. There is likely room for a more optimized implementation.
-- Rounds down. 1 -> 0. 2 -> 1. 3 -> 1. 4 -> 2
simpleBinaryLog :: forall a. (Bits a, Integral a) => a -> Int
simpleBinaryLog n = f n 0
  where f :: a -> Int -> Int
        f 0 _ = undefined
        f 1 i = i
        f n i = f (shiftR n 1) (i + 1)

-- Non-exhaustive. Will exception with negative numbers.
-- Negative support may be safely added in the future but it is not needed now.
pack32x2To64 :: Int -> Int -> Int
pack32x2To64 a b
  | (a >= 0) && (a <= maxInt32) && (b >= 0) && (b <= maxInt32) = shiftL a 32 .|. b

unpack64To32x2 :: Int -> (Int, Int)
unpack64To32x2 ab
  | (ab >= 0) && (ab <= maxInt64) = (shiftR ab 32 .&. maxInt32, ab .&. maxInt32)

-- integerBitsToBoolList 8 0x17 -> [False,False,False,True,False,True,True,True]
integerBitsToBoolList :: Int -> Integer -> [Bool]
integerBitsToBoolList n value = fmap (testBit value) $ reverse [0..(n-1)]

-- Informal tests:
-- (unpack64To32x2 . (uncurry pack32x2To64)) (123, 987)
-- (unpack64To32x2 . (uncurry pack32x2To64)) (0x6EEFDADD, 0x6EEFDADD)
-- (unpack64To32x2 . (uncurry pack32x2To64)) (maxInt32, maxInt32)
-- (unpack64To32x2 . (uncurry pack32x2To64)) (1234567890, 987654321)

