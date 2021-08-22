{-# LANGUAGE ScopedTypeVariables #-}

module RandomUtilities where

import BinaryUtilities
import Modulo
import SimpleRSA

import Numeric (showHex)
import Data.Bits
import Data.Function (on)
import Data.List (nubBy, sortBy)
import System.Random

       -- Random Functions
--randomIntN :: forall g. RandomGen g => Int -> g -> ([Int], g)
--randomIntN n gen =
--  let tupleList = take n $ iterate (\(x, g) -> next g) (next gen)
--      g' = snd $ last tupleList
--      values = fmap fst tupleList
--  in (values, g')

-- Get n random values with the default range and provide updated RandomGen state
randomNextN :: forall a g. (Random a, RandomGen g) => Int -> g -> ([a], g)
randomNextN n gen =
  let tupleList = take n $ iterate (\(x, g) -> random g) (random gen)
      g' = snd $ last tupleList
      values = fmap fst tupleList
  in (values, g')

-- Get n random values with the specified range and provide updated RandomGen state
randomRNextN :: forall a g. (Random a, RandomGen g) => (a, a) -> Int -> g -> ([a], g)
randomRNextN bounds n gen =
  let tupleList = take n $ iterate (\(x, g) -> randomR bounds g) (randomR bounds gen)
      g' = snd $ last tupleList
      values = fmap fst tupleList
  in (values, g')

-- Get n _unique_ random values with the specified range and provide updated RandomGen state
randomRUniqueN :: forall a g. (Eq a, Random a, RandomGen g) => (a, a) -> Int -> g -> ([a], g)
randomRUniqueN bounds n gen =
  let infiniteTuples = iterate (\(x, g) -> randomR bounds g) (randomR bounds gen)
      uniqueTuples = nubBy ((==) `on` fst) infiniteTuples
      tupleList = take n uniqueTuples
      g' = snd $ last tupleList
      values = fmap fst tupleList
  in (values, g')

-- Gives a random integer of exactly n bits. The leading bit is random as well.
-- Bit positions greater than n are guaranteed to be zero.
randomIntegerNBits :: forall g. RandomGen g => Int -> g -> (Integer, g)
randomIntegerNBits n gen1 =
  let (bitValues, gen2) = randomRNextN (0, 1) n gen1
      value = foldl (\acc bit -> shiftL acc 1 .|. bit) (0 :: Integer) bitValues
  in (value, gen2)

deterministicallyRandomlyFlipBits :: forall g. RandomGen g => Integer -> g -> Integer
deterministicallyRandomlyFlipBits n gen = xor n $ fst $ randomIntegerNBits (simpleBinaryLog n) gen

       -- Chaum Paper Functions
chaumF :: Integer -> Integer -> Integer -> Integer
chaumF n a b =
  let gen :: StdGen = mkStdGen 1865050052
      a' = deterministicallyRandomlyFlipBits a gen
      b' = deterministicallyRandomlyFlipBits b gen
  in (a + b) `mod` n

chaumG :: Integer -> Integer -> Integer -> Integer
chaumG n a b =
  let gen :: StdGen = mkStdGen 2052883677
      a' = deterministicallyRandomlyFlipBits a gen
      b' = deterministicallyRandomlyFlipBits b gen
  in (a + b) `mod` n

-- ["b", "z", "a", "c"] -> [2, 0, 3, 1]
getSortedIndices :: Ord a => [a] -> [Int]
getSortedIndices xs = fmap fst $ sortBy (compare `on` snd) $ zip [0..] xs

-- [2, 0, 3, 1] ["b", "z", "a", "c"] -> ["a","b","c","z"]
sublistByIndices :: [Int] -> [a] -> [a]
sublistByIndices indices xs = fmap (xs !!) indices



