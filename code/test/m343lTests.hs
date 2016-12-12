import Modulo

import Data.Maybe (fromJust)

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

basicGcdCases = TestLabel "basic gcd cases" $ TestList [
  TestCase $ assertEqual "gcd" 2 $ gcd' 10 12,
  TestCase $ assertEqual "gcd" 44 $ gcd' 2024 748
  ]

extendedGcdCase :: (Integral a, Show a) => a -> a -> Test.HUnit.Test
extendedGcdCase a b =
  let g = gcd a b
      (g', u, v) = egcd a b in
  TestLabel "extended gcd" $ TestList [
    TestCase $ assertEqual "egcd g value" g g',
    TestCase $ assertEqual "egcd u + v values" g $ u*a + v*b
  ]

extendedGcdCases = TestLabel "extended gcd cases" $ TestList [
  extendedGcdCase 10 12,
  extendedGcdCase 2024 748
  ]

gcdTestCases = TestLabel "all gcd cases" $ TestList [
  basicGcdCases,
  extendedGcdCases
  ]

modularExponentiationTestCases = TestLabel "basic modular exponentiation test cases" $ TestList [
  TestCase $ assertEqual "modPositiveExponentiation" 34 $ modPositiveExponentiation 73 13 247,
  TestCase $ assertEqual "modPositiveExponentiation" 649 $ modPositiveExponentiation 839 13 347
  ]

allTestCases = TestLabel "all tests" $ TestList [
  gcdTestCases, modularExponentiationTestCases
  ]

main :: IO ()
main = defaultMain $ hUnitTestToTests allTestCases
