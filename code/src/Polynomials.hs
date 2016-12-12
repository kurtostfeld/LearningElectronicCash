{-# LANGUAGE ScopedTypeVariables #-}

module Polynomials where

import Modulo
import Data.List (dropWhileEnd, intercalate)
import Data.List.Split
import Data.Maybe (fromJust, fromMaybe)

parsePolynomial :: forall a. (Integral a, Read a) => String -> [a]
parsePolynomial s =
  let textCoefficients = splitOn " " s
  in fmap read textCoefficients

formatVarExp :: forall a. (Integral a, Show a) => String -> a -> String
formatVarExp v 0 = "1"
formatVarExp v 1 = v
formatVarExp v e = v ++ "^" ++ (show e)

formatVarCoefExp :: forall a. (Integral a, Show a) => String -> a -> a -> String
formatVarCoefExp v 0 _ = "0"
formatVarCoefExp v 1 0 = v
formatVarCoefExp v c 0 = (show c)
formatVarCoefExp v 1 e = formatVarExp v e
formatVarCoefExp v c e = (show c) ++ (formatVarExp v e)

formatPolynomial :: forall a. (Integral a, Show a) => String -> [a] -> String
formatPolynomial v cs =
  let natZero :: [a] = [0,1..]
      coefAndExp = zip cs [0,1..]
      filteredCoefAndExp = filter (\(c,e) -> c /= 0) coefAndExp
      stringTerms = fmap (\(c,e) -> formatVarCoefExp v c e) filteredCoefAndExp
  in intercalate " + " stringTerms

formatPolynomialx = formatPolynomial "x"
formatPolynomialMst = formatPolynomial "t" . reverse

-- zipWithLeadingDefault 0 (\x y -> (x, y)) [6,5,4,2] [9,7]
-- zipWithLeadingDefault 0 (\x y -> (x, y)) [9,7] [6,5,4,2]
zipWithLeadingDefault :: a -> (a -> a -> b) -> [a] -> [a] -> [b]
zipWithLeadingDefault d f xs ys =
  let lengthX = length xs
      lengthY = length ys
  in if lengthX > lengthY
        then zipWith f xs ((replicate (lengthX - lengthY) d) ++ ys)
        else zipWith f ((replicate (lengthY - lengthX) d) ++ xs) ys

prunePolyMs :: forall a. (Integral a) => [a] -> [a]
prunePolyMs xs = dropWhile (== 0) xs

polyMsRingAdd :: forall a. (Integral a) => a -> [a] -> [a] -> [a]
polyMsRingAdd p xs ys = prunePolyMs $ zipWithLeadingDefault 0 (\a b -> (a + b) `mod` p) xs ys

polyMsRingSubtract :: forall a. (Integral a) => a -> [a] -> [a] -> [a]
polyMsRingSubtract p xs ys = prunePolyMs $ zipWithLeadingDefault 0 (\a b -> (a - b) `mod` p) xs ys

polyMsRingScalarMultiply :: forall a. (Integral a) => a -> a -> [a] -> [a]
polyMsRingScalarMultiply p s xs = prunePolyMs $ fmap (\x -> (x * s) `mod` p) xs

polyMsRingMultiply :: forall a. (Integral a) => a -> [a] -> [a] -> [a]
polyMsRingMultiply p [] _ = []
polyMsRingMultiply p (x:[]) ys = polyMsRingScalarMultiply p x ys
polyMsRingMultiply p (x:xs) ys =
  let scalarMultiple = polyMsRingScalarMultiply p x ys
      padding = replicate (length xs) 0
      scalarMultipleShifted = scalarMultiple ++ padding
  in polyMsRingAdd p scalarMultipleShifted $ polyMsRingMultiply p xs ys

-- polyMsRingQuotRem 7 [5, 0, 4, 3] [2, 1]
-- ([6, 4, 0], [3])
-- polyMsRingQuotRem 7 [1, 4, 3] [2, 1]
-- ([4, 0], [3])
polyMsRingQuotRem :: forall a. (Integral a) => a -> [a] -> [a] -> ([a], [a])
polyMsRingQuotRem p dividend divisor =
  let dividend' = prunePolyMs dividend
      divisor' = prunePolyMs divisor
      dividendLen = length dividend'
      divisorLen = length divisor'
  in if divisorLen > dividendLen then ([], dividend') else
    let dividendHead = head dividend'
        divisorHead = head divisor'
        divisorHeadInv = fromJust $ modMultInverse p divisorHead
        scalar = (divisorHeadInv * dividendHead) `mod` p
        divisorProduct = polyMsRingScalarMultiply p scalar divisor
        padding = replicate (dividendLen - divisorLen) 0
        paddedDivisorProduct = divisorProduct ++ padding
        paddedQuotient = scalar : padding
        nextDividend = polyMsRingSubtract p dividend paddedDivisorProduct
        (nextQuotient, remainder) = polyMsRingQuotRem p nextDividend divisor
    in (polyMsRingAdd p paddedQuotient nextQuotient, remainder)

prunePoly :: forall a. (Integral a) => [a] -> [a]
prunePoly xs = dropWhileEnd (== 0) xs

polyRingAdd :: forall a. (Integral a) => a -> [a] -> [a] -> [a]
polyRingAdd p as bs =
  let l = max (length as) (length bs)
      padding = repeat 0
  in prunePoly $ take l $ zipWith (\a b -> (a + b) `mod` p) (as ++ padding) (bs ++ padding)

polyRingSubtract :: forall a. (Integral a) => a -> [a] -> [a] -> [a]
polyRingSubtract p as bs =
  let l = max (length as) (length bs)
      padding = repeat 0
  in prunePoly $ take l $ zipWith (\a b -> (a - b) `mod` p) (as ++ padding) (bs ++ padding)

polyRingScalarMultiply :: forall a. (Integral a) => a -> a -> [a] -> [a]
polyRingScalarMultiply p s xs = prunePoly $ fmap (\x -> (x * s) `mod` p) xs

polyRingMultiply :: forall a. (Integral a) => a -> [a] -> [a] -> [a]
polyRingMultiply p [] _ = []
polyRingMultiply p (x:[]) bs = polyRingScalarMultiply p x bs
polyRingMultiply p (x:xs) bs =
  polyRingAdd p (polyRingScalarMultiply p x bs) $ 0 : polyRingMultiply p xs bs

polyRingQuotRem :: forall a. (Integral a) => a -> [a] -> [a] -> ([a], [a])
polyRingQuotRem p dividend divisor =
  let (qms, rms) = polyMsRingQuotRem p (reverse dividend) (reverse divisor)
  in (reverse qms, reverse rms)
