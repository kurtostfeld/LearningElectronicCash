-- Problem 6, Textbook
import Data.List (intercalate)

let p = 13
let ca = 3
let cb = 8

let coordinates = listNonZeroPointsInEFP p ca cb
let points = EcZero : fmap (uncurry EcPoint) coordinates

let pointAdd = pointAddMod p ca cb

let tableData = [(a,b,pointAdd a b) | a <- points, b <- points]
putStr $ intercalate "\n" $ fmap show tableData

-- Problem 6, Part (a)
import Data.List (intercalate)

let p = 5
let ca = 1
let cb = 2

let coordinates = listNonZeroPointsInEFP p ca cb
let points = EcZero : fmap (uncurry EcPoint) coordinates
let pointAdd = pointAddMod p ca cb

let tableData = [(a,b,pointAdd a b) | a <- points, b <- points]
putStr $ intercalate "\n" $ fmap show tableData

-- Problem 6, Part (b)
import Data.List (intercalate)

let p = 7
let ca = 2
let cb = 3

let coordinates = listNonZeroPointsInEFP p ca cb
let points = EcZero : fmap (uncurry EcPoint) coordinates
let pointAdd = pointAddMod p ca cb

let tableData = [(a,b,pointAdd a b) | a <- points, b <- points]
putStr $ intercalate "\n" $ fmap show tableData

-- Problem 6, Part (c)
import Data.List (intercalate)

let p = 11
let ca = 2
let cb = 5

-- echo "11\t2\t5\n8\t7\n3\t4\n" >> input.txt

let coordinates = listNonZeroPointsInEFP p ca cb
let points = EcZero : fmap (uncurry EcPoint) coordinates
let pointAdd = pointAddMod p ca cb

let tableData = [(a,b,pointAdd a b) | a <- points, b <- points]
putStr $ intercalate "\n" $ fmap show tableData

-- Older
let ca = -2
let cb = 4
let p@(EcPoint xp yp) = EcPoint 0 2
let q@(EcPoint xq yq) = EcPoint 3 (-5)

-- Part a
let pq@(EcPoint x3 y3) = pointAddNum ca cb p q

let lambdapq = (yq - yp)/(xq-xp)
let nupq = yp - lambdapq * xp

-- Part b
let pp@(EcPoint xpp ypp) = pointAddNum ca cb p p
let qq@(EcPoint xqq yqq) = pointAddNum ca cb q q
-- Part c
let ppp@(EcPoint xppp yppp) = pointAddNum ca cb (pointAddNum ca cb p p) p
let qqq@(EcPoint xqqq yqqq) = pointAddNum ca cb (pointAddNum ca cb q q) q

let lambdappp = (ypp - yp)/(xpp - xp)
let nuppp = yp - lambdappp * xp

let lambdaqqq = (yqq - yq)/(xqq - xq)
let nuqqq = yq - lambdaqqq * xq


let lambdapp = (3*xp^2 + ca) / (2*yp)
let nupp = yp - lambdapp * xp
let lambdaqq = (3*xq^2 + ca) / (2*yq)
let nuqq = yq - lambdaqq * xq
               
let lambda = (3*x2 + ca) / (2*y2)
let x3 = lambda^2 - x2 - x2
let y3 = lambda * (x2 - x3) - y2

Y^2 = X^3 + AX + B
dy/dx = (3x^2 + A)/2y


Y^2 = X^3 - 2X + 4
2Y dy = (3x^2 - 2) dx
dy/dx = (3x^2 - 2)/2y

at 3,-5, this is (3*9-2)/-10 = 25/-10 = -2.5
6.25 - 2*3 = 0.25
