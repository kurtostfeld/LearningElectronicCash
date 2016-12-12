-- Weil Pairing
-- 242
calculateWeilPairing 631 30 34 5 (EcPoint 36 60) (EcPoint 121 387)

let polyfp = millerFforM 631 30 34 5 (EcPoint 36 60)
let spt = (EcPoint 0 36)
polyfp spt

let p = 631
let ca = 30
let cb = 34
let m = 5
let ppt = (EcPoint 36 60)
let qpt = (EcPoint 121 387)
let spt = (EcPoint 0 36)

-- ("(y - 60 - 569 * (x - 36))","(x + 36 + 36 - 569^2)")
-- (y - 60 - 569 * (x - 36)) / (x + 36 + 36 - 569^2)
millerFforMDiag p ca cb 2 ppt

-- ("(y - 60 - 569 * (x - 36))(y - 626 - 443 * (x - 617))","(x + 36 + 36 - 569^2)(x + 617 + 36 - 443^2)")
-- (y - 60 - 569 * (x - 36)) * (y - 626 - 443 * (x - 617)) / ((x + 36 + 36 - 569^2) * (x + 617 + 36 - 443^2))
millerFforMDiag p ca cb 3 ppt
millerFforMDiag p ca cb 4 ppt
millerFforMDiag p ca cb 5 ppt

let polyfp = millerFforM p ca cb m ppt
let polyfq = millerFforM p ca cb m qpt

polyfp spt -- 219
polyfp $ pointAddMod p ca cb qpt spt -- 103

let n = simpleBinaryLog m + 1 -- 3

let iplusone = n - 1
let tpt = ppt
let f = ("", "")

let i = iplusone - 1
let gtt = millerGDiag p ca cb tpt tpt
let f' = mergeRationalDiag (sqrRationalDiagUncurried f) (gtt) -- ("(y - 9 - 11 * (x - 2))","(x + 2 + 2 - 11^2)")
let t' = pointAddMod p ca cb tpt tpt -- (24, 28)
let f'' = if testBit m i then mergeRationalDiag f' (millerGDiag p ca cb tpt ppt) else f'
let t'' = if testBit m i then pointAddMod p ca cb t' ppt else t'

let iplusone = i
let tpt = t''
let f = f''

let i = iplusone - 1
let gtt = millerGDiag p ca cb tpt tpt
let f' = mergeRationalDiag (sqrRationalDiagUncurried f) (gtt) -- ("(y - 9 - 11 * (x - 2))","(x + 2 + 2 - 11^2)")
let t' = pointAddMod p ca cb tpt tpt -- (24, 28)
let f'' = if testBit m i then mergeRationalDiag f' (millerGDiag p ca cb tpt ppt) else f'
let t'' = if testBit m i then pointAddMod p ca cb t' ppt else t'


------
millerFforM 31 0 11 5 (EcPoint 2 9) (EcPoint 1 2)
millerFforMDiag 31 0 11 5 (EcPoint 2 9)

let p = 31
let x = 1
let y = 2

let n = (y - 9 - 11 * (x - 2))^2 * (y - 28 - 22 * (x - 24)) * (y - 28 - 22 * (x - 24)) -- 4
let n = (y + 13 - 11*x)^2 * (y - 28 - 22 * (x - 24)) * (y - 28 - 22 * (x - 24)) -- 4
let n = (-11*x + y + 13)^2 * (y - 28 - 22 * (x - 24)) * (y - 28 - 22 * (x - 24)) -- 4
let d = ((x + 2 + 2 - 11^2))^2 * (x + 24 + 24 - 22^2) * (x + 24 + 2 - 22^2)

let ns = (20*x + y + 13)^2 * (9*x +y+4) * (x+29) -- 8
let ds = (x+7)^2 * (x+29)
          
modDivide p n d -- Just 23
modDivide p ns ds -- Just 27


let inputpt = (EcPoint 1 2)


let p = 31
let ca = 0
let cb = 11
let m = 5
let ppt = (EcPoint 2 9)
let n = simpleBinaryLog m + 1 -- 3

let iplusone = n - 1
let tpt = ppt
let f = (\x -> Just 1)

let i = iplusone - 1
let gtt = millerG p ca cb tpt tpt
let f' = (\x -> (f x) <$$> (^2) <$$> (*) <*> (gtt x) <$$> (`mod` p))
let t' = pointAddMod p ca cb tpt tpt -- (24, 28)
let f'' = if testBit m i then ((\x -> (f' x) <$$> (*) <*> (millerG p ca cb tpt ppt x)) <$$> (`mod` p)) else f'
let t'' = if testBit m i then pointAddMod p ca cb t' ppt else t'

let iplusone = i
let tpt = t''
let f = f''

let i = iplusone - 1
let gtt = millerG p ca cb tpt tpt
let f' = (\x -> (f x) <$$> (^2) <$$> (*) <*> (gtt x) <$$> (`mod` p))
let t' = pointAddMod p ca cb tpt tpt -- (24, 28)
let f'' = if testBit m i then ((\x -> (f' x) <$$> (*) <*> (millerG p ca cb tpt ppt x)) <$$> (`mod` p)) else f'
let t'' = if testBit m i then pointAddMod p ca cb t' ppt else t'

exampleMillerF 1 2
f'' (EcPoint 1 2)

---------------
-- Textbook 6.17 (c)

let p = 1201
let ca = 19
let cb = 17
let ppt = (EcPoint 278 285)

let na = 595
-- Q_A = n_A \cdot P
let qa = pointMultiplyMod p ca cb ppt na

-- ((1147,640),279,1189)
let rpt = (EcPoint 1147 640)
let c1 = 279
let c2 = 1189

let tpt = pointMultiplyMod p ca cb rpt na

let tx = fromJust $ getEcX tpt
let txinv = fromJust $ modMultInverse p tx
let m1 = (txinv * c1) `mod` p

let ty = fromJust $ getEcY tpt
let tyinv = fromJust $ modMultInverse p ty
let m2 = (tyinv * c2) `mod` p

-- 6.18 (b)
import Data.MultiMap ((!))

let p = 1201
let ca = 19
let cb = 17
let ppt = (EcPoint 278 285)

-- ((269, 339), 814, 1050)
let rpt = (EcPoint 269 339)
let c1 = 814
let c2 = 1050

let m1 = 1050
let m1inv = fromJust $ modMultInverse p m1
let s_x = (c1 * m1inv) `mod` p
let s_y_sqr = (s_x^3 + 19*s_x + 17) `mod` p -- 697

let p_mod_sqr_map = buildModSquareRootMap p
let s_y_candidates = p_mod_sqr_map ! s_y_sqr -- [182,1019]
let s_y_inv_candidates = fmap (fromJust . (modMultInverse p)) s_y_candidates -- [33,1168]
let m2_s_candidates = fmap (\x -> (x * c2) `mod` p) s_y_inv_candidates -- [1022,179]
