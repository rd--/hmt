{- | Larry Polansky.
\"Notes on the Tunings of Three Central Javanese Slendro\/Pelog Pairs\".
/Experimental Musical Instruments/, 6(2):12-13,16-17, 1990.
-}
module Music.Theory.Tuning.Polansky_1990 where

import Data.Ratio {- base -}

import qualified Music.Theory.List as List {- hmt-base -}

import qualified Music.Theory.Tuning as Tuning {- hmt -}

{- | Kanjutmesem Slendro (S1,S2,S3,S5,S6,S1')

>>> List.d_dx kanjutmesem_s
[252,238,241,236,253]
-}
kanjutmesem_s :: Num n => [n]
kanjutmesem_s = [0, 252, 490, 731, 967, 1220]

{- | Kanjutmesem Pelog (P1,P2,P3,P4,P5,P6,P7,P1')

>>> List.d_dx kanjutmesem_p
[141,141,272,140,115,172,246]
-}
kanjutmesem_p :: Num n => [n]
kanjutmesem_p = [37, 178, 319, 591, 731, 846, 1018, 1264]

{- | Darius Slendro (S1,S2,S3,S5,S6,S1')

>>> List.d_dx darius_s
[204,231,267,231,267]

>>> ax_r darius_s == [9/8,8/7,7/6,8/7,7/6]
True
-}
darius_s :: Num n => [n]
darius_s = [0, 204, 435, 702, 933, 1200]

{- | Madeleine Pelog (P1,P2,P3,P4,P5,P6,P7,P1')

>>> List.d_dx madeleine_p
[139,128,336,99,94,173,231]

>>> ax_r madeleine_p == [13/12,14/13,17/14,18/17,19/18,21/19,8/7]
True
-}
madeleine_p :: Num n => [n]
madeleine_p = [137, 276, 404, 740, 839, 933, 1106, 1337]

{- | Lipur Sih Slendro (S1,S2,S3,S5,S6,S1')

>>> List.d_dx lipur_sih_s
[273,236,224,258,256]
-}
lipur_sih_s :: Num n => [n]
lipur_sih_s = [0, 273, 509, 733, 991, 1247]

{- | Lipur Sih Pelog (P1,P2,P3,P4,P5,P6,P7,P1')

>>> List.d_dx lipur_sih_p
[110,153,253,146,113,179]
-}
lipur_sih_p :: Num n => [n]
lipur_sih_p = [216, 326, 479, 732, 878, 991, 1170]

{- | Idealized ET Slendro, 5-tone equal temperament (p.17)

>>> List.d_dx idealized_et_s
[240,240,240,240,240]
-}
idealized_et_s :: Num n => [n]
idealized_et_s = [0, 240, 480, 720, 960, 1200]

{- | Idealized ET Pelog, subset of 9-tone equal temperament (p.17)

>>> List.d_dx idealized_et_p == [400/3,800/3,400/3,400/3,400/3,400/3,800/3]
True
-}
idealized_et_p :: Integral n => [Ratio n]
idealized_et_p = [160, 293 + 1 / 3, 560, 693 + 1 / 3, 826 + 2 / 3, 960, 1093 + 1 / 3, 1360]

-- | Reconstruct approximate ratios to within @1e-3@ from intervals.
ax_r :: Real n => [n] -> [Rational]
ax_r = map (Tuning.reconstructed_ratio 1e-3 . realToFrac) . List.d_dx
