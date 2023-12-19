-- | Kyle Gann.
module Music.Theory.Tuning.Db.Gann where

import Music.Theory.Tuning {- hmt -}
import Music.Theory.Tuning.Type {- hmt -}

-- * Historical

{- | Cents for 'pietro_aaron_1523'.

> let c = [0,76,193,310,386,503,580,697,773,890,1007,1083]
> map round pietro_aaron_1523_c == c

> map ((+ 60) . (/ 100)) pietro_aaron_1523_c
-}
pietro_aaron_1523_c :: [Cents]
pietro_aaron_1523_c =
  [ 0
  , 76.0
  , 193.2
  , 310.3
  , 386.3 -- 5/4
  , 503.4
  , 579.5
  , 696.8
  , 772.6 -- 25/16
  , 889.7
  , 1006.8
  , 1082.9
  ]

{- | Pietro Aaron (1523) meantone temperament, see
<http://www.kylegann.com/histune.html>

> tn_cents_i pietro_aaron_1523 == [0,76,193,310,386,503,580,697,773,890,1007,1083]

> import Music.Theory.Tuning.Scala
> scl <- scl_load "meanquar"
> tn_cents_i (scale_to_tuning 0.01 scl) == [0,76,193,310,386,503,579,697,773,890,1007,1083]
-}
pietro_aaron_1523 :: Tuning
pietro_aaron_1523 = Tuning (Right pietro_aaron_1523_c) Nothing

{- | Cents for 'thomas_young_1799'.

> let c = [0,94,196,298,392,500,592,698,796,894,1000,1092]
> map round thomas_young_1799_c == c
-}
thomas_young_1799_c :: [Cents]
thomas_young_1799_c =
  [ 0
  , 93.9
  , 195.8
  , 297.8
  , 391.7
  , 499.9
  , 591.9
  , 697.9
  , 795.8
  , 893.8
  , 999.8
  , 1091.8
  ]

{- | Thomas Young (1799), Well Temperament, <http://www.kylegann.com/histune.html>.

> tn_cents_i thomas_young_1799 == [0,94,196,298,392,500,592,698,796,894,1000,1092]

> scl <- scl_load "young2"
> tn_cents_i (scale_to_tuning 0.01 scl) == tn_cents_i thomas_young_1799
-}
thomas_young_1799 :: Tuning
thomas_young_1799 = Tuning (Right thomas_young_1799_c) Nothing

{- | Ratios for 'zarlino'.

> length zarlino_1588_r == 16
-}
zarlino_1588_r :: [Rational]
zarlino_1588_r = [1, 25 / 24, 10 / 9, 9 / 8, 32 / 27, 6 / 5, 5 / 4, 4 / 3, 25 / 18, 45 / 32, 3 / 2, 25 / 16, 5 / 3, 16 / 9, 9 / 5, 15 / 8]

{- | Gioseffo Zarlino, 1588, see <http://www.kylegann.com/tuning.html>.

> tn_divisions zarlino_1588 == 16
> tn_cents_i zarlino_1588 == [0,71,182,204,294,316,386,498,569,590,702,773,884,996,1018,1088]

> scl <- scl_load "zarlino2"
> tn_cents_i (scale_to_tuning 0.01 scl) == tn_cents_i zarlino_1588
-}
zarlino_1588 :: Tuning
zarlino_1588 = Tuning (Left zarlino_1588_r) Nothing

-- * 20th Century

{- | Ratios for 'ben_johnston_mtp_1977'.

> let c = [0,105,204,298,386,471,551,702,841,906,969,1088]
> map (round . ratio_to_cents) ben_johnston_mtp_1977_r == c
-}
ben_johnston_mtp_1977_r :: [Rational]
ben_johnston_mtp_1977_r =
  [ 1
  , 17 / 16
  , 9 / 8
  , 19 / 16
  , 5 / 4
  , 216
  , 11 / 8
  , 3 / 2
  , 13 / 8
  , 27 / 16
  , 7 / 4
  , 15 / 8
  ]

{- | Ben Johnston's \"Suite for Microtonal Piano\" (1977), see
<http://www.kylegann.com/tuning.html>

> tn_cents_i ben_johnston_mtp_1977 == [0,105,204,298,386,471,551,702,841,906,969,1088]
-}
ben_johnston_mtp_1977 :: Tuning
ben_johnston_mtp_1977 = Tuning (Left ben_johnston_mtp_1977_r) Nothing

-- * Gann

-- | Ratios for 'gann_arcana_xvi'.
gann_arcana_xvi_r :: [Rational]
gann_arcana_xvi_r =
  [ 1
  , 21 / 20
  , 16 / 15
  , 9 / 8
  , 7 / 6
  , 6 / 5
  , 11 / 9
  , 5 / 4
  , 216
  , 4 / 3
  , 27 / 20
  , 7 / 5
  , 22 / 15
  , 3 / 2
  , 55 / 36
  , 8 / 5
  , 44 / 27
  , 5 / 3
  , 42 / 25
  , 7 / 4
  , 9 / 5
  , 11 / 6
  , 15 / 8
  , 88 / 45
  ]

{- | Kyle Gann, _Arcana XVI_, see <http://www.kylegann.com/Arcana.html>.

> let r = [0,84,112,204,267,316,347,386,471,498,520,583,663,702,734,814,845,884,898,969,1018,1049,1088,1161]
> tn_cents_i gann_arcana_xvi == r
-}
gann_arcana_xvi :: Tuning
gann_arcana_xvi = Tuning (Left gann_arcana_xvi_r) Nothing

-- | Ratios for 'gann_superparticular'.
gann_superparticular_r :: [Rational]
gann_superparticular_r =
  [ 1
  , 110
  , 10 / 9
  , 9 / 8
  , 8 / 7
  , 7 / 6
  , 6 / 5
  , 5 / 4
  , 9 / 7
  , 4 / 3
  , 11 / 8
  , 7 / 5
  , 10 / 7
  , 3 / 2
  , 11 / 7
  , 14 / 9
  , 8 / 5
  , 5 / 3
  , 12 / 7
  , 7 / 4
  , 16 / 9
  , 9 / 5
  ]

{- | Kyle Gann, _Superparticular_, see <http://www.kylegann.com/Super.html>.

> tn_divisions gann_superparticular == 22

> let r = [0,165,182,204,231,267,316,386,435,498,551,583,617,702,782,765,814,884,933,969,996,1018]
> tn_cents_i gann_superparticular == r

> scl <- scl_load "gann_super"
> tn_cents_i (scale_to_tuning 0.01 scl) == tn_cents_i gann_superparticular
-}
gann_superparticular :: Tuning
gann_superparticular = Tuning (Left gann_superparticular_r) Nothing
