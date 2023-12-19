-- | <http://www.microtonal-synthesis.com/scales.html>
module Music.Theory.Tuning.Db.Microtonal_Synthesis where

import Music.Theory.Tuning {- hmt -}
import Music.Theory.Tuning.Type {- hmt -}

-- | Ratios for 'pythagorean'.
pythagorean_12_r :: [Rational]
pythagorean_12_r =
  [ 1
  , 2187 / 2048 {- 256/243 -}
  , 9 / 8
  , 32 / 27
  , 81 / 64
  , 4 / 3
  , 729 / 512
  , 3 / 2
  , 6561 / 4096 {- 128/81 -}
  , 27 / 16
  , 16 / 9
  , 243 / 128
  ]

{- | Pythagorean tuning, <http://www.microtonal-synthesis.com/scale_pythagorean.html>.

> cents_i pythagorean_12 == [0,114,204,294,408,498,612,702,816,906,996,1110]

> scl <- scl_load "pyth_12"
> cents_i (scale_tuning 0.1 scl) == cents_i pythagorean_12
-}
pythagorean_12 :: Tuning
pythagorean_12 = Tuning (Left pythagorean_12_r) Nothing

{- | Ratios for 'five_limit_tuning'.

> let c = [0,112,204,316,386,498,590,702,814,884,996,1088]
> in map (round . ratio_to_cents) five_limit_tuning_r == c
-}
five_limit_tuning_r :: [Rational]
five_limit_tuning_r =
  [ 1
  , 16 / 15
  , 9 / 8
  , 6 / 5
  , 5 / 4
  , 4 / 3
  , 45 / 32 {- 64/45 -}
  , 3 / 2
  , 8 / 5
  , 5 / 3
  , 16 / 9 {- 9/5 -}
  , 15 / 8
  ]

{- | Five-limit tuning (five limit just intonation), Alexander Malcolm's Monochord (1721).

> cents_i five_limit_tuning == [0,112,204,316,386,498,590,702,814,884,996,1088]

> scl <- scl_load "malcolm"
> cents_i (scale_tuning 0.1 scl) == cents_i five_limit_tuning
-}
five_limit_tuning :: Tuning
five_limit_tuning = Tuning (Left five_limit_tuning_r) Nothing

{- | Ratios for 'septimal_tritone_just_intonation'.

> let c = [0,112,204,316,386,498,583,702,814,884,1018,1088]
> in map (round . ratio_to_cents) septimal_tritone_just_intonation == c
-}
septimal_tritone_just_intonation_r :: [Rational]
septimal_tritone_just_intonation_r =
  [ 1
  , 16 / 15
  , 9 / 8
  , 6 / 5
  , 5 / 4
  , 4 / 3
  , 7 / 5
  , 3 / 2
  , 8 / 5
  , 5 / 3
  , 9 / 5
  , 15 / 8
  ]

{- | Septimal tritone Just Intonation, see
<http://www.microtonal-synthesis.com/scale_just_intonation.html>

> let c = [0,112,204,316,386,498,583,702,814,884,1018,1088]
> in cents_i septimal_tritone_just_intonation == c

> scl <- scl_load "ji_12"
> cents_i (scale_tuning 0.1 scl) == cents_i septimal_tritone_just_intonation
-}
septimal_tritone_just_intonation :: Tuning
septimal_tritone_just_intonation = Tuning (Left septimal_tritone_just_intonation_r) Nothing

{- | Ratios for 'seven_limit_just_intonation'.

> let c = [0,112,204,316,386,498,583,702,814,884,969,1088]
> in map (round . ratio_to_cents) seven_limit_just_intonation == c
-}
seven_limit_just_intonation_r :: [Rational]
seven_limit_just_intonation_r =
  [ 1
  , 16 / 15
  , 9 / 8
  , 6 / 5
  , 5 / 4
  , 4 / 3
  , 7 / 5
  , 3 / 2
  , 8 / 5
  , 5 / 3
  , 7 / 4
  , 15 / 8
  ]

{- | Seven limit Just Intonation.

> cents_i seven_limit_just_intonation == [0,112,204,316,386,498,583,702,814,884,969,1088]
-}
seven_limit_just_intonation :: Tuning
seven_limit_just_intonation = Tuning (Left seven_limit_just_intonation_r) Nothing

{- | Approximate ratios for 'kirnberger_iii'.

> let c = [0,90,193,294,386,498,590,697,792,890,996,1088]
> in map (round.to_cents) kirnberger_iii_ar == c
-}
kirnberger_iii_ar :: [Approximate_Ratio]
kirnberger_iii_ar =
  [ 1
  , 256 / 243
  , sqrt 5 / 2
  , 32 / 27
  , 5 / 4
  , 4 / 3
  , 45 / 32
  , 5 ** 0.25
  , 128 / 81
  , (5 ** 0.75) / 2
  , 16 / 9
  , 15 / 8
  ]

{- | <http://www.microtonal-synthesis.com/scale_kirnberger.html>.

> cents_i kirnberger_iii == [0,90,193,294,386,498,590,697,792,890,996,1088]

> scl <- scl_load "kirnberger"
> cents_i (scale_tuning 0.1 scl) == cents_i kirnberger_iii
-}
kirnberger_iii :: Tuning
kirnberger_iii = Tuning (Right (map approximate_ratio_to_cents kirnberger_iii_ar)) Nothing

-- > let c = [0,94,196,298,392,502,592,698,796,894,1000,1090]
-- > in map round vallotti_c == c
vallotti_c :: [Cents]
vallotti_c =
  [ 0.0
  , 94.135
  , 196.09
  , 298.045
  , 392.18
  , 501.955
  , 592.18
  , 698.045
  , 796.09
  , 894.135
  , 1000.0
  , 1090.225
  ]

{- | Vallotti & Young scale (Vallotti version), see
<http://www.microtonal-synthesis.com/scale_vallotti_young.html>.

> cents_i vallotti == [0,94,196,298,392,502,592,698,796,894,1000,1090]

> scl <- scl_load "vallotti"
> cents_i (scale_tuning 0.1 scl) == cents_i vallotti
-}
vallotti :: Tuning
vallotti = Tuning (Right vallotti_c) Nothing

-- > let c = [0,128,139,359,454,563,637,746,841,911,1072,1183]
-- > in map (round . ratio_to_cents) mayumi_tsuda == c
mayumi_tsuda_r :: [Rational]
mayumi_tsuda_r =
  [ 1
  , 14 / 13
  , 13 / 12
  , 16 / 13
  , 13 / 10
  , 18 / 13
  , 13 / 9
  , 20 / 13
  , 13 / 8
  , 22 / 13
  , 13 / 7
  , 208 / 105
  ]

{- | Mayumi Tsuda 13-limit Just Intonation scale,
<http://www.microtonal-synthesis.com/scale_reinhard.html>.

> cents_i mayumi_tsuda == [0,128,139,359,454,563,637,746,841,911,1072,1183]

> scl <- scl_load "tsuda13"
> cents_i (scale_tuning 0.1 scl) == cents_i mayumi_tsuda
-}
mayumi_tsuda :: Tuning
mayumi_tsuda = Tuning (Left mayumi_tsuda_r) Nothing

{- | Ratios for 'lou_harrison_16'.

> length lou_harrison_16_r == 16

> let c = [0,112,182,231,267,316,386,498,603,702,814,884,933,969,1018,1088]
> in map (round . ratio_to_cents) lou_harrison_16_r == c
-}
lou_harrison_16_r :: [Rational]
lou_harrison_16_r =
  [ 1
  , 16 / 15
  , 10 / 9
  , 8 / 7
  , 7 / 6
  , 6 / 5
  , 5 / 4
  , 4 / 3
  , 17 / 12
  , 3 / 2
  , 8 / 5
  , 5 / 3
  , 12 / 7
  , 7 / 4
  , 9 / 5
  , 15 / 8
  ]

{- | Lou Harrison 16 tone Just Intonation scale, see
<http://www.microtonal-synthesis.com/scale_harrison_16.html>

> let r = [0,112,182,231,267,316,386,498,603,702,814,884,933,969,1018,1088]
> in cents_i lou_harrison_16 == r

> import Music.Theory.Tuning.Scala
> scl <- scl_load "harrison_16"
> cents_i (scale_tuning 0.1 scl) == cents_i lou_harrison_16
-}
lou_harrison_16 :: Tuning
lou_harrison_16 = Tuning (Left lou_harrison_16_r) Nothing

-- | Ratios for 'partch_43'.
partch_43_r :: [Rational]
partch_43_r =
  [ 1
  , 81 / 80
  , 33 / 32
  , 21 / 20
  , 16 / 15
  , 12 / 11
  , 110
  , 10 / 9
  , 9 / 8
  , 8 / 7
  , 7 / 6
  , 32 / 27
  , 6 / 5
  , 11 / 9
  , 5 / 4
  , 14 / 11
  , 9 / 7
  , 216
  , 4 / 3
  , 27 / 20
  , 11 / 8
  , 7 / 5
  , 10 / 7
  , 16 / 11
  , 40 / 27
  , 3 / 2
  , 32 / 21
  , 14 / 9
  , 11 / 7
  , 8 / 5
  , 18 / 11
  , 5 / 3
  , 27 / 16
  , 12 / 7
  , 7 / 4
  , 16 / 9
  , 9 / 5
  , 20 / 11
  , 11 / 6
  , 15 / 8
  , 40 / 21
  , 64 / 33
  , 160 / 81
  ]

{- | Harry Partch 43 tone scale, see
<http://www.microtonal-synthesis.com/scale_partch.html>

> cents_i partch_43 == [0,22,53,84,112,151,165
>                      ,182,204,231,267,294,316
>                      ,347,386,418,435
>                      ,471,498,520,551,583,617,649
>                      ,680,702,729,765,782,814,853,884,906,933
>                      ,969,996,1018,1035,1049,1088,1116,1147,1178]

> scl <- scl_load "partch_43"
> cents_i (scale_tuning 0.1 scl) == cents_i partch_43
-}
partch_43 :: Tuning
partch_43 = Tuning (Left partch_43_r) Nothing

-- | Ratios for 'ben_johnston_25'.
ben_johnston_25_r :: [Rational]
ben_johnston_25_r =
  [ 1
  , 25 / 24
  , 135 / 128
  , 16 / 15
  , 10 / 9
  , 9 / 8
  , 75 / 64
  , 6 / 5
  , 5 / 4
  , 81 / 64
  , 32 / 25
  , 4 / 3
  , 27 / 20
  , 45 / 32
  , 36 / 25
  , 3 / 2
  , 25 / 16
  , 8 / 5
  , 5 / 3
  , 27 / 16
  , 225 / 128
  , 16 / 9
  , 9 / 5
  , 15 / 8
  , 48 / 25
  ]

{- | Ben Johnston 25 note just enharmonic scale, see
<http://www.microtonal-synthesis.com/scale_johnston_25.html>

> scl <- scl_load "johnston_25"
> cents_i (scale_tuning 0.1 scl) == cents_i ben_johnston_25
-}
ben_johnston_25 :: Tuning
ben_johnston_25 = Tuning (Left ben_johnston_25_r) Nothing
