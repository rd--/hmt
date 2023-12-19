-- | Andreas Werckmeister (1645-1706).
module Music.Theory.Tuning.Db.Werckmeister where

import Music.Theory.Tuning {- hmt -}
import Music.Theory.Tuning.Scala {- hmt -}
import Music.Theory.Tuning.Type {- hmt -}

{- | Approximate ratios for 'werckmeister_iii'.

>>> map (round . fratio_to_cents) werckmeister_iii_ar
[0,90,192,294,390,498,588,696,792,888,996,1092]
-}
werckmeister_iii_ar :: [Approximate_Ratio]
werckmeister_iii_ar =
  let c0 = 2 ** (1 / 2)
      c1 = 2 ** (1 / 4)
      c2 = 8 ** (1 / 4)
  in [ 1
     , 256 / 243
     , 64 / 81 * c0
     , 32 / 27
     , 256 / 243 * c1
     , 4 / 3
     , 1024 / 729
     , 8 / 9 * c2
     , 128 / 81
     , 1024 / 729 * c1
     , 16 / 9
     , 128 / 81 * c1
     ]

-- | Cents for 'werckmeister_iii'.
werckmeister_iii_ar_c :: [Cents]
werckmeister_iii_ar_c = map approximate_ratio_to_cents werckmeister_iii_ar

werckmeister_iii_scl :: IO Scale
werckmeister_iii_scl = scl_load "werck3"

{- | Werckmeister III, Andreas Werckmeister (1645-1706)

>>> tn_cents_i werckmeister_iii
[0,90,192,294,390,498,588,696,792,888,996,1092]

>>> scl <- werckmeister_iii_scl
>>> tn_cents_i (scale_to_tuning scl) == tn_cents_i werckmeister_iii
True

>>> let r = [1,17/16,9/8,13/11,5/4,4/3,7/5,3/2,11/7,5/3,16/9,15/8]
>>> tn_reconstructed_ratios 0.01 werckmeister_iii == Just r
True
-}
werckmeister_iii :: Tuning
werckmeister_iii = Tuning (Right werckmeister_iii_ar_c) Nothing

{- | Approximate ratios for 'werckmeister_iv'.

>>> map (round . fratio_to_cents) werckmeister_iv_ar
[0,82,196,294,392,498,588,694,784,890,1004,1086]
-}
werckmeister_iv_ar :: [Approximate_Ratio]
werckmeister_iv_ar =
  let c0 = 2 ** (1 / 3)
      c1 = 4 ** (1 / 3)
  in [ 1
     , 16384 / 19683 * c0
     , 8 / 9 * c0
     , 32 / 27
     , 64 / 81 * c1
     , 4 / 3
     , 1024 / 729
     , 32 / 27 * c0
     , 8192 / 6561 * c0
     , 256 / 243 * c1
     , 9 / (4 * c0)
     , 4096 / 2187
     ]

-- | Cents for 'werckmeister_iv'.
werckmeister_iv_c :: [Cents]
werckmeister_iv_c = map approximate_ratio_to_cents werckmeister_iv_ar

werckmeister_iv_scl :: IO Scale
werckmeister_iv_scl = scl_load "werck4"

{- | Werckmeister IV, Andreas Werckmeister (1645-1706)

>>> tn_cents_i werckmeister_iv
[0,82,196,294,392,498,588,694,784,890,1004,1086]

>>> scl <- werckmeister_iv_scl
>>> tn_cents_i (scale_to_tuning scl) == tn_cents_i werckmeister_iv
True
-}
werckmeister_iv :: Tuning
werckmeister_iv = Tuning (Right werckmeister_iv_c) Nothing

{- | Approximate ratios for 'werckmeister_v'.

>>> map (round . fratio_to_cents) werckmeister_v_ar
[0,96,204,300,396,504,600,702,792,900,1002,1098]
-}
werckmeister_v_ar :: [Approximate_Ratio]
werckmeister_v_ar =
  let c0 = 2 ** (1 / 4)
      c1 = 2 ** (1 / 2)
      c2 = 8 ** (1 / 4)
  in [ 1
     , 8 / 9 * c0
     , 9 / 8
     , c0
     , 8 / 9 * c1
     , 9 / 8 * c0
     , c1
     , 3 / 2
     , 128 / 81
     , c2
     , 3 / c2
     , 4 / 3 * c1
     ]

-- | Cents for 'werckmeister_v'.
werckmeister_v_c :: [Cents]
werckmeister_v_c = map approximate_ratio_to_cents werckmeister_v_ar

werckmeister_v_scl :: IO Scale
werckmeister_v_scl = scl_load "werck5"

{- | Werckmeister V, Andreas Werckmeister (1645-1706)

>>> tn_cents_i werckmeister_v
[0,96,204,300,396,504,600,702,792,900,1002,1098]

>>> scl <- werckmeister_v_scl
>>> tn_cents_i (scale_to_tuning scl) == tn_cents_i werckmeister_v
True
-}
werckmeister_v :: Tuning
werckmeister_v = Tuning (Right werckmeister_v_c) Nothing

{- | Ratios for 'werckmeister_vi', with supposed correction of 28/25 to 49/44.

>>> map (round . fratio_to_cents) werckmeister_vi_r
[0,91,186,298,395,498,595,698,793,893,1000,1097]
-}
werckmeister_vi_r :: [Rational]
werckmeister_vi_r =
  [ 1
  , 98 / 93
  , 49 / 44 {- 28/25 -}
  , 196 / 165
  , 49 / 39
  , 4 / 3
  , 196 / 139
  , 196 / 131
  , 49 / 31
  , 196 / 117
  , 98 / 55
  , 49 / 26
  ]

werckmeister_vi_scl :: IO Scale
werckmeister_vi_scl = scl_load "werck6"

{- | Werckmeister VI, Andreas Werckmeister (1645-1706)

>>> tn_cents_i werckmeister_vi
[0,91,186,298,395,498,595,698,793,893,1000,1097]

>>> scl <- werckmeister_vi_scl
>>> tn_cents_i (scale_to_tuning scl) == tn_cents_i werckmeister_vi
True

>>> import Data.Maybe
>>> let map_zip f l = zip l (map f l)
>>> map_zip (fromJust . tn_ratios_lookup werckmeister_vi) [-24,-17 .. 25]
[(-24,1 % 4),(-17,49 % 131),(-10,49 % 88),(-3,98 % 117),(4,49 % 39),(11,49 % 26),(18,392 % 139),(25,392 % 93)]

>>> map_zip (round . tn_approximate_ratios_lookup werckmeister_v) [-24, -17 .. 25]
[(-24,0),(-17,0),(-10,1),(-3,1),(4,1),(11,2),(18,3),(25,4)]
-}
werckmeister_vi :: Tuning
werckmeister_vi = Tuning (Left werckmeister_vi_r) Nothing
