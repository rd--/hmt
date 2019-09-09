-- | Andreas Werckmeister (1645-1706).
module Music.Theory.Tuning.DB.Werckmeister where

import Music.Theory.Tuning {- hmt -}
import Music.Theory.Tuning.Type {- hmt -}

-- | Approximate ratios for 'werckmeister_iii'.
--
-- > let c = [0,90,192,294,390,498,588,696,792,888,996,1092]
-- > in map (round . ratio_to_cents) werckmeister_iii_ar == c
werckmeister_iii_ar :: [Approximate_Ratio]
werckmeister_iii_ar =
    let c0 = 2 ** (1/2)
        c1 = 2 ** (1/4)
        c2 = 8 ** (1/4)
    in [1,256/243
       ,64/81 * c0,32/27
       ,256/243 * c1
       ,4/3,1024/729
       ,8/9 * c2,128/81
       ,1024/729 * c1,16/9
       ,128/81 * c1]

-- | Cents for 'werckmeister_iii'.
werckmeister_iii_ar_c :: [Cents]
werckmeister_iii_ar_c = map approximate_ratio_to_cents werckmeister_iii_ar

-- | Werckmeister III, Andreas Werckmeister (1645-1706)
--
-- > cents_i werckmeister_iii == [0,90,192,294,390,498,588,696,792,888,996,1092]
--
-- > import Music.Theory.Tuning.Scala
-- > scl <- scl_load "werck3"
-- > cents_i (scale_tuning 0.01 scl) == cents_i werckmeister_iii
werckmeister_iii :: Tuning
werckmeister_iii = Tuning (Right werckmeister_iii_ar_c) 2

-- | Approximate ratios for 'werckmeister_iv'.
--
-- > let c = [0,82,196,294,392,498,588,694,784,890,1004,1086]
-- > in map (round . ratio_to_cents) werckmeister_iv_ar == c
werckmeister_iv_ar :: [Approximate_Ratio]
werckmeister_iv_ar =
    let c0 = 2 ** (1/3)
        c1 = 4 ** (1/3)
    in [1,16384/19683 * c0
       ,8/9 * c0,32/27
       ,64/81 * c1
       ,4/3,1024/729
       ,32/27 * c0,8192/6561 * c0
       ,256/243 * c1,9/(4*c0)
       ,4096/2187]

-- | Cents for 'werckmeister_iv'.
werckmeister_iv_c :: [Cents]
werckmeister_iv_c = map approximate_ratio_to_cents werckmeister_iv_ar

-- | Werckmeister IV, Andreas Werckmeister (1645-1706)
--
-- > cents_i werckmeister_iv == [0,82,196,294,392,498,588,694,784,890,1004,1086]
--
-- > scl <- scl_load "werck4"
-- > cents_i (scale_tuning 0.01 scl) == cents_i werckmeister_iv
werckmeister_iv :: Tuning
werckmeister_iv = Tuning (Right werckmeister_iv_c) 2

-- | Approximate ratios for 'werckmeister_v'.
--
-- > let c = [0,96,204,300,396,504,600,702,792,900,1002,1098]
-- > in map (round . ratio_to_cents) werckmeister_v_ar == c
werckmeister_v_ar :: [Approximate_Ratio]
werckmeister_v_ar =
    let c0 = 2 ** (1/4)
        c1 = 2 ** (1/2)
        c2 = 8 ** (1/4)
    in [1,8/9 * c0
       ,9/8,c0
       ,8/9 * c1
       ,9/8 * c0,c1
       ,3/2,128/81
       ,c2,3/c2
       ,4/3 * c1]

-- | Cents for 'werckmeister_v'.
werckmeister_v_c :: [Cents]
werckmeister_v_c = map approximate_ratio_to_cents werckmeister_v_ar

-- | Werckmeister V, Andreas Werckmeister (1645-1706)
--
-- > cents_i werckmeister_v == [0,96,204,300,396,504,600,702,792,900,1002,1098]
--
-- > scl <- scl_load "werck5"
-- > cents_i (scale_tuning 0.01 scl) == cents_i werckmeister_v
werckmeister_v :: Tuning
werckmeister_v = Tuning (Right werckmeister_v_c) 2

-- | Ratios for 'werckmeister_vi', with supposed correction of 28/25 to 49/44.
--
-- > let c = [0,91,186,298,395,498,595,698,793,893,1000,1097]
-- > in map (round . ratio_to_cents) werckmeister_vi_r == c
werckmeister_vi_r :: [Rational]
werckmeister_vi_r =
    [1,98/93
    ,49/44 {- 28/25 -},196/165
    ,49/39
    ,4/3,196/139
    ,196/131,49/31
    ,196/117,98/55
    ,49/26]

-- | Werckmeister VI, Andreas Werckmeister (1645-1706)
--
-- > cents_i werckmeister_vi == [0,91,186,298,395,498,595,698,793,893,1000,1097]
--
-- > scl <- scl_load "werck6"
-- > cents_i (scale_tuning 0.01 scl) == cents_i werckmeister_vi
werckmeister_vi :: Tuning
werckmeister_vi = Tuning (Left werckmeister_vi_r) 2
