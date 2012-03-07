-- | Tuning theory
module Music.Theory.Tuning where

import Data.List
import Data.Ratio

-- * Types

-- | An approximation of a ratio.
type Approximate_Ratio = Double

-- | A real valued division of a tone into one hundred parts.
type Cents = Double

-- | Convert from an 'Approximate_Ratio' to 'Cents'.
--
-- > round (to_cents (3/2)) == 702
to_cents :: Approximate_Ratio -> Cents
to_cents x = 1200 * logBase 2 x

-- | Convert from 'Rational' to 'Approximate_Ratio', ie. 'fromRational'.
approximate_ratio :: Rational -> Approximate_Ratio
approximate_ratio = fromRational

-- | 'to_cents' '.' 'approximate_ratio'.
to_cents_r :: Rational -> Cents
to_cents_r = to_cents . approximate_ratio

-- * Commas

-- | The Syntonic comma.
--
-- > syntonic_comma == 81/80
syntonic_comma :: Rational
syntonic_comma = 81 % 80

-- | The Pythagorean comma.
--
-- > pythagorean_comma == 3^12 / 2^19
pythagorean_comma :: Rational
pythagorean_comma = 531441 / 524288

-- | Mercators comma.
--
-- > mercators_comma == 3^53 / 2^84
mercators_comma :: Rational
mercators_comma = 19383245667680019896796723 / 19342813113834066795298816

-- | Calculate /n/th root of /x/.
--
-- > 12 `nth_root` 2  == twelve_tone_equal_temperament_comma
nth_root :: (Floating a) => a -> a -> a
nth_root n x =
    let f (_,x0) = (x0, ((n-1)*x0+x/x0**(n-1))/n)
        e = uncurry (==)
    in fst (until e f (x, x/n))

-- | 12-tone equal temperament comma (ie. 12th root of 2).
--
-- > twelve_tone_equal_temperament_comma == 1.0594630943592953
twelve_tone_equal_temperament_comma :: (Floating a) => a
twelve_tone_equal_temperament_comma = 12 `nth_root` 2

-- * Tunings

-- | Harmonic series to /n/th harmonic (folded).
--
-- > harmonic_series_folded 3 == [1/2,2/3,1]
harmonic_series_folded :: Integer -> [Rational]
harmonic_series_folded n =
    let hs = (zipWith (%) (repeat 1) [1..n])
        fold x = if x >= 0.5
                 then x
                 else fold (x * 2)
    in nub (sort (map fold hs))

-- | Harmonic series to /n/th harmonic (folded, cents).
--
-- > map round (harmonic_series_folded_c 3) == [-1200,-702,0]
harmonic_series_folded_c :: Integer -> [Cents]
harmonic_series_folded_c = map to_cents_r . harmonic_series_folded

-- | Ditone/pythagorean tuning,
-- see <http://www.billalves.com/porgitaro/ditonesettuning.html>
--
-- > let c = [0,114,204,294,408,498,612,702,816,906,996,1110]
-- > in map (round.to_cents_r) ditone_r == c
ditone_r :: [Rational]
ditone_r =
    [1/1,2187/2048 {- 256/243 -}
    ,9/8,32/27
    ,81/64
    ,4/3,729/512
    ,3/2,6561/4096 {- 128/81 -}
    ,27/16,16/9
    ,243/128]

-- | Pythagorean tuning
--
-- > let c = [0,90,204,294,408,498,612,702,792,906,996,1110]
-- > in map (round.to_cents_r) pythagorean_r == c
pythagorean_r :: [Rational]
pythagorean_r =
    [1,256%243 {- 2187%2048 -}
    ,9%8,32%27
    ,81%64
    ,4%3,729%512
    ,3%2,128%81 {- 6561%4096 -}
    ,27%16,16%9
    ,243%128]

-- | Pythagorean tuning (cents)
pythagorean_c :: [Cents]
pythagorean_c = map to_cents_r pythagorean_r

-- | Werckmeister III, Andreas Werckmeister (1645-1706)
--
-- > let c = [0,90,192,294,390,498,588,696,792,888,996,1092]
-- > in map (round.to_cents) werckmeister_iii_ar == c
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

-- | Werckmeister III, Andreas Werckmeister (1645-1706)
werckmeister_iii_c :: [Cents]
werckmeister_iii_c = map to_cents werckmeister_iii_ar

-- | Werckmeister IV, Andreas Werckmeister (1645-1706)
--
-- > let c = [0,82,196,294,392,498,588,694,784,890,1004,1086]
-- > in map (round.to_cents) werckmeister_iv_ar == c
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

-- | Werckmeister IV, Andreas Werckmeister (1645-1706)
werckmeister_iv_c :: [Cents]
werckmeister_iv_c = map to_cents werckmeister_iv_ar

-- | Werckmeister V, Andreas Werckmeister (1645-1706)
--
-- > let c = [0,96,204,300,396,504,600,702,792,900,1002,1098]
-- > in map (round.to_cents) werckmeister_v_ar == c
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

-- | Werckmeister V, Andreas Werckmeister (1645-1706)
werckmeister_v_c :: [Cents]
werckmeister_v_c = map to_cents werckmeister_v_ar

-- | Werckmeister VI, Andreas Werckmeister (1645-1706)
--
-- > let c = [0,91,196,298,395,498,595,698,793,893,1000,1097]
-- > in map (round.to_cents_r) werckmeister_vi_r == c
werckmeister_vi_r :: [Rational]
werckmeister_vi_r =
    [1,98%93
    ,28%25,196%165
    ,49%39
    ,4%3,196%139
    ,196%131,49%31
    ,196%117,98%55
    ,49%26]

-- | Werckmeister VI, Andreas Werckmeister (1645-1706)
werckmeister_vi_c :: [Cents]
werckmeister_vi_c = map to_cents_r werckmeister_vi_r

-- | Pietro Aaron (1523) meantone temperament, see
-- <http://www.kylegann.com/histune.html>
--
-- > let c = [0,76,193,310,386,503,580,697,773,890,1007,1083,1200]
-- > in map round pietro_aaron_1523_c == c
pietro_aaron_1523_c :: [Cents]
pietro_aaron_1523_c =
    [0,76.0
    ,193.2,310.3
    ,386.3
    ,503.4,579.5
    ,696.8,772.6
    ,889.7,1006.8
    ,1082.9]

-- | Thomas Young (1799) - Well Temperament
--
-- > let c = [0,94,196,298,392,500,592,698,796,894,1000,1092,1200]
-- > in map round thomas_young_1799_c == c
thomas_young_1799_c :: [Cents]
thomas_young_1799_c =
    [0,93.9
    ,195.8,297.8
    ,391.7
    ,499.9,591.9
    ,697.9,795.8
    ,893.8,999.8
    ,1091.8
    ,1200]

-- | Five-limit tuning (five limit just intonation).
--
-- > let c = [0,112,204,316,386,498,590,702,814,884,996,1088]
-- > in map (round.to_cents_r) five_limit_tuning_r == c
five_limit_tuning_r :: [Rational]
five_limit_tuning_r =
    [1,16%15
    ,9%8,6%5
    ,5%4
    ,4%3,45%32
    ,3%2,8%5
    ,5%3,16%9 {- 9%5 -}
    ,15%8]

-- | 'Cents' variant of 'five_limit_tuning_r'.
five_limit_tuning_c :: [Cents]
five_limit_tuning_c = map to_cents_r five_limit_tuning_r

-- | Equal temperament.
--
-- > equal_temperament_c == [0,100..1200]
equal_temperament_c :: [Cents]
equal_temperament_c = [0, 100 .. 1200]

-- > let c = [0,112,204,316,386,498,583,702,814,884,1018,1088]
-- > in map (round.to_cents_r) septimal_tritone_just_intonation == c
septimal_tritone_just_intonation :: [Rational]
septimal_tritone_just_intonation =
    [1,16/15
    ,9/8,6/5
    ,5/4
    ,4/3,7/5
    ,3/2,8/5
    ,5/3,9/5
    ,15/8]

-- > let c = [0,112,204,316,386,498,583,702,814,884,969,1088]
-- > in map (round.to_cents_r) seven_limit_just_intonation == c
seven_limit_just_intonation_r :: [Rational]
seven_limit_just_intonation_r =
    [1,16/15
    ,9/8,6/5
    ,5/4
    ,4/3,7/5
    ,3/2,8/5
    ,5/3,7/4
    ,15/8]

-- > let c = [0,90,193,294,386,498,590,697,792,890,996,1088]
-- > in map (round.to_cents) kirnberger_iii_ar == c
kirnberger_iii_ar :: [Approximate_Ratio]
kirnberger_iii_ar =
    [1,256/243
    ,(sqrt 5)/2,32/27
    ,5/4
    ,4/3,45/32
    ,5 ** 0.25,128/81
    ,(5 ** 0.75)/2,16/9
    ,15/8]

-- > let c = [0,94,196,298,392,502,592,698,796,894,1000,1090]
-- > in map round vallotti_c == c
vallotti_c :: [Cents]
vallotti_c =
    [0.0,94.135
    ,196.09,298.045
    ,392.18
    ,501.955,592.18
    ,698.045,796.09
    ,894.135,1000.0
    ,1090.225]

-- > let c = [0,128,139,359,454,563,637,746,841,911,1072,1183]
-- > in map (round.to_cents_r) mayumi_reinhard == c
mayumi_reinhard :: [Rational]
mayumi_reinhard = [1,14/13
                  ,13/12,16/13
                  ,13/10
                  ,18/13,13/9
		  ,20/13,13/8
                  ,22/13,13/7
                  ,208/105]

-- | La Monte Young's \"The Well-Tuned Piano\", see
-- <http://www.kylegann.com/tuning.html>.
--
-- > let c = [0,177,204,240,471,444,675,702,738,969,942,1173]
-- > in map (round.to_cents_r) la_monte_young_r == c
la_monte_young_r :: [Rational]
la_monte_young_r =
    [1/1,567/512
    ,9/8,147/128
    ,21/16
    ,1323/1024,189/128
    ,3/2,49/32
    ,7/4,441/256
    ,63/32]

-- | Ben Johnston's \"Suite for Microtonal Piano\" (1977), see
-- <http://www.kylegann.com/tuning.html>
--
-- > let c = [0,105,204,298,386,471,551,702,841,906,969,1088]
-- > in map (round.to_cents_r) ben_johnston_r == c
ben_johnston_r :: [Rational]
ben_johnston_r =
    [1/1,17/16
    ,9/8,19/16
    ,5/4
    ,21/16,11/8
    ,3/2,13/8
    ,27/16,7/4
    ,15/8]

-- > let c = [0,112,182,231,267,316,386,498,603,702,814,884,933,969,1018,1088]
-- > in map (round.to_cents_r) lou_harrison_17_r == c
lou_harrison_17_r :: [Rational]
lou_harrison_17_r =
    [1,16/15
    ,10/9,8/7,7/6,6/5
    ,5/4
    ,4/3,17/12
    ,3/2,8/5
    ,5/3,12/7,7/4,9/5
    ,15/8]

-- | Harry Partch 43 tone scale, see
-- <http://www.microtonal-synthesis.com/scale_partch.html>
--
-- > let c = [0,22,53,84,112,151,165
-- >         ,182,204,231,267,294,316
-- >         ,347,386,418,435
-- >         ,471,498,520,551,583,617,649
-- >         ,680,702,729,765,782,814,853,884,906,933
-- >         ,969,996,1018,1035,1049,1088,1116,1147,1178]
-- > in map (round.to_cents_r) partch_43_r == c
partch_43_r :: [Rational]
partch_43_r =
    [1,81/80,33/32,21/20,16/15,12/11,11/10,10/9,9/8,8/7
    ,7/6,32/27,6/5,11/9,5/4,14/11,9/7
    ,21/16,4/3,27/20
    ,11/8,7/5,10/7,16/11
    ,40/27,3/2,32/21,14/9,11/7,8/5,18/11,5/3,27/16,12/7
    ,7/4,16/9,9/5,20/11,11/6,15/8,40/21,64/33,160/81]

-- * Alves

-- | Bill Alves' HMC /slendro/ tuning, see
-- <http://www2.hmc.edu/~alves/pleng.html>
--
-- > let c = [0,231,498,765,996]
-- > in map (round.to_cents_r) alves_slendro_r == c
alves_slendro_r :: [Rational]
alves_slendro_r = [1/1,8/7,4/3,14/9,16/9]

-- | Bill Alves' HMC /pelog bem/ tuning.
--
-- > let c = [0,231,316,702,814]
-- > in map (round.to_cents_r) alves_pelog_bem_r == c
alves_pelog_bem_r :: [Rational]
alves_pelog_bem_r = [1,8/7,6/5,3/2,8/5]

-- | Bill Alves' HMC /pelog 2,3,4,6,7/ tuning.
--
-- > let c = [0,386,471,857,969]
-- > in map (round.to_cents_r) alves_pelog_barang_r == c
alves_pelog_barang_r :: [Rational]
alves_pelog_barang_r = [1,5/4,21/16,105/64,7/4]

-- | Bill Alves' HMC /pelog barang/ tuning.
--
-- > let c = [0,386,471,702,969]
-- > in map (round.to_cents_r) alves_pelog_23467 == c
alves_pelog_23467 :: [Rational]
alves_pelog_23467 = [1,5/4,21/16,3/2,7/4]

-- * Syntonic tuning

-- | Construct an isomorphic layout of /r/ rows and /c/ columns with
-- an upper left value of /(i,j)/.
mk_isomorphic_layout :: Integral a => a -> a -> (a,a) -> [[(a,a)]]
mk_isomorphic_layout n_row n_col top_left =
    let (a,b) `plus` (c,d) = (a+c,b+d)
        mk_seq 0 _ _ = []
        mk_seq n i z = z : mk_seq (n-1) i (z `plus` i)
        left = mk_seq n_row (-1,1) top_left
    in map (mk_seq n_col (-1,2)) left

-- | A minimal isomorphic note layout.
--
-- > let [i,j,k] = mk_isomorphic_layout 3 5 (3,-4)
-- > in [i,take 4 j,(2,-4):take 4 k] == minimal_isomorphic_note_layout
minimal_isomorphic_note_layout :: [[(Int,Int)]]
minimal_isomorphic_note_layout =
    [[(3,-4),(2,-2),(1,0),(0,2),(-1,4)]
       ,[(2,-3),(1,-1),(0,1),(-1,3)]
    ,[(2,-4),(1,-2),(0,0),(-1,2),(-2,4)]]

-- | Make a rank two regular temperament from a list of /(i,j)/
-- positions by applying the scalars /a/ and /b/.
rank_two_regular_temperament :: Integral a => a -> a -> [(a,a)] -> [a]
rank_two_regular_temperament a b = let f (i,j) = i * a + j * b in map f

-- | Syntonic tuning system based on 'mk_isomorphic_layout' of @5@
-- rows and @7@ columns starting at @(3,-4)@ and a
-- 'rank_two_regular_temperament' with /a/ of @1200@ and indicated
-- /b/.
mk_syntonic_tuning :: Int -> [Cents]
mk_syntonic_tuning b =
  let l = mk_isomorphic_layout 5 7 (3,-4)
      t = map (rank_two_regular_temperament 1200 b) l
  in nub (sort (map (\x -> fromIntegral (x `mod` 1200)) (concat t)))

-- | 'mk_syntonic_tuning' of @697@.
--
-- > let c = [0,79,194,273,309,388,467,503,582,697,776,812,891,970,1006,1085,1164]
-- > in map round syntonic_697_c == c
syntonic_697_c :: [Cents]
syntonic_697_c = mk_syntonic_tuning 697

-- | 'mk_syntonic_tuning' of @702@.
--
-- > let c = [0,24,114,204,294,318,408,498,522,612,702,792,816,906,996,1020,1110]
-- > in map round syntonic_702_c == c
syntonic_702_c :: [Cents]
syntonic_702_c = mk_syntonic_tuning 702
