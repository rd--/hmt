-- | Tuning theory
module Music.Theory.Tuning where

import Data.List
import Data.Ratio

-- | An approximation of a ratio.
type Approximate_Ratio = Double

-- | A real valued division of a tone into one hundred parts.
type Cents = Double

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
harmonic_series_folded_c =
    let f = to_cents . approximate_ratio
    in map f . harmonic_series_folded

-- | Pythagorean tuning
pythagorean_r :: [Rational]
pythagorean_r =
    [1%1,243%256 {- 2048%2187 -}
    ,8%9,27%32
    ,64%81
    ,3%4,512%729
    ,2%3,81%128
    ,16%27,9%16
    ,128%243
    ,1%2]

-- | Pythagorean tuning (cents)
pythagorean_c :: [Cents]
pythagorean_c = map (to_cents.approximate_ratio) pythagorean_r

-- | Werckmeister III, Andreas Werckmeister (1645-1706)
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
werckmeister_vi_c = map (to_cents.approximate_ratio) werckmeister_vi_r

-- | Pietro Aaron (1523) - Meantone temperament
pietro_aaron_1523_c :: [Cents]
pietro_aaron_1523_c =
    [0,76.0
    ,193.2,310.3
    ,386.3
    ,503.4,579.5
    ,696.8,772.6
    ,889.7,1006.8
    ,1082.9
    ,1200]

-- | Thomas Young (1799) - Well Temperament
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

-- | Five-limit tuning
five_limit_tuning_r :: [Rational]
five_limit_tuning_r =
    [1%1,15%16
    ,8%9,5%6
    ,4%5
    ,3%4,32%45
    ,2%3,5%8
    ,3%5,9%16
    ,8%15
    ,1%2]

-- | 'Cents' variant of 'five_limit_tuning_r'.
five_limit_tuning_c :: [Cents]
five_limit_tuning_c = map (to_cents.approximate_ratio) five_limit_tuning_r

-- | Equal temperament.
--
-- > equal_temperament_c == [0,100..1200]
equal_temperament_c :: [Cents]
equal_temperament_c = [0, 100 .. 1200]

-- | Construct an isomorphic layout of /r/ rows and /c/ columns with
-- an upper left value of /(i,j)/.
mk_isomorphic_layout :: Integral a => a -> a -> (a,a) -> [[(a,a)]]
mk_isomorphic_layout n_row n_col top_left =
    let (a,b) `plus` (c,d) = (a+c,b+d)
        mk_seq 0 _ _ = []
        mk_seq n i z = z : mk_seq (n-1) i (z `plus` i)
        left = mk_seq n_row (-1,1) top_left
    in map (mk_seq n_col (-1,2)) left

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
-- > take 10 (map round syntonic_697_c) == [0,79,194,273,309,388,467,503,582,697]
syntonic_697_c :: [Cents]
syntonic_697_c = mk_syntonic_tuning 697

-- | 'mk_syntonic_tuning' of @702@.
--
-- > take 11 (map round syntonic_702_c) == [0,24,114,204,294,318,408,498,522,612,702]
syntonic_702_c :: [Cents]
syntonic_702_c = mk_syntonic_tuning 702

-- | The Syntonic comma.
--
-- > syntonic_comma == 81/80
syntonic_comma :: Rational
syntonic_comma = 81 % 80

-- | The Pythagorean comma.
--
-- > pythagorean_comma == 3^12 % 2^19
pythagorean_comma :: Rational
pythagorean_comma = 531441 % 524288

-- | Mercators comma.
--
-- > mercators_comma == 3^53 % 2^84
mercators_comma :: Rational
mercators_comma = 19383245667680019896796723 % 19342813113834066795298816

-- | Convert from 'Rational' to 'Approximate_Ratio', ie. 'fromRational'.
approximate_ratio :: Rational -> Approximate_Ratio
approximate_ratio = fromRational

-- | Convert from an 'Approximate_Ratio' to 'Cents'.
--
-- > round (to_cents (3/2)) == 702
to_cents :: Approximate_Ratio -> Cents
to_cents x = 1200 * logBase 2 x

-- | Calculate /n/th root of /x/.
--
-- > 12 `nth_root` 2  == twelve_tone_equal_temperament_comma
nth_root :: (Floating a) => a -> a -> a
nth_root n x =
    let f (_,x0) = (x0, ((n-1)*x0+x/x0**(n-1))/n)
        e = uncurry (==)
    in fst (until e f (x, x/n))

-- | 12-tone equal temperament comma (ie. 12th root of 2).
twelve_tone_equal_temperament_comma :: (Floating a) => a
twelve_tone_equal_temperament_comma = 12 `nth_root` 2

-- | A minimal isomorphic note layout.
--
-- > let [i,j,k] = mk_isomorphic_layout 3 5 (3,-4)
-- > in [i,take 4 j,(2,-4):take 4 k] == minimal_isomorphic_note_layout
minimal_isomorphic_note_layout :: [[(Int,Int)]]
minimal_isomorphic_note_layout =
    [[(3,-4),(2,-2),(1,0),(0,2),(-1,4)]
       ,[(2,-3),(1,-1),(0,1),(-1,3)]
    ,[(2,-4),(1,-2),(0,0),(-1,2),(-2,4)]]
