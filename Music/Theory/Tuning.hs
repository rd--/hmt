module Music.Theory.Tuning where

import Data.List
import Data.Ratio

type Cents = Double

-- | Harmonic series (folded)
harmonic_series_folded :: Integer -> [Rational]
harmonic_series_folded n =
    let hs = (zipWith (%) (repeat 1) [1..n])
        fold x = if x >= 0.5
                 then x
                 else fold (x * 2)
    in nub (sort (map fold hs))

-- | Pythagorean tuning
pythagorean :: [Rational]
pythagorean =
    [1%1,243%256 {- 2048%2187 -}
    ,8%9,27%32
    ,64%81
    ,3%4,512%729
    ,2%3,81%128
    ,16%27,9%16
    ,128%243
    ,1%2]

-- | Werckmeister III, Andreas Werckmeister (1645-1706)
werckmeister_iii :: [Cents]
werckmeister_iii =
    [0,90.225
    ,192.18,294.135
    ,390.225
    ,498.045,588.27
    ,696.09,792.18
    ,888.27,996.09
    ,1092.18
    ,1200]

-- | Pietro Aaron (1523) - Meantone temperament
pietro_aaron_1523 :: [Cents]
pietro_aaron_1523 =
    [0,76.0
    ,193.2,310.3
    ,386.3
    ,503.4,579.5
    ,696.8,772.6
    ,889.7,1006.8
    ,1082.9
    ,1200]

-- | Thomas Young (1799) - Well Temperament
thomas_young_1799 :: [Cents]
thomas_young_1799 =
    [0,93.9
    ,195.8,297.8
    ,391.7
    ,499.9,591.9
    ,697.9,795.8
    ,893.8,999.8
    ,1091.8
    ,1200]

-- | Five-limit tuning
five_limit_tuning :: [Rational]
five_limit_tuning =
    [1%1,15%16
    ,8%9,5%6
    ,4%5
    ,3%4,32%45
    ,2%3,5%8
    ,3%5,9%16
    ,8%15
    ,1%2]

equal_temperament :: [Cents]
equal_temperament = [0, 100 .. 1200]

mk_isomorphic_layout :: Integral a => a -> a -> (a,a) -> [[(a,a)]]
mk_isomorphic_layout n_row n_col top_left =
    let (a,b) `plus` (c,d) = (a+c,b+d)
        mk_seq 0 _ _ = []
        mk_seq n i z = z : mk_seq (n-1) i (z `plus` i)
        left = mk_seq n_row (-1,1) top_left
    in map (\i -> mk_seq n_col (-1,2) i) left

rank_two_regular_temperament :: Integral a => a -> a -> [(a,a)] -> [a]
rank_two_regular_temperament a b =
    map (\(a', b') -> a * a' + b * b')

mk_syntonic_tuning :: Int -> [Cents]
mk_syntonic_tuning b =
  let l = mk_isomorphic_layout 5 7 (3,-4)
      t = map (rank_two_regular_temperament 1200 b) l
  in nub (sort (map (\x -> fromIntegral (x `mod` 1200)) (concat t)))

syntonic_697 :: [Cents]
syntonic_697 = mk_syntonic_tuning 697

syntonic_702 :: [Cents]
syntonic_702 = mk_syntonic_tuning 702

syntonic_comma :: Rational
syntonic_comma = 81 % 80

-- ie. 3^12 % 2^19
pythagorean_comma :: Rational
pythagorean_comma = 531441 % 524288

-- ie. 3^53 % 2^84
mercators_comma :: Rational
mercators_comma = 19383245667680019896796723 % 19342813113834066795298816

to_cents :: [Rational] -> [Cents]
to_cents = map ((+ (-1200)) . (* 1200) . fromRational)

nth_root :: (Floating a) => a -> a -> a
nth_root n x =
    let f (_,x0) = (x0, ((n-1)*x0+x/x0**(n-1))/n)
        e = uncurry (==)
    in fst (until e f (x, x/n))

twelve_tone_equal_temperament_comma :: (Floating a) => a
twelve_tone_equal_temperament_comma = 12 `nth_root` 2

minimal_isomorphic_note_layout :: [[(Int,Int)]]
minimal_isomorphic_note_layout =
    [[(3,-4),(2,-2),(1,0),(0,2),(-1,4)]
       ,[(2,-3),(1,-1),(0,1),(-1,3)]
    ,[(2,-4),(1,-2),(0,0),(-1,2),(-2,4)]]
