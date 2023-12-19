{- | Larry Polansky.
\"Tuning Systems in American Gamelan, Part I: Interval Sizes in Javanese Slendro\".
/Balungan/, 1(2):9-11, 1984
-}
module Music.Theory.Tuning.Polansky_1984 where

import Data.List {- base -}

import qualified Music.Theory.List as List

import qualified Music.Theory.Tuning as Tuning

k_manisrenga :: Fractional n => [n]
k_manisrenga = [219.5, 266.5, 227, 233.5, 258.5]

k_kanjutmesem :: Fractional n => [n]
k_kanjutmesem = [224, 253.5, 237.5, 232.5, 264]

k_udanriris :: Fractional n => [n]
k_udanriris = [255.5, 256.5, 223.5, 235.5, 234]

k_pengawesari :: Fractional n => [n]
k_pengawesari = [251.5, 233.5, 233.5, 236, 250]

k_rarasrum :: Fractional n => [n]
k_rarasrum = [229.5, 227.5, 253, 232, 261.5]

k_hardjanagara :: Fractional n => [n]
k_hardjanagara = [216, 249.5, 216, 262, 261.5]

k_madukentir :: Fractional n => [n]
k_madukentir = [268.5, 242, 243, 230, 221]

k_surak :: Fractional n => [n]
k_surak = [206, 231.5, 238.5, 265, 264.5]

{- | The set of /K/ slendro tunings.

>>> map length k_set == replicate (length k_set) 5
True

>>> minimum (concat k_set)
206.0

>>> maximum (concat k_set)
268.5
-}
k_set :: Fractional n => [[n]]
k_set =
  [ k_manisrenga
  , k_kanjutmesem
  , k_udanriris
  , k_pengawesari
  , k_rarasrum
  , k_hardjanagara
  , k_madukentir
  , k_surak
  ]

{- | Given a set of equal length lists calculate the average value of each position.

>>> calculate_averages [[1,2,3],[3,2,1]]
[2.0,2.0,2.0]
-}
calculate_averages :: Fractional n => [[n]] -> [n]
calculate_averages set =
  let n = fromIntegral (length set)
      z = map sum (transpose set)
  in map (/ n) z

{- | Averages of /K/ set, p. 10.

>>> k_averages
[233.8125,245.0625,234.0,240.8125,251.875]
-}
k_averages :: Fractional n => [n]
k_averages = calculate_averages k_set

gm_1, gm_2, gm_3, gm_4, gm_5, gm_6, gm_7, gm_8 :: Fractional n => [n]
gm_1 = [237, 251, 248, 242, 258]
gm_2 = [252, 239, 242, 236.5, 253.5]
gm_3 = [237, 238.5, 232.5, 262, 238]
gm_4 = [226, 252, 260, 234, 256]
gm_5 = [232, 239, 248, 232, 259.5]
gm_6 = [218, 238.5, 244.5, 244.5, 260]
gm_7 = [238, 230, 257, 243, 250.5]
gm_8 = [232, 234, 249, 251, 257]

{- | The set of /GM/ (Gadja Mada University) slendro tunings.

>>> map length gm_set == replicate (length gm_set) 5
True

>>> minimum (concat gm_set)
218.0

>>> maximum (concat gm_set)
262.0
-}
gm_set :: Fractional n => [[n]]
gm_set = [gm_1, gm_2, gm_3, gm_4, gm_5, gm_6, gm_7, gm_8]

{- | Averages of /GM/ set, p. 10.

>>> gm_averages
[234.0,240.25,247.625,243.125,254.0625]
-}
gm_averages :: Fractional n => [n]
gm_averages = calculate_averages gm_set

-- | Association list giving interval boundaries for interval class categories (pp.10-11).
i_categories :: Num n => [((n, n), String)]
i_categories =
  [ ((206, 238), "S")
  , ((238, 240), "S-E")
  , ((240, 248), "E")
  , ((248, 250), "E-L")
  , ((250, 269), "L")
  ]

-- | Categorise an interval.
i_category :: (Ord a, Num a) => a -> String
i_category x =
  let f n (i, j) = i <= n && n < j
  in maybe "U" snd (find (f x . fst) i_categories)

{- | Pretty interval category table (pp. 10-11).

> putStrLn $ unlines $ i_category_table k_set

@
S    L    S    S    L
S    L    S    S    L
L    L    S    S    S
L    S    S    S    L
S    S    L    S    L
S    E-L  S    L    L
L    E    E    S    S
S    S    S-E  L    L
@

> putStrLn $ unlines $ i_category_table gm_set

@
S    L    E-L  E    L
L    S-E  E    S    L
S    S-E  S    L    S-E
S    L    L    S    L
S    S-E  E-L  S    L
S    S-E  E    E    L
S-E  S    L    E    L
S    S    E-L  L    L
@
-}
i_category_table :: (Ord a, Num a) => [[a]] -> [String]
i_category_table = map (intercalate "  " . map (List.pad_right ' ' 3 . i_category))

{- | Rational tuning derived from 'gm_averages', p.11.

>>> polansky_1984_r == sort polansky_1984_r
True

>>> polansky_1984_r == [1/1,8/7,21/16,512/343,12/7,96/49]
True

>>> List.d_dx polansky_1984_r == [1/7,19/112,989/5488,76/343,12/49]
True
-}
polansky_1984_r :: [Rational]
polansky_1984_r =
  let vi = 12 / 7
      v = 128 / 147 * vi
      i' = 21 / 16 * v
  in [1, 8 / 7, 21 / 16, v, vi, i']

{- | 'ratio_to_cents' of 'polansky_1984_r'.

>>> map round (List.d_dx polansky_1984_c)
[231,240,223,240,231]
-}
polansky_1984_c :: [Tuning.Cents]
polansky_1984_c = map Tuning.ratio_to_cents polansky_1984_r
