{- | Zwicker, E. (1961) "Subdivision of the audible frequency range into critical bands"
  The Journal of the Acoustical Society of America, Volume 33, Issue 2, p. 248 (1961)

<https://ccrma.stanford.edu/courses/120-fall-2003/lecture-5.html>
-}
module Music.Theory.Pitch.Bark where

import qualified Music.Theory.List as List {- hmt-base -}

-- * Tables

-- | Center freqencies of Bark scale critical bands (hz).
bark_center :: Num n => [n]
bark_center =
  [ 50
  , 150
  , 250
  , 350
  , 450
  , 570
  , 700
  , 840
  , 1000
  , 1170
  , 1370
  , 1600
  , 1850
  , 2150
  , 2500
  , 2900
  , 3400
  , 4000
  , 4800
  , 5800
  , 7000
  , 8500
  , 10500
  , 13500
  ]

-- | Edge freqencies of Bark scale critical bands (hz).
bark_edge :: Num n => [n]
bark_edge =
  [ 0
  , 100
  , 200
  , 300
  , 400
  , 510
  , 630
  , 770
  , 920
  , 1080
  , 1270
  , 1480
  , 1720
  , 2000
  , 2320
  , 2700
  , 3150
  , 3700
  , 4400
  , 5300
  , 6400
  , 7700
  , 9500
  , 12000
  , 15500
  ]

-- | Bandwidths of Bark scale critical bands (hz).
bark_bandwidth :: Num n => [n]
bark_bandwidth = let c = bark_edge in zipWith (-) (List.tail_err c) c

-- * Functions

{- | Zwicker & Terhardt (1980)

>>> map (round . cps_to_bark_zwicker) bark_center == concat [[0..7],[9..15],[15..19],[21..24]]
True

> let f = [0,100 .. 8000] in Sound.Sc3.Plot.plot_p2_ln [zip f (map cps_to_bark_zwicker f)]
-}
cps_to_bark_zwicker :: Floating a => a -> a
cps_to_bark_zwicker x = 13 * atan (0.00076 * x) + 3.5 * atan ((x / 7500) ** 2)

{- | Traunmüller, Hartmut.
   "Analytical Expressions for the Tonotopic Sensory Scale."
   Journal of the Acoustical Society of America. Vol. 88, Issue 1, 1990, pp. 97-100.

>>> map (round . cps_to_bark_traunmuller) bark_center
[0,1,3,4,4,6,7,8,9,9,10,12,12,13,14,15,16,17,19,20,20,22,22,23]

> let f = [0,100 .. 8000] in Sound.Sc3.Plot.plot_p2_ln [zip f (map cps_to_bark_traunmuller f)]
-}
cps_to_bark_traunmuller :: (Fractional n, Ord n) => n -> n
cps_to_bark_traunmuller x =
  let y = ((26.81 * x) / (1960 + x)) - 0.53
  in if y < 2 then y + 0.15 * (2 - y) else if y > 20.1 then y + 0.22 * (y - 20.1) else y

{- | Traunmüller (1990)

> Sound.Sc3.Plot.plot_p2_ln [zip (map bark_to_cps_traunmuller [0..23]) [0..23]]
-}
bark_to_cps_traunmuller :: (Fractional n, Ord n) => n -> n
bark_to_cps_traunmuller y =
  let f x = 1960 * ((x + 0.53) / (26.28 - x))
  in if y < 2 then f ((y - 0.3) / 0.85) else if y > 20.1 then f ((y + 4.422) / 1.22) else f y

{- | Wang, Sekey & Gersho (1992)

>>> map (round . cps_to_bark_wsg) bark_center == concat [[0..9],[9..21],[23]]
True

> let f = [0,100 .. 8000] in Sound.Sc3.Plot.plot_p2_ln [zip f (map cps_to_bark_wsg f)]
-}
cps_to_bark_wsg :: Floating a => a -> a
cps_to_bark_wsg x = 6 * asinh (x / 600)

{- | Wang, Sekey & Gersho (1992)

>>> r = [100,204,313,430,560,705,870,1059,1278,1532,1828,2176,2584,3065,3630,4297,5083,6011,7106,8399]
>>> map (round . bark_to_cps_wsg) [1 .. 20] == r
True

> Sound.Sc3.Plot.plot_p2_ln [zip (map bark_to_cps_wsg [0..23]) [0..23]]
-}
bark_to_cps_wsg :: Floating a => a -> a
bark_to_cps_wsg x = 600 * sinh (x / 6)
