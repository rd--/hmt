-- | William A. Sethares.
-- "Adaptive Tunings for Musical Scales".
-- /Journal of the Acoustical Society of America/, 96(1), July 1994.
module Music.Theory.Tuning.Sethares_1994 where

import Data.Maybe {- base -}

import qualified Music.Theory.Tuning as T {- hmt -}

{- | Plomp-Levelt consonance curve.

R. Plomp and W. J. M. Levelt,
"Tonal Consonance and Critical Bandwidth,"
Journal of the Acoustical Society of America.38, 548-560 (1965).

"Relating Tuning and Timbre" <http://sethares.engr.wisc.edu/consemi.html>
MATLAB: <https://sethares.engr.wisc.edu/comprog.html>

> import Sound.SC3.Plot {- hsc3-plot -}
> plot_p1_ln [map (\f -> pl_dissonance (220,1) (f,1)) [220 .. 440]]
-}
pl_dissonance :: (Floating n, Ord n) => (n,n) -> (n,n) -> n
pl_dissonance (f1,v1) (f2,v2) =
    let d_star = 0.24
        s1 = 0.0207
        s2 = 18.96
        c1 = 5
        c2 = -5
        a1 = -3.51
        a2 = -5.75
        s = d_star / (s1 * min f1 f2 + s2)
        f_dif = abs (f2 - f1)
        e1 = c1 * exp (a1 * s * f_dif)
        e2 = c2 * exp (a2 * s * f_dif)
    in v1 * v2 * (e1 + e2)

-- | Sum of 'pl_dissonance' for all p in s1 and all q in s2.
pl_dissonance_h :: (Floating n, Ord n) => [(n,n)] -> [(n,n)] -> n
pl_dissonance_h s1 s2 = sum [pl_dissonance p q | p <- s1, q <- s2]

-- | Return local minima of sequence with index.
local_minima :: Ord t => [t] -> [(Int,t)]
local_minima =
  let f (ix,i,j,k) = if j <= i && j <= k then Just (ix,j) else Nothing
      triples ix l = case l of
                       i:j:k:_ -> (ix,i,j,k) : triples (ix + 1) (tail l)
                       _ -> []
  in mapMaybe f . triples 1

-- | William A. Sethares "Adaptive Tunings for Musical Scales".
--
-- > plot_p1_ln atms_fig_1
atms_fig_1 :: (Floating n,Enum n,Ord n) => [[n]]
atms_fig_1 =
    let f0 = [125,250,500,1000,2000]
        r_seq = map T.cents_to_fratio [0 .. 1200]
    in map (\f -> map (\r -> pl_dissonance (f,1) (f * r,1)) r_seq) f0

-- > plot_p1_ln [atms_fig_2 880]
-- > map fst (local_minima (atms_fig_2 880)) == [204,231,267,316,386,435,498,583,702,814,884,969,1018]
atms_fig_2 :: (Ord t, Floating t, Enum t) => t -> [t]
atms_fig_2 f0 =
  let gen fq = map (\r -> (fq * r,1)) [1 .. 9]
      r_seq = map T.cents_to_fratio [0,1 .. 1200]
  in map (\r -> pl_dissonance_h (gen f0) (gen (f0 * r))) r_seq

-- > Sound.SC3.Plot.plot_p1_ln [atms_fig_3 880]
-- > map fst (local_minima (atms_fig_3 880)) == [267,400,533,667,800,933,1043]
atms_fig_3 :: (Ord t, Floating t, Enum t) => t -> [t]
atms_fig_3 f0 =
  let b = 2 ** (1/9)
      gen fq = map (\r -> (fq * r,1)) (1 : map (b **) [9,14,18,21,25,27,30])
      r_seq = map T.cents_to_fratio [0,1 .. 1200]
  in map (\r -> pl_dissonance_h (gen f0) (gen (f0 * r))) r_seq

-- | "Relating Tuning and Timbre" <http://sethares.engr.wisc.edu/consemi.html>
--
-- > plot_p1_ln [rtt_fig_2 880]
-- > map fst (local_minima (rtt_fig_2 880)) == [267,316,386,498,582,702,884,969]
rtt_fig_2 :: (Ord t, Floating t, Enum t) => t -> [t]
rtt_fig_2 f0 =
  let a_seq = take 7 (iterate (* 0.88) 1.0)
      gen fq = zipWith (\r a -> (fq * r,a)) [1 .. 7] a_seq
      r_seq = map T.cents_to_fratio [0,1 .. 1200]
  in map (\r -> pl_dissonance_h (gen f0) (gen (f0 * r))) r_seq
