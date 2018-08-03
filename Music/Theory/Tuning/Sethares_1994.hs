-- | William A. Sethares.
-- "Adaptive Tunings for Musical Scales".
-- /Journal of the Acoustical Society of America/, 96(1), July 1994.
--
-- <http://sethares.engr.wisc.edu/consemi.html>
module Music.Theory.Tuning.Sethares_1994 where

import qualified Music.Theory.Tuning as T

-- > import Sound.SC3.Plot
-- > plotTable1 (map (\f -> d (220,1) (f,1)) [220 .. 440])
d :: (Floating n, Ord n) => (n,n) -> (n,n) -> n
d (f1,v1) (f2,v2) =
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

-- > plotTable fig_1
fig_1 :: (Floating n,Enum n,Ord n) => [[n]]
fig_1 =
    let f0 = [125,250,500,1000,2000]
        r_seq = map T.cents_to_fratio [0 .. 1200]
    in map (\f -> map (\r -> d (f,1) (f * r,1)) r_seq) f0

-- > let a_seq = take 7 (iterate (* 0.88) 1.0)
-- > let gen f0 = zipWith (\r a -> (f0 * r,a)) [1 .. 7] a_seq
-- > let r_seq = map T.cents_to_ratio [0,1 .. 1200]
-- > plotTable1 (let f0 = 880 in map (\r -> d_h (gen f0) (gen (f0 * r))) r_seq)
d_h :: (Floating n, Ord n) => [(n,n)] -> [(n,n)] -> n
d_h s1 s2 = sum [d p q | p <- s1, q <- s2]
