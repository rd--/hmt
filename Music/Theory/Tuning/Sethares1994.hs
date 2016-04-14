-- | William A. Sethares.
-- "Adaptive Tunings for Musical Scales".
-- /Journal of the Acoustical Society of America/, 96(1), July 1994.
module Music.Theory.Tuning.Sethares1994 where

import qualified Music.Theory.Tuning as T

-- > import Sound.SC3.Plot
-- > let c = map (\f -> d 220 f 1 1) [220 .. 440]
-- > plotTable1 c
d :: (Floating n, Ord n) => n -> n -> n -> n -> n
d f1 f2 v1 v2 =
    let a = 3.5
        a_ = negate a
        b = 5.75
        b_ = negate b
        d' = 0.24
        s1 = 0.021
        s2 = 19
        s = d' / (s1 * min f1 f2 + s2)
        e1 = exp (a_ * s * abs (f2 - f1))
        e2 = exp (b_ * s * abs (f2 - f1))
    in v1 * v2 * (e1 - e2)

-- > plotTable fig_1
fig_1 :: (Floating n,Enum n,Ord n) => [[n]]
fig_1 =
    let f0 = [125,250,500,1000,2000]
        r_seq = map T.cents_to_ratio [0 .. 1200]
    in map (\f -> map (\r -> d f (f * r) 1 1) r_seq) f0

