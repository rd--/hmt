-- | Kyle Gann. "La Monte Young's The Well-Tuned Piano".
-- /Perspectives of New Music/, 31(1):134--162, Winter 1993.
module Music.Theory.Tuning.Gann_1993 where

import Music.Theory.Tuning {- hmt -}
import qualified Music.Theory.Tuning.Euler as T {- hmt -}

{- | Ratios for 'lmy_wtp'. lmy = La Monte Young. wtp = Well-Tuned Piano.

> let c = [0,177,204,240,471,444,675,702,738,969,942,1173]
> in map (round . ratio_to_cents) lmy_wtp_r == c

-}
lmy_wtp_r :: [Rational]
lmy_wtp_r =
    [1,567/512
    ,9/8,147/128
    ,21/16
    ,1323/1024,189/128
    ,3/2,49/32
    ,7/4,441/256
    ,63/32]

{- | Gann, 1993, p.137.

> cents_i lmy_wtp == [0,177,204,240,471,444,675,702,738,969,942,1173]

> import Data.List {- base -}
> import Music.Theory.Tuning.Scala {- hmt -}
> scl <- scl_load "young-lm_piano"
> cents_i (scale_to_tuning 0.01 scl) == cents_i lmy_wtp

> let f = d12_midi_tuning_f (lmy_wtp,-74.7,-3)
> import qualified Music.Theory.Pitch as T
> T.octpc_to_midi (-1,11) == 11
> map (round . T.midi_detune_to_cps . f) [62,63,69] == [293,298,440]
> map (fmap round . T.midi_detune_normalise . f) [0 .. 127]

-}
lmy_wtp :: Tuning
lmy_wtp = Tuning (Left lmy_wtp_r) 2

-- | Ratios for 'lmy_wtp_1964.
lmy_wtp_1964_r :: [Rational]
lmy_wtp_1964_r =
    let n = [1,279,9,147,21,93,189,3,49,7,31,63]
        d = [1,256,8,128,16,64,128,2,32,4,16,32]
    in zipWith (/) n d

{- | La Monte Young's initial 1964 tuning for \"The Well-Tuned Piano\" (Gann, 1993, p.141).

> cents_i lmy_wtp_1964 == [0,149,204,240,471,647,675,702,738,969,1145,1173]

> import Music.Theory.Tuning.Scala
> let nm = ("young-lm_piano_1964","LaMonte Young's Well-Tuned Piano (1964)")
> let scl = tuning_to_scale nm lmy_wtp_1964
> putStr $ unlines $ scale_pp scl

-}
lmy_wtp_1964 :: Tuning
lmy_wtp_1964 = Tuning (Left lmy_wtp_1964_r) 2

{- | Euler diagram for 'lmy_wtp'.

let dir = "/home/rohan/sw/hmt/data/dot/"
let f = unlines . T.euler_plane_to_dot_rat (3,True)
writeFile (dir ++ "euler-wtp.dot") (f lmy_wtp_euler)

-}
lmy_wtp_euler :: T.Euler_Plane Rational
lmy_wtp_euler =
    let {l1 = T.tun_seq 4 (3/2) (49/32)
        ;l2 = T.tun_seq 5 (3/2) (7/4)
        ;l3 = T.tun_seq 3 (3/2) (1/1)
        ;(c1,c2) = T.euler_align_rat (7/4,7/4) (l1,l2,l3)}
    in ([l1,l2,l3],c1 ++ c2)
