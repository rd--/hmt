-- | Kyle Gann. "La Monte Young's The Well-Tuned Piano".
-- /Perspectives of New Music/, 31(1):134--162, Winter 1993.
module Music.Theory.Tuning.Gann_1993 where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Math as T {- hmt -}
import qualified Music.Theory.Pitch as T {- hmt -}
import qualified Music.Theory.Tuning as T {- hmt -}
import qualified Music.Theory.Tuning.Euler as T {- hmt -}
import qualified Music.Theory.Z as T {- hmt -}

{- | Ratios for 'lmy_wtp'. lmy = La Monte Young. wtp = Well-Tuned Piano.

> let c = [0,177,204,240,471,444,675,702,738,969,942,1173]
> in map (round . T.ratio_to_cents) lmy_wtp_r == c

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

-- | The pitch-class of the key associated with each ratio of the tuning.
--
-- > mapMaybe lmy_wtp_ratio_to_pc [1,1323/1024,7/4] == [3,8,0]
lmy_wtp_ratio_to_pc :: Rational -> Maybe T.PitchClass
lmy_wtp_ratio_to_pc r = fmap (T.mod12 . (+ 3)) (elemIndex r lmy_wtp_r)

lmy_wtp_ratio_to_pc_err :: Rational -> T.PitchClass
lmy_wtp_ratio_to_pc_err = fromMaybe (error "lmy_wtp_ratio_to_pc") . lmy_wtp_ratio_to_pc

-- | The list of all non-unison ascending intervals possible in 'lmy_wtp_r'.
--
-- > length lmy_wtp_univ == 66
lmy_wtp_univ :: [(Rational,(T.PitchClass,T.PitchClass))]
lmy_wtp_univ =
    let f (p,q) = if p < q
                  then Just (T.ratio_interval_class (p/q)
                            ,(lmy_wtp_ratio_to_pc_err p
                             ,lmy_wtp_ratio_to_pc_err q))
                  else Nothing
    in mapMaybe f (T.all_pairs lmy_wtp_r lmy_wtp_r)

{- | Collated and sorted 'lmy_wtp_univ'.

> let r_cents_pp = show . round . T.ratio_to_cents

> import qualified Music.Theory.Math as T {- hmt -}

> let f (r,i) = concat [T.ratio_pp r," = "
>                      ,r_cents_pp r," = #"
>                      ,show (length i)," = "
>                      ,unwords (map show i)]

> putStrLn $ unlines $ map f lmy_wtp_uniq

3:2 = 702 = #9 = (3,10) (4,9) (5,10) (6,11) (6,1) (7,0) (7,2) (8,1) (9,2)
7:4 = 969 = #7 = (3,0) (5,2) (6,7) (7,10) (8,9) (11,0) (1,2)
7:6 = 267 = #6 = (4,8) (5,7) (6,2) (7,11) (9,1) (10,0)
9:7 = 435 = #4 = (4,1) (5,0) (6,9) (11,2)
9:8 = 204 = #6 = (3,5) (4,2) (6,8) (7,9) (11,1) (0,2)
21:16 = 471 = #6 = (3,7) (5,9) (6,0) (7,1) (8,2) (10,2)
27:14 = 1137 = #2 = (4,6) (9,11)
27:16 = 906 = #3 = (4,7) (8,11) (9,0)
49:32 = 738 = #3 = (3,11) (5,1) (6,10)
49:36 = 534 = #1 = (5,11)
63:32 = 1173 = #5 = (3,2) (4,5) (8,7) (9,10) (1,0)
49:48 = 36 = #2 = (5,6) (10,11)
81:56 = 639 = #1 = (4,11)
81:64 = 408 = #1 = (4,0)
147:128 = 240 = #3 = (3,6) (5,8) (10,1)
189:128 = 675 = #3 = (3,9) (4,10) (8,0)
441:256 = 942 = #2 = (3,1) (8,10)
567:512 = 177 = #1 = (3,4)
1323:1024 = 444 = #1 = (3,8)

-}
lmy_wtp_uniq :: [(Rational,[(T.PitchClass,T.PitchClass)])]
lmy_wtp_uniq = sortOn (T.ratio_nd_sum . fst) $ T.collate_on fst snd $ lmy_wtp_univ

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
lmy_wtp :: T.Tuning
lmy_wtp = T.Tuning (Left lmy_wtp_r) 2

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
lmy_wtp_1964 :: T.Tuning
lmy_wtp_1964 = T.Tuning (Left lmy_wtp_1964_r) 2

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
