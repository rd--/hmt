{- | David Rosenboom, "In the Beginning: Etude I (Trombones)", 1979
<http://davidrosenboom.com/media/beginning-etude-i-trombones>

kw: subharmonics, difference tones
-}
module Music.Theory.Tuning.Rosenboom_1979 where

import Data.List {- base -}
import Data.Ratio {- base -}

import qualified Music.Theory.Function as Function {- hmt-base -}
import qualified Music.Theory.List as List {- hmt-base -}
import qualified Music.Theory.Tuple as Tuple {- hmt-base -}

import qualified Music.Theory.Pitch as Pitch {- hmt -}
import qualified Music.Theory.Pitch.Name as Pitch.Name {- hmt -}
import qualified Music.Theory.Tuning.Et as Et {- hmt -}
import qualified Music.Theory.Tuning.Scala as Scala {- hmt -}

t2_to_ratio :: (Integer, Integer) -> Rational
t2_to_ratio (n, d) = n % d

{- | Tuning, ratios for each octave.

>>> length (concat dr_tuning_oct)
19

>>> import qualified Music.Theory.Tuning as Tuning
>>> map (map (round . Tuning.ratio_to_cents . t2_to_ratio)) dr_tuning_oct
[[0,498,649,814,996],[0,231,498,702,814,996],[0,204,386,498,551,702,814,969]]
-}
dr_tuning_oct :: Num n => [[(n, n)]]
dr_tuning_oct =
  [ [(1, 1), (4, 3), (16, 11), (8, 5), (16, 9)]
  , [(1, 1), (8, 7), (4, 3), (3, 2), (8, 5), (16, 9)]
  , [(1, 1), (9, 8), (5, 4), (4, 3), (11, 8), (3, 2), (8, 5), (7, 4)]
  ]

-- | Tuning, actual ratios.
dr_tuning :: [Rational]
dr_tuning = concat (zipWith (\o -> map ((* o) . t2_to_ratio)) [1, 2, 4] dr_tuning_oct)

{- | Actual scale, in Cps.

>>> map round dr_scale
[52,69,76,83,92,104,119,138,156,166,185,208,234,260,277,286,311,332,363]

>>> map Pitch.cps_to_midi dr_scale
[32,37,38,40,42,44,46,49,51,52,54,56,58,60,61,62,63,64,66]

>>> map (\x -> (Pitch.cps_to_midi x, x)) dr_scale
[(32,51.91308719749315),(37,69.21744959665753),(38,75.50994501453549),(40,83.06093951598905),(42,92.28993279554336),(44,103.8261743949863),(46,118.65848502284148),(49,138.43489919331506),(51,155.73926159247944),(52,166.1218790319781),(54,184.57986559108673),(56,207.6523487899726),(58,233.60889238871917),(60,259.5654359874657),(61,276.8697983866301),(62,285.5219795862123),(63,311.4785231849589),(64,332.2437580639562),(66,363.39161038245203)]
-}
dr_scale :: [Double]
dr_scale =
  let f0 = Pitch.octpc_to_cps (1 :: Int, 8)
      f = (* f0) . fromRational
  in map f dr_tuning

{- | 12et

>>> putStr (unlines (map (unwords . Et.hs_r_pitch_pp 1)  dr_scale_tbl_12et))
51.9 A♭1 51.9 0.0
69.2 C♯2 69.3 -2.0
75.5 D2 73.4 48.7
83.1 E2 82.4 13.7
92.3 F♯2 92.5 -3.9
103.8 A♭2 103.8 0.0
118.7 B♭2 116.5 31.2
138.4 C♯3 138.6 -2.0
155.7 E♭3 155.6 2.0
166.1 E3 164.8 13.7
184.6 F♯3 185.0 -3.9
207.7 A♭3 207.7 0.0
233.6 B♭3 233.1 3.9
259.6 C4 261.6 -13.7
276.9 C♯4 277.2 -2.0
285.5 D4 293.7 -48.7
311.5 E♭4 311.1 2.0
332.2 E4 329.6 13.7
363.4 F♯4 370.0 -31.2

>>> map (\(f,p,_,_,_) -> (Pitch.pitch_to_midi p,round f)) dr_scale_tbl_12et
[(32,52),(37,69),(38,76),(40,83),(42,92),(44,104),(46,119),(49,138),(51,156),(52,166),(54,185),(56,208),(58,234),(60,260),(61,277),(62,286),(63,311),(64,332),(66,363)]
-}
dr_scale_tbl_12et :: [Et.HS_R Pitch.Pitch]
dr_scale_tbl_12et = map (Et.nearest_12et_tone_k0 (69, 440)) dr_scale

{- | Scala

>>> Scala.scale_verify dr_scale_scala
True

> putStrLn $ unlines $ Scala.scale_pp dr_scale_scala
-}
dr_scale_scala :: Scala.Scale
dr_scale_scala =
  let f r (_, p, _, _, _) = (Pitch.pitch_to_midi p :: Int, r)
      sq = zipWith f dr_tuning dr_scale_tbl_12et
      g z k = case lookup k sq of
        Nothing -> (z, (k, z))
        Just r -> (r, (k, r))
      r_seq = snd (mapAccumL g 1 [33 .. 32 + 12 * 3 - 1]) ++ [(68, 8)]
  in ("dr_itb_etude_1", "...", 3 * 12, map (Right . snd) r_seq)

{- | 24et

> putStrLn (unlines (map (unwords . Pitch.hs_r_pitch_pp 1)  dr_scale_tbl_24et))

@
51.9 A♭1 51.9 0.0
69.2 C♯2 69.3 -2.0
75.5 D𝄲2 75.6 -1.3
83.1 E2 82.4 13.7
92.3 F♯2 92.5 -3.9
103.8 A♭2 103.8 0.0
118.7 B𝄳2 120.0 -18.8
138.4 C♯3 138.6 -2.0
155.7 E♭3 155.6 2.0
166.1 E3 164.8 13.7
184.6 F♯3 185.0 -3.9
207.7 A♭3 207.7 0.0
233.6 B♭3 233.1 3.9
259.6 C4 261.6 -13.7
276.9 C♯4 277.2 -2.0
285.5 D𝄳4 285.3 1.3
311.5 E♭4 311.1 2.0
332.2 E4 329.6 13.7
363.4 F𝄲4 359.5 18.8
@
-}
dr_scale_tbl_24et :: [Et.HS_R Pitch.Pitch]
dr_scale_tbl_24et = map (Et.nearest_24et_tone_k0 (69, 440)) dr_scale

dr_chords :: [[Pitch.Pitch]]
dr_chords =
  [ [Pitch.Name.aes1, Pitch.Name.bes2, Pitch.Name.des3, Pitch.Name.ees4] -- S1
  , [Pitch.Name.aes1, Pitch.Name.aes2, Pitch.Name.fes3, Pitch.Name.ees4]
  , [Pitch.Name.aes1, Pitch.Name.bes2, Pitch.Name.des3, Pitch.Name.ees4]
  , [Pitch.Name.aes1, Pitch.Name.bes2, Pitch.Name.des3, Pitch.Name.ees4] -- S2
  , [Pitch.Name.aes1, Pitch.Name.ges2, Pitch.Name.aes3, Pitch.Name.ees4]
  , [Pitch.Name.aes1, Pitch.Name.bes2, Pitch.Name.des3, Pitch.Name.ees4]
  , [Pitch.Name.aes1, Pitch.Name.bes2, Pitch.Name.des3, Pitch.Name.ees4] -- S3
  , [Pitch.Name.aes1, Pitch.Name.ges2, Pitch.Name.aes3, Pitch.Name.ees4]
  , [Pitch.Name.aes1, Pitch.Name.ges2, Pitch.Name.aes3, Pitch.Name.ees4] -- S4
  , [Pitch.Name.aes1, Pitch.Name.aes2, Pitch.Name.fes3, Pitch.Name.ees4]
  , [Pitch.Name.aes1, Pitch.Name.fes2, Pitch.Name.des4, Pitch.Name.ees4] -- S5
  , [Pitch.Name.ges2, Pitch.Name.aes2, Pitch.Name.aes3, Pitch.Name.d4]
  , [Pitch.Name.aes1, Pitch.Name.d2, Pitch.Name.aes3, Pitch.Name.ees4]
  , [Pitch.Name.aes2, Pitch.Name.fes3, Pitch.Name.d4] -- S6
  , [Pitch.Name.aes1, Pitch.Name.fes2, Pitch.Name.des4, Pitch.Name.ees4]
  , [Pitch.Name.aes1, Pitch.Name.fes2, Pitch.Name.des4, Pitch.Name.ees4] -- S7
  , [Pitch.Name.aes1, Pitch.Name.ges2, Pitch.Name.aes3, Pitch.Name.ees4]
  , [Pitch.Name.aes1, Pitch.Name.ges2, Pitch.Name.aes3, Pitch.Name.ees4] -- S8
  , [Pitch.Name.aes1, Pitch.Name.d2, Pitch.Name.aes3, Pitch.Name.ees4]
  ]

{- | Ratios

>>> sum (map snd (concat dr_ratio_seq)) == 20 * 11
True

>>> map (sum . map snd) dr_ratio_seq == replicate 20 11
True
-}
dr_ratio_seq :: Num n => [[(n, n)]]
dr_ratio_seq =
  [ [(11, 3), (2, 2), (6, 6)]
  , [(7, 2), (7, 7), (6, 2)]
  , [(6, 9), (2, 2)]
  , [(2, 9), (11, 2)]
  , [(10, 5), (10, 3), (10, 3)]
  , [(10, 10), (5, 1)]
  , [(5, 7), (11, 4)]
  , [(11, 3), (8, 8)]
  , [(8, 8), (10, 3)] -- p2
  , [(10, 7), (10, 4)]
  , [(10, 4), (3, 3), (4, 4)]
  , [(4, 3), (9, 7), (5, 1)]
  , [(7, 7), (7, 4)]
  , [(9, 9), (9, 2)]
  , [(9, 7), (7, 4)]
  , [(7, 3), (9, 4), (7, 4)]
  , [(5, 3), (4, 4), (6, 1), (4, 3)]
  , [(4, 4), (7, 7)]
  , [(7, 2), (5, 8), (8, 1)]
  , [(8, 1), (1, 10)]
  ]

{- | Histogram

>>> import Data.Function
>>> import Data.List
>>> reverse (sortBy (compare `on` snd) dr_ratio_seq_hist)
[((10,3),3),((7,7),3),((7,4),3),((4,4),3),((11,3),2),((10,4),2),((9,7),2),((8,8),2),((8,1),2),((7,2),2),((5,1),2),((4,3),2),((2,2),2),((11,4),1),((11,2),1),((10,10),1),((10,7),1),((10,5),1),((9,9),1),((9,4),1),((9,2),1),((7,3),1),((6,9),1),((6,6),1),((6,2),1),((6,1),1),((5,8),1),((5,7),1),((5,3),1),((3,3),1),((2,9),1),((1,10),1)]
-}
dr_ratio_seq_hist :: (Ord n, Num n) => [((n, n), Int)]
dr_ratio_seq_hist = List.histogram (concat dr_ratio_seq)

dr_nt :: Integral i => [([i], [i])]
dr_nt =
  [ ([1, 7, 8, 17], [12, 13, 15, 17])
  , ([1, 6, 10, 17], [6, 10, 9])
  ]

{- | Pitch

>>> map (Function.bimap1 (unwords . map Pitch.pitch_pp) . dr_nt_pitch) dr_nt
[("A\9837\&1 B\119091\&2 C\9839\&3 E\9837\&4","A\9837\&3 B\9837\&3 C\9839\&4 E\9837\&4"),("A\9837\&1 A\9837\&2 E3 E\9837\&4","A\9837\&2 E3 E\9837\&3")]
-}
dr_nt_pitch :: ([Int], [Int]) -> ([Pitch.Pitch], [Pitch.Pitch])
dr_nt_pitch =
  let f k = Tuple.p5_snd (dr_scale_tbl_24et !! (k - 1))
  in Function.bimap1 (map f)

{-

-- from harmonic series
hs :: Num n => [(n,n)]
hs = [(1,1),(9,8),(5,4),(11,8),(3,2),(7,4)]

-- from subharmonic series
shs :: Num n => [(n,n)]
shs = [(8,7),(16,11),(8,5),(16,9)]

-}
