-- | David Rosenboom, "In the Beginning: Etude I (Trombones)", 1979
--   <http://davidrosenboom.com/media/beginning-etude-i-trombones>
--
-- kw: subharmonics, difference tones
module Music.Theory.Tuning.Rosenboom_1979 where

import Data.List {- base -}
import Data.Ratio {- base -}

import qualified Music.Theory.List as T
import qualified Music.Theory.Pitch as T
import qualified Music.Theory.Pitch.Name as T
import qualified Music.Theory.Tuning.ET as T
import qualified Music.Theory.Tuning.Scala as Scala
import qualified Music.Theory.Tuple as T

t2_to_ratio :: (Integer,Integer) -> Rational
t2_to_ratio (n,d) = n % d

-- | Tuning, ratios for each octave.
--
-- > length (concat dr_tuning_oct) == 19
-- > import qualified Music.Theory.Tuning as T
-- > map (map (T.ratio_to_cents . t2_to_ratio)) dr_tuning_oct
dr_tuning_oct :: Num n => [[(n,n)]]
dr_tuning_oct =
    [[(1,1),(4,3),(16,11),(8,5),(16,9)]
    ,[(1,1),(8,7),(4,3),(3,2),(8,5),(16,9)]
    ,[(1,1),(9,8),(5,4),(4,3),(11,8),(3,2),(8,5),(7,4)]]

-- | Tuning, actual ratios.
dr_tuning :: [Rational]
dr_tuning = concat (zipWith (\o -> map ((* o) . t2_to_ratio)) [1,2,4] dr_tuning_oct)

-- | Actual scale, in CPS.
--
-- > let r = [52,69,76,83,92,104,119,138,156,166,185,208,234,260,277,286,311,332,363]
-- > in map round dr_scale == r
dr_scale :: [Double]
dr_scale =
    let f0 = T.octpc_to_cps (1::Int,8)
        f = (* f0) . fromRational
    in map f dr_tuning

-- > putStrLn (unlines (map (unwords . T.hs_r_pitch_pp 1)  dr_scale_tbl_12et))
-- > map (\(f,p,_,_,_) -> (T.pitch_to_midi p,f)) dr_scale_tbl_12et
dr_scale_tbl_12et :: [T.HS_R T.Pitch]
dr_scale_tbl_12et = map T.nearest_12et_tone dr_scale

{-

51.9 A♭1 51.9 0.0 0.0
69.2 C♯2 69.3 -0.1 -2.0
75.5 D2 73.4 2.1 48.7
83.1 E2 82.4 0.7 13.7
92.3 F♯2 92.5 -0.2 -3.9
103.8 A♭2 103.8 0.0 0.0
118.7 B♭2 116.5 2.1 31.2
138.4 C♯3 138.6 -0.2 -2.0
155.7 E♭3 155.6 0.2 2.0
166.1 E3 164.8 1.3 13.7
184.6 F♯3 185.0 -0.4 -3.9
207.7 A♭3 207.7 0.0 0.0
233.6 B♭3 233.1 0.5 3.9
259.6 C4 261.6 -2.1 -13.7
276.9 C♯4 277.2 -0.3 -2.0
285.5 D4 293.7 -8.1 -48.7
311.5 E♭4 311.1 0.4 2.0
332.2 E4 329.6 2.6 13.7
363.4 F♯4 370.0 -6.6 -31.2

-}

-- > Scala.scale_verify dr_scale_scala
-- > putStrLn $ unlines $ Scala.scale_pp dr_scale_scala
dr_scale_scala :: Scala.Scale
dr_scale_scala =
    let f (r,(_,p,_,_,_)) = (T.pitch_to_midi p :: Int,r)
        sq = map f (zip dr_tuning dr_scale_tbl_12et)
        g z k = case lookup k sq of
                  Nothing -> (z,(k,z))
                  Just r -> (r,(k,r))
        r_seq = snd (mapAccumL g 1 [33 .. 32 + 12 * 3 - 1]) ++ [(68,8)]
    in ("dr_itb_etude_1","...",3 * 12,map (Right . snd) r_seq)

-- > putStrLn (unlines (map (unwords . T.hs_r_pitch_pp 1)  dr_scale_tbl_24et))
dr_scale_tbl_24et :: [T.HS_R T.Pitch]
dr_scale_tbl_24et = map T.nearest_24et_tone dr_scale

{-

51.9 A♭1 51.9 0.0 0.0
69.2 C♯2 69.3 -0.1 -2.0
75.5 D𝄲2 75.6 -0.1 -1.3
83.1 E2 82.4 0.7 13.7
92.3 F♯2 92.5 -0.2 -3.9
103.8 A♭2 103.8 0.0 0.0
118.7 B𝄳2 120.0 -1.3 -18.8
138.4 C♯3 138.6 -0.2 -2.0
155.7 E♭3 155.6 0.2 2.0
166.1 E3 164.8 1.3 13.7
184.6 F♯3 185.0 -0.4 -3.9
207.7 A♭3 207.7 0.0 0.0
233.6 B♭3 233.1 0.5 3.9
259.6 C4 261.6 -2.1 -13.7
276.9 C♯4 277.2 -0.3 -2.0
285.5 D𝄳4 285.3 0.2 1.3
311.5 E♭4 311.1 0.4 2.0
332.2 E4 329.6 2.6 13.7
363.4 F𝄲4 359.5 3.9 18.8

-}

dr_chords :: [[T.Pitch]]
dr_chords =
    [[T.aes1,T.bes2,T.des3,T.ees4] -- S1
    ,[T.aes1,T.aes2,T.fes3,T.ees4]
    ,[T.aes1,T.bes2,T.des3,T.ees4]
    ,[T.aes1,T.bes2,T.des3,T.ees4] -- S2
    ,[T.aes1,T.ges2,T.aes3,T.ees4]
    ,[T.aes1,T.bes2,T.des3,T.ees4]
    ,[T.aes1,T.bes2,T.des3,T.ees4] -- S3
    ,[T.aes1,T.ges2,T.aes3,T.ees4]
    ,[T.aes1,T.ges2,T.aes3,T.ees4] -- S4
    ,[T.aes1,T.aes2,T.fes3,T.ees4]
    ,[T.aes1,T.fes2,T.des4,T.ees4] -- S5
    ,[T.ges2,T.aes2,T.aes3,T.d4]
    ,[T.aes1,T.d2,T.aes3,T.ees4]
    ,[T.aes2,T.fes3,T.d4] -- S6
    ,[T.aes1,T.fes2,T.des4,T.ees4]
    ,[T.aes1,T.fes2,T.des4,T.ees4] -- S7
    ,[T.aes1,T.ges2,T.aes3,T.ees4]
    ,[T.aes1,T.ges2,T.aes3,T.ees4] -- S8
    ,[T.aes1,T.d2,T.aes3,T.ees4]
    ]

-- > sum (map snd (concat dr_ratio_seq)) == 20 * 11
-- > map (sum . map snd) dr_ratio_seq == replicate 20 11
dr_ratio_seq :: Num n => [[(n,n)]]
dr_ratio_seq =
    [[(11,3),(2,2),(6,6)]
    ,[(7,2),(7,7),(6,2)]
    ,[(6,9),(2,2)]
    ,[(2,9),(11,2)]
    ,[(10,5),(10,3),(10,3)]
    ,[(10,10),(5,1)]
    ,[(5,7),(11,4)]
    ,[(11,3),(8,8)]
    ,[(8,8),(10,3)] -- p2
    ,[(10,7),(10,4)]
    ,[(10,4),(3,3),(4,4)]
    ,[(4,3),(9,7),(5,1)]
    ,[(7,7),(7,4)]
    ,[(9,9),(9,2)]
    ,[(9,7),(7,4)]
    ,[(7,3),(9,4),(7,4)]
    ,[(5,3),(4,4),(6,1),(4,3)]
    ,[(4,4),(7,7)]
    ,[(7,2),(5,8),(8,1)]
    ,[(8,1),(1,10)]
    ]

-- > import Data.Function
-- > import Data.List
-- > reverse (sortBy (compare `on` snd) dr_ratio_seq_hist)
dr_ratio_seq_hist :: (Ord n,Num n) => [((n,n),Int)]
dr_ratio_seq_hist = T.histogram (concat dr_ratio_seq)

dr_nt :: Integral i => [([i],[i])]
dr_nt =
    [([1,7,8,17],[12,13,15,17])
    ,([1,6,10,17],[6,10,9])]

-- > map (T.bimap1 (map T.pitch_pp) . dr_nt_pitch) dr_nt
dr_nt_pitch :: ([Int], [Int]) -> ([T.Pitch], [T.Pitch])
dr_nt_pitch =
    let f k = T.p5_snd (dr_scale_tbl_24et !! (k - 1))
    in T.bimap1 (map f)

{-

-- from harmonic series
hs :: Num n => [(n,n)]
hs = [(1,1),(9,8),(5,4),(11,8),(3,2),(7,4)]

-- from subharmonic series
shs :: Num n => [(n,n)]
shs = [(8,7),(16,11),(8,5),(16,9)]

-}
