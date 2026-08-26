-- | Equal temperament tuning tables.
module Music.Theory.Tuning.Et where

import qualified Data.List {- base -}
import qualified Data.Ratio {- base -}
import qualified Text.Printf {- base -}

import qualified Data.List.Split {- split -}

import qualified Music.Theory.List as List {- hmt-base -}

import qualified Music.Theory.Pitch as Pitch {- hmt -}
import qualified Music.Theory.Pitch.Note as Note {- hmt -}
import qualified Music.Theory.Tuning as Tuning {- hmt -}

-- | 'octpc_to_pitch' and 'octpc_to_cps_k0'.
octpc_to_pitch_cps_k0 :: (Floating n) => (n, n) -> Pitch.OctPc -> (Pitch.Pitch, n)
octpc_to_pitch_cps_k0 zero x =
  ( Pitch.octpc_to_pitch Pitch.pc_spell_ks x
  , Pitch.octpc_to_cps_k0 zero x
  )

-- | 'octpc_to_pitch_cps_k0' of (69,440)
octpc_to_pitch_cps :: (Floating n) => Pitch.OctPc -> (Pitch.Pitch, n)
octpc_to_pitch_cps = octpc_to_pitch_cps_k0 (69, 440)

{- | 12-tone equal temperament table equating 'Pitch' and frequency
over range of human hearing, where @A4@ has given frequency.

> map (\(p, d) -> (pitch_pp_iso p,d)) (tbl_12et_k0 (69,440))
-}
tbl_12et_k0 :: (Double, Double) -> [(Pitch.Pitch, Double)]
tbl_12et_k0 zero =
  let z = [(o, pc) | o <- [-5 .. 10], pc <- [0 .. 11]]
  in map (octpc_to_pitch_cps_k0 zero) z

{- | 'tbl_12et_k0' @(69,440)@.

>>> length tbl_12et
192

>>> List.minmax (map (round . snd) tbl_12et)
(1,31609)
-}
tbl_12et :: [(Pitch.Pitch, Double)]
tbl_12et = tbl_12et_k0 (69, 440)

-- | 24-tone equal temperament variant of 'tbl_12et_k0'.
tbl_24et_k0 :: (Double, Double) -> [(Pitch.Pitch, Double)]
tbl_24et_k0 zero =
  let f x =
        let p = Pitch.fmidi_to_pitch_err Pitch.pc_spell_ks x
            p' = Pitch.pitch_rewrite_threequarter_alteration p
        in (p', Tuning.fmidi_to_cps_k0 zero x)
      k0 = -36
  in map f [k0, k0 + 0.5 .. 143.5]

{- | 'tbl_24et_k0' @(69,440)@.

>>> length tbl_24et
360

>>> List.minmax (map (round . snd) tbl_24et)
(1,32535)
-}
tbl_24et :: [(Pitch.Pitch, Double)]
tbl_24et = tbl_24et_k0 (69, 440)

{- | Given an @Et@ table (or like) find bounds of frequency.

>>> import qualified Music.Theory.Tuple as Tuple
>>> let r = Just (Tuple.t2_map octpc_to_pitch_cps ((3,11),(4,0)))
>>> bounds_et_table tbl_12et 256 == r
True
-}
bounds_et_table :: Ord s => [(t, s)] -> s -> Maybe ((t, s), (t, s))
bounds_et_table = List.find_bounds True (compare . snd)

{- | 'bounds_et_table' of 'tbl_12et'.

> import qualified Music.Theory.Tuning.Hs as Hs
> map bounds_12et_tone (Hs.harmonic_series_cps_n 17 55)
-}
bounds_12et_tone :: Double -> Maybe ((Pitch.Pitch, Double), (Pitch.Pitch, Double))
bounds_12et_tone = bounds_et_table tbl_12et

{- | Tuple indicating nearest 'Pitch' to /frequency/ with @Et@
frequency, and deviation in hertz and 'Cents'.

(cps,nearest-pitch,cps-of-nearest-pitch,cps-deviation,cents-deviation)
-}
type Hs_R p = (Double, p, Double, Double, Tuning.Cents)

{- | /n/-decimal places.

>>> ndp 3 (1/3)
"0.333"
-}
ndp :: Int -> Double -> String
ndp = Text.Printf.printf "%.*f"

-- | Pretty print 'Hs_R'.  This discards the /cps-deviation/ field, ie. it has only four fields.
hs_r_pp :: (p -> String) -> Int -> Hs_R p -> [String]
hs_r_pp pp n (f, p, pf, _, c) = let dp = ndp n in [dp f, pp p, dp pf, dp c]

-- | 'hs_r_pp' of 'pitch_pp'
hs_r_pitch_pp :: Int -> Hs_R Pitch.Pitch -> [String]
hs_r_pitch_pp = hs_r_pp Pitch.pitch_pp

{- | Form 'Hs_R' for /frequency/ by consulting table.

>>> let f = 256
>>> let f' = Pitch.octpc_to_cps (4,0)
>>> let r = (f,Pitch.Pitch Note.C Note.Natural 4,f',f - f',Tuning.fratio_to_cents (f / f'))
>>> nearest_et_table_tone tbl_12et 256 == r
True
-}
nearest_et_table_tone :: [(p, Double)] -> Double -> Hs_R p
nearest_et_table_tone tbl f =
  case bounds_et_table tbl f of
    Nothing -> error "nearest_et_table_tone: no bounds?"
    Just ((lp, lf), (rp, rf)) ->
      let ld = f - lf
          rd = f - rf
      in if abs ld < abs rd
          then (f, lp, lf, ld, Tuning.fratio_to_cents (f / lf))
          else (f, rp, rf, rd, Tuning.fratio_to_cents (f / rf))

-- | 'nearest_et_table_tone' for 'tbl_12et_k0'.
nearest_12et_tone_k0 :: (Double, Double) -> Double -> Hs_R Pitch.Pitch
nearest_12et_tone_k0 zero = nearest_et_table_tone (tbl_12et_k0 zero)

{- | 'nearest_et_table_tone' for 'tbl_24et'.

>>> unwords (hs_r_pitch_pp 1 (nearest_24et_tone_k0 (69,440) 55))
"55.0 A1 55.0 0.0"
-}
nearest_24et_tone_k0 :: (Double, Double) -> Double -> Hs_R Pitch.Pitch
nearest_24et_tone_k0 zero = nearest_et_table_tone (tbl_24et_k0 zero)

-- * 72Et

{- | Monzo 72-edo HEWM notation.  The domain is (-9,9).
<http://www.tonalsoft.com/enc/number/72edo.aspx>

>>> map alteration_72et_monzo [1 .. 9]
["+",">","^","#<","#-","#","#+","#>","#^"]

>>> map alteration_72et_monzo [-1,-2 .. -9]
["-","<","v","b>","b+","b","b-","b<","bv"]
-}
alteration_72et_monzo :: Integral n => n -> String
alteration_72et_monzo n =
  let spl = Data.List.Split.splitOn ","
      asc = spl ",+,>,^,#<,#-,#,#+,#>,#^"
      dsc = spl ",-,<,v,b>,b+,b,b-,b<,bv"
  in case compare n 0 of
      LT -> Data.List.genericIndex dsc (-n)
      EQ -> ""
      GT -> Data.List.genericIndex asc n

{- | Given a midi note number and @1/6@ deviation determine 'Pitch''
and frequency.

>>> let f = Pitch.pitch_r_pp . fst . pitch_72et_k0 (69,440)
>>> unwords (map f (zip (repeat 60) [0..9]))
"C4 C+4 C>4 C^4 C#<4 C#-4 C#4 C#+4 C#>4 C#^4"

>>> unwords (map f (zip (repeat 69) [0..9]))
"A4 A+4 A>4 A^4 Bb<4 Bb-4 Bb4 Bb+4 Bb>4 Bv4"

>>> unwords (map f (zip (repeat 70) [0..9]))
"Bb4 Bb+4 Bb>4 Bv4 B<4 B-4 B4 B+4 B>4 B^4"
-}
pitch_72et_k0 :: (Double, Double) -> (Pitch.Midi, Int) -> (Pitch.Pitch_R, Double)
pitch_72et_k0 zero (x, n) =
  let p = Pitch.midi_to_pitch_ks x
      t = Pitch.note p
      a = Pitch.alteration p
      (t', n') = case a of
        Note.Flat -> if n < (-3) then (pred t, n + 6) else (t, n - 6)
        Note.Natural -> (t, n)
        Note.Sharp -> if n > 3 then (succ t, n - 6) else (t, n + 6)
        _ -> error "pitch_72et: alteration?"
      a' = alteration_72et_monzo n'
      x' = fromIntegral x + (fromIntegral n / 6)
      r =
        ( Pitch.Pitch_R t' (fromIntegral n' Data.Ratio.% 12, a') (Pitch.octave p)
        , Tuning.fmidi_to_cps_k0 zero x'
        )
      r' =
        if n > 3
          then pitch_72et_k0 zero (x + 1, n - 6)
          else
            if n < (-3)
              then pitch_72et_k0 zero (x - 1, n + 6)
              else r
  in case a of
      Note.Natural -> r'
      _ -> r

{- | 72-tone equal temperament table equating 'Pitch'' and frequency
over range of human hearing, where @A4@ = @440@hz.

>>> length (tbl_72et_k0 (69,440))
792

>>> List.minmax (map (round . snd) (tbl_72et_k0 (69,440)))
(16,33167)
-}
tbl_72et_k0 :: (Double, Double) -> [(Pitch.Pitch_R, Double)]
tbl_72et_k0 zero =
  let f n = zipWith (curry (pitch_72et_k0 zero)) (replicate 6 n) [0 .. 5]
  in concatMap f [12 .. 143]

{- | 'nearest_et_table_tone' for 'tbl_72et'.

>>> unwords (hs_r_pp Pitch.pitch_r_pp 1 (nearest_72et_tone_k0 (69,440) 324))
"324.0 E<4 323.3 3.5"

> let f = take 2 . hs_r_pp Pitch.pitch_r_pp 1 . nearest_72et_tone_k0 (69,440) . snd
> mapM_ (print . unwords . f) (tbl_72et_k0 (69,440))
-}
nearest_72et_tone_k0 :: (Double, Double) -> Double -> Hs_R Pitch.Pitch_R
nearest_72et_tone_k0 zero = nearest_et_table_tone (tbl_72et_k0 zero)

-- * Detune

-- | 'Pitch' with 12-Et/24-Et tuning deviation given in 'Cents'.
type Pitch_Detune = (Pitch.Pitch, Tuning.Cents)

-- | Extract 'Pitch_Detune' from 'Hs_R'.
hsr_to_pitch_detune :: Hs_R Pitch.Pitch -> Pitch_Detune
hsr_to_pitch_detune (_, p, _, _, c) = (p, c)

{- | Nearest 12-Et 'Pitch_Detune' to indicated frequency (hz).

>>> nearest_pitch_detune_12et_k0 (69,440) 452.8929841231365
(Pitch {note = A, alteration = Natural, octave = 4},50.00000000000007)
-}
nearest_pitch_detune_12et_k0 :: (Double, Double) -> Double -> Pitch_Detune
nearest_pitch_detune_12et_k0 zero = hsr_to_pitch_detune . nearest_12et_tone_k0 zero

{- | Nearest 24-Et 'Pitch_Detune' to indicated frequency (hz).

>>> nearest_pitch_detune_24et_k0 (69,440) 452.8929841231365
(Pitch {note = A, alteration = QuarterToneSharp, octave = 4},0.0)
-}
nearest_pitch_detune_24et_k0 :: (Double, Double) -> Double -> Pitch_Detune
nearest_pitch_detune_24et_k0 zero = hsr_to_pitch_detune . nearest_24et_tone_k0 zero

-- | Given /near/ function, /f0/ and ratio derive 'Pitch_Detune'.
ratio_to_pitch_detune :: (Double -> Hs_R Pitch.Pitch) -> Pitch.OctPc -> Rational -> Pitch_Detune
ratio_to_pitch_detune near_f f0 r =
  let f = Pitch.octpc_to_cps f0 * realToFrac r
      (_, p, _, _, c) = near_f f
  in (p, c)

{- | Frequency (hz) of 'Pitch_Detune'.

>>> pitch_detune_to_cps (Pitch.octpc_to_pitch Pitch.pc_spell_ks (4,9),50)
452.8929841231365
-}
pitch_detune_to_cps :: Floating n => Pitch_Detune -> n
pitch_detune_to_cps (p, d) = Tuning.cps_shift_cents (Pitch.pitch_to_cps p) (realToFrac d)

-- | 'ratio_to_pitch_detune' of 'nearest_12et_tone'
ratio_to_pitch_detune_12et_k0 :: (Double, Double) -> Pitch.OctPc -> Rational -> Pitch_Detune
ratio_to_pitch_detune_12et_k0 zero = ratio_to_pitch_detune (nearest_12et_tone_k0 zero)

-- | 'ratio_to_pitch_detune' of 'nearest_24et_tone'
ratio_to_pitch_detune_24et_k0 :: (Double, Double) -> Pitch.OctPc -> Rational -> Pitch_Detune
ratio_to_pitch_detune_24et_k0 zero = ratio_to_pitch_detune (nearest_24et_tone_k0 zero)

pitch_detune_in_octave_nearest :: Pitch.Pitch -> Pitch_Detune -> Pitch_Detune
pitch_detune_in_octave_nearest p1 (p2, d2) = (Pitch.pitch_in_octave_nearest p1 p2, d2)

{- | Markdown pretty-printer for 'Pitch_Detune'.

>>> pitch_detune_md (nearest_pitch_detune_12et_k0 (69,440) 452.8929841231365)
"A4^+50^"

>>> let cps = [174,285,396,417,528,639,741,852,963]
>>> let f = pitch_detune_md . nearest_pitch_detune_12et_k0 (69,440)
>>> putStr (unlines (map f cps))
F3^-6^
C♯4^+48^
G4^+18^
A♭4^+7^
C5^+16^
E♭5^+46^
F♯5^+2^
A♭5^+44^
B5^-44^
-}
pitch_detune_md :: Pitch_Detune -> String
pitch_detune_md (p, c) =
  Pitch.pitch_pp p
    ++ Tuning.cents_diff_md (round c :: Integer)

{- | HTML pretty-printer for 'Pitch_Detune'.

>>> pitch_detune_html (nearest_pitch_detune_12et_k0 (69,440) 452.8929841231365)
"A4<SUP>+50</SUP>"
-}
pitch_detune_html :: Pitch_Detune -> String
pitch_detune_html (p, c) =
  Pitch.pitch_pp p
    ++ Tuning.cents_diff_html (round c :: Integer)

{- | No-octave variant of 'pitch_detune_md'.

>>> pitch_class_detune_md (nearest_pitch_detune_12et_k0 (69,440) 452.8929841231365)
"A^+50^"
-}
pitch_class_detune_md :: Pitch_Detune -> String
pitch_class_detune_md (p, c) =
  Pitch.pitch_class_pp p
    ++ Tuning.cents_diff_md (round c :: Integer)

{- | No-octave variant of 'pitch_detune_html'.

>>> pitch_class_detune_html (nearest_pitch_detune_12et_k0 (69,440) 452.8929841231365)
"A<SUP>+50</SUP>"
-}
pitch_class_detune_html :: Pitch_Detune -> String
pitch_class_detune_html (p, c) =
  Pitch.pitch_class_pp p
    ++ Tuning.cents_diff_html (round c :: Integer)
