-- | Common music notation pitch values.
module Music.Theory.Pitch where

import Data.Char {- base -}
import Data.Function {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Math as T {- hmt -}
import Music.Theory.Pitch.Note {- hmt -}
import qualified Music.Theory.Pitch.Spelling as T {- hmt -}

-- | Pitch classes are modulo twelve integers.
type PitchClass = Int

-- | Octaves are integers, the octave of middle C is @4@.
type Octave = Int

-- | 'Octave' and 'PitchClass' duple.
type Octave_PitchClass i = (i,i)
type OctPC = (Octave,PitchClass)

-- | Common music notation pitch value.
data Pitch = Pitch {note :: Note_T
                   ,alteration :: Alteration_T
                   ,octave :: Octave}
           deriving (Eq,Show)

instance Ord Pitch where
    compare = pitch_compare

-- | Generalised pitch, given by a generalised alteration.
data Pitch' = Pitch' Note_T Alteration_T' Octave
            deriving (Eq,Show)

-- | Pretty printer for 'Pitch''.
pitch'_pp :: Pitch' -> String
pitch'_pp (Pitch' n (_,a) o) = show n ++ a ++ show o

-- | 'Pitch'' printed without octave.
pitch'_class_pp :: Pitch' -> String
pitch'_class_pp = T.dropWhileRight isDigit . pitch'_pp

-- | Simplify 'Pitch' to standard 12ET by deleting quarter tones.
--
-- > let p = Pitch A QuarterToneSharp 4
-- > in alteration (pitch_clear_quarter_tone p) == Sharp
pitch_clear_quarter_tone :: Pitch -> Pitch
pitch_clear_quarter_tone p =
    let Pitch n a o = p
    in Pitch n (alteration_clear_quarter_tone a) o

-- | 'Pitch' to 'Octave' and 'PitchClass' notation.
--
-- > pitch_to_octpc (Pitch F Sharp 4) == (4,6)
pitch_to_octpc :: Integral i => Pitch -> Octave_PitchClass i
pitch_to_octpc = midi_to_octpc . pitch_to_midi

-- | Is 'Pitch' 12-ET.
pitch_is_12et :: Pitch -> Bool
pitch_is_12et = alteration_is_12et . alteration

-- | 'Pitch' to midi note number notation.
--
-- > pitch_to_midi (Pitch A Natural 4) == 69
pitch_to_midi :: Integral i => Pitch -> i
pitch_to_midi (Pitch n a o) =
    let a' = alteration_to_diff_err a
        n' = note_to_pc n
        o' = fromIntegral o
    in 12 + o' * 12 + n' + a'

-- | 'Pitch' to fractional midi note number notation.
--
-- > pitch_to_fmidi (Pitch A QuarterToneSharp 4) == 69.5
pitch_to_fmidi :: Fractional n => Pitch -> n
pitch_to_fmidi (Pitch n a o) =
    let a' = alteration_to_fdiff a
        o' = fromIntegral o
        n' = fromInteger (note_to_pc n)
    in 12 + o' * 12 + n' + a'

-- | Extract 'PitchClass' of 'Pitch'
--
-- > pitch_to_pc (Pitch A Natural 4) == 9
-- > pitch_to_pc (Pitch F Sharp 4) == 6
pitch_to_pc :: Pitch -> PitchClass
pitch_to_pc (Pitch n a _) = note_to_pc n + alteration_to_diff_err a

-- | 'Pitch' comparison, implemented via 'pitch_to_fmidi'.
--
-- > pitch_compare (Pitch A Natural 4) (Pitch A QuarterToneSharp 4) == LT
pitch_compare :: Pitch -> Pitch -> Ordering
pitch_compare =
    let f = pitch_to_fmidi :: Pitch -> Double
    in compare `on` f

-- | Given 'Spelling' function translate from 'OctPC' notation to
-- 'Pitch'.
octpc_to_pitch :: Integral i => Spelling i -> Octave_PitchClass i -> Pitch
octpc_to_pitch sp (o,pc) =
    let (n,a) = sp pc
    in Pitch n a (fromIntegral o)

-- | Normalise 'OctPC' value, ie. ensure 'PitchClass' is in (0,11).
--
-- > octpc_nrm (4,16) == (5,4)
octpc_nrm :: Integral i => Octave_PitchClass i -> Octave_PitchClass i
octpc_nrm (o,pc) =
    if pc > 11
    then octpc_nrm (o+1,pc-12)
    else if pc < 0
         then octpc_nrm (o-1,pc+12)
         else (o,pc)

-- | Transpose 'OctPC' value.
--
-- > octpc_trs 7 (4,9) == (5,4)
-- > octpc_trs (-11) (4,9) == (3,10)
octpc_trs :: Integral i => i -> Octave_PitchClass i -> Octave_PitchClass i
octpc_trs n (o,pc) =
    let pc' = fromIntegral pc
        k = pc' + n
        (i,j) = k `divMod` 12
    in (fromIntegral o + fromIntegral i,fromIntegral j)

-- | 'OctPC' value to integral /midi/ note number.
--
-- > map octpc_to_midi [(0,0),(4,9),(9,0)] == [12,69,120]
octpc_to_midi :: Integral i => Octave_PitchClass i -> i
octpc_to_midi (o,pc) = 60 + ((fromIntegral o - 4) * 12) + pc

-- | 'fromIntegral' of 'octpc_to_midi'.
octpc_to_fmidi :: (Integral i,Num n) => Octave_PitchClass i -> n
octpc_to_fmidi = fromIntegral . octpc_to_midi

-- | Inverse of 'octpc_to_midi'.
--
-- > midi_to_octpc 69 == (4,9)
midi_to_octpc :: Integral i => i -> Octave_PitchClass i
midi_to_octpc n = (n - 12) `divMod` 12

-- | Enumerate range, inclusive.
--
-- > octpc_range ((3,8),(4,1)) == [(3,8),(3,9),(3,10),(3,11),(4,0),(4,1)]
octpc_range :: (OctPC,OctPC) -> [OctPC]
octpc_range (l,r) =
    let (l',r') = (octpc_to_midi l,octpc_to_midi r)
    in map midi_to_octpc [l' .. r']

-- | Midi note number to 'Pitch'.
--
-- > let r = ["C4","E♭4","F♯4"]
-- > in map (pitch_pp . midi_to_pitch pc_spell_ks) [60,63,66] == r
midi_to_pitch :: Integral i => Spelling i -> i -> Pitch
midi_to_pitch sp = octpc_to_pitch sp . midi_to_octpc

-- | Fractional midi note number to 'Pitch'.
--
-- > fmidi_to_pitch' pc_spell_ks 69.25 == Nothing
fmidi_to_pitch' :: RealFrac n => Spelling Int -> n -> Maybe Pitch
fmidi_to_pitch' sp m =
    let m' = round m
        (Pitch n a o) = midi_to_pitch sp m'
        q = m - fromIntegral m'
    in case alteration_edit_quarter_tone q a of
         Nothing -> Nothing
         Just a' -> Just (Pitch n a' o)

-- | Fractional midi note number to 'Pitch'.
--
-- > import Music.Theory.Pitch.Spelling
-- > pitch_pp (fmidi_to_pitch pc_spell_ks 65.5) == "F𝄲4"
-- > pitch_pp (fmidi_to_pitch pc_spell_ks 66.5) == "F𝄰4"
-- > pitch_pp (fmidi_to_pitch pc_spell_ks 67.5) == "A𝄭4"
-- > pitch_pp (fmidi_to_pitch pc_spell_ks 69.5) == "B𝄭4"
fmidi_to_pitch :: (Show n,RealFrac n) => Spelling Int -> n -> Pitch
fmidi_to_pitch sp m = fromMaybe (error (show ("fmidi_to_pitch",m))) (fmidi_to_pitch' sp m)

-- | Composition of 'pitch_to_fmidi' and then 'fmidi_to_pitch'.
--
-- > import Music.Theory.Pitch.Name as T
-- > import Music.Theory.Pitch.Spelling as T
--
-- > pitch_tranpose T.pc_spell_ks 2 T.ees5 == T.f5
pitch_tranpose :: (RealFrac n,Show n) => Spelling Int -> n -> Pitch -> Pitch
pitch_tranpose sp n p =
    let m = pitch_to_fmidi p
    in fmidi_to_pitch sp (m + n)

-- | Set octave of /p2/ so that it nearest to /p1/.
--
-- > import Music.Theory.Pitch.Name as T
--
-- > let {r = ["B1","C2","C#2"];f = pitch_in_octave_nearest T.cis2}
-- > in map (pitch_pp_iso . f) [T.b4,T.c4,T.cis4] == r
pitch_in_octave_nearest :: Pitch -> Pitch -> Pitch
pitch_in_octave_nearest p1 p2 =
    let o1 = octave p1
        p2' = map (\n -> p2 {octave = n}) [o1 - 1,o1,o1 + 1]
        m1 = pitch_to_fmidi p1 :: Double
        m2 = map (pitch_to_fmidi) p2'
        d = map (abs . (m1 -)) m2
        z = sortBy (compare `on` snd) (zip p2' d)
    in fst (head z)

-- | Raise 'Note_T' of 'Pitch', account for octave transposition.
--
-- > pitch_note_raise (Pitch B Natural 3) == Pitch C Natural 4
pitch_note_raise :: Pitch -> Pitch
pitch_note_raise (Pitch n a o) =
    if n == maxBound
    then Pitch minBound a (o + 1)
    else Pitch (succ n) a o

-- | Lower 'Note_T' of 'Pitch', account for octave transposition.
--
-- > pitch_note_lower (Pitch C Flat 4) == Pitch B Flat 3
pitch_note_lower :: Pitch -> Pitch
pitch_note_lower (Pitch n a o) =
    if n == minBound
    then Pitch maxBound a (o - 1)
    else Pitch (pred n) a o

-- | Rewrite 'Pitch' to not use @3/4@ tone alterations, ie. re-spell
-- to @1/4@ alteration.
--
-- > let {p = Pitch A ThreeQuarterToneFlat 4
-- >     ;q = Pitch G QuarterToneSharp 4}
-- > in pitch_rewrite_threequarter_alteration p == q
pitch_rewrite_threequarter_alteration :: Pitch -> Pitch
pitch_rewrite_threequarter_alteration (Pitch n a o) =
    case a of
      ThreeQuarterToneFlat -> pitch_note_lower (Pitch n QuarterToneSharp o)
      ThreeQuarterToneSharp -> pitch_note_raise (Pitch n QuarterToneFlat o)
      _ -> Pitch n a o

-- | Apply function to 'octave' of 'PitchClass'.
--
-- > pitch_edit_octave (+ 1) (Pitch A Natural 4) == Pitch A Natural 5
pitch_edit_octave :: (Octave -> Octave) -> Pitch -> Pitch
pitch_edit_octave f (Pitch n a o) = Pitch n a (f o)

-- * Frequency (CPS)

-- | /Midi/ note number to cycles per second, given frequency of ISO A4.
midi_to_cps_f0 :: (Integral i,Floating f) => f -> i -> f
midi_to_cps_f0 f0 = fmidi_to_cps_f0 f0 . fromIntegral

-- | 'midi_to_cps_f0' 440.
--
-- > map midi_to_cps [60,69] == [261.6255653005986,440.0]
midi_to_cps :: (Integral i,Floating f) => i -> f
midi_to_cps = midi_to_cps_f0 440

-- | Fractional /midi/ note number to cycles per second, given frequency of ISO A4.
fmidi_to_cps_f0 :: Floating a => a -> a -> a
fmidi_to_cps_f0 f0 i = f0 * (2 ** ((i - 69) * (1 / 12)))

-- | 'fmidi_to_cps_f0' 440.
--
-- > map fmidi_to_cps [69,69.1] == [440.0,442.5488940698553]
fmidi_to_cps :: Floating a => a -> a
fmidi_to_cps = fmidi_to_cps_f0 440

-- | 'fmidi_to_cps' of 'pitch_to_fmidi', given frequency of ISO A4.
pitch_to_cps_f0 :: Floating n => n -> Pitch -> n
pitch_to_cps_f0 f0 = fmidi_to_cps_f0 f0 . pitch_to_fmidi

-- | 'pitch_to_cps_f0' 440.
pitch_to_cps :: Floating n => Pitch -> n
pitch_to_cps = pitch_to_cps_f0 440

-- | Frequency (cycles per second) to /midi/ note number, ie. 'round'
-- of 'cps_to_fmidi'.
--
-- > map cps_to_midi [261.6,440] == [60,69]
cps_to_midi :: (Integral i,Floating f,RealFrac f) => f -> i
cps_to_midi = round . cps_to_fmidi

-- | Frequency (cycles per second) to fractional /midi/ note number.
--
-- > cps_to_fmidi 440 == 69
-- > cps_to_fmidi (fmidi_to_cps 60.25) == 60.25
cps_to_fmidi :: Floating a => a -> a
cps_to_fmidi a = (logBase 2 (a * (1 / 440)) * 12) + 69

-- | 'midi_to_cps_f0' of 'octpc_to_midi', given frequency of ISO A4.
octpc_to_cps_f0 :: (Integral i,Floating n) => n -> Octave_PitchClass i -> n
octpc_to_cps_f0 f0 = midi_to_cps_f0 f0 . octpc_to_midi

-- | 'octpc_to_cps_f0' 440.
--
-- > octpc_to_cps (4,9) == 440
octpc_to_cps :: (Integral i,Floating n) => Octave_PitchClass i -> n
octpc_to_cps = octpc_to_cps_f0 440

-- | 'midi_to_octpc' of 'cps_to_midi'.
cps_to_octpc :: (Floating f,RealFrac f,Integral i) => f -> Octave_PitchClass i
cps_to_octpc = midi_to_octpc . cps_to_midi

-- * MIDI detune

-- | Midi note number with cents detune.
type Midi_Detune = (Int,Double)

-- | Frequency (in hertz) to 'Midi_Detune'.
--
-- > map (fmap round . cps_to_midi_detune) [440.00,508.35] == [(69,0),(71,50)]
cps_to_midi_detune :: Double -> Midi_Detune
cps_to_midi_detune f =
    let (n,c) = T.integral_and_fractional_parts (cps_to_fmidi f)
    in (n,c * 100)

-- | In normal form the detune is in the range (-50,+50] instead of [0,100).
midi_detune_normalise :: Midi_Detune -> Midi_Detune
midi_detune_normalise (m,c) = if c <= 50 then (m,c) else (m + 1, negate (100 - c))

-- | Inverse of 'cps_to_midi_detune', given frequency of ISO A4.
midi_detune_to_cps_f0 :: Double -> Midi_Detune -> Double
midi_detune_to_cps_f0 f0 (m,c) = fmidi_to_cps_f0 f0 (fromIntegral m + (c / 100))

-- | Inverse of 'cps_to_midi_detune'.
--
-- > map midi_detune_to_cps [(69,0),(68,100)] == [440,440]
midi_detune_to_cps :: Midi_Detune -> Double
midi_detune_to_cps = midi_detune_to_cps_f0 440

-- | 'Midi_Detune' to 'Pitch', detune must be precisely at a notateable Pitch.
--
-- > let p = Pitch {note = C, alteration = QuarterToneSharp, octave = 4}
-- > in midi_detune_to_pitch T.pc_spell_ks (midi_detune_nearest_24et (60,35)) == p
midi_detune_to_pitch :: Spelling Int -> Midi_Detune -> Pitch
midi_detune_to_pitch sp = fmidi_to_pitch sp . cps_to_fmidi . midi_detune_to_cps

-- | Round /detune/ value to nearest multiple of @50@.
midi_detune_nearest_24et :: Midi_Detune -> Midi_Detune
midi_detune_nearest_24et (m,dt) = (m,T.round_to 50 dt)

-- * Parsers

-- | Slight generalisation of ISO pitch representation.  Allows octave
-- to be elided, pitch names to be lower case, and double sharps
-- written as @##@.
--
-- See <http://www.musiccog.ohio-state.edu/Humdrum/guide04.html>
--
-- > let r = [Pitch C Natural 4,Pitch A Flat 5,Pitch F DoubleSharp 6]
-- > in mapMaybe (parse_iso_pitch_oct 4) ["C","Ab5","f##6",""] == r
parse_iso_pitch_oct :: Octave -> String -> Maybe Pitch
parse_iso_pitch_oct def_o s =
    let nte n = let tb = zip "cdefgab" [C,D,E,F,G,A,B]
                in lookup (toLower n) tb
        oct o = case o of
                  [] -> Just def_o
                  [n] -> if isDigit n
                         then Just (fromIntegral (digitToInt n))
                         else Nothing
                  _ -> Nothing
        mk n a o = case nte n of
                   Nothing -> Nothing
                   Just n' -> fmap (Pitch n' a) (oct o)
    in case s of
         [] -> Nothing
         n:'b':'b':o -> mk n DoubleFlat o
         n:'#':'#':o -> mk n DoubleSharp o
         n:'x':o -> mk n DoubleSharp o
         n:'b':o -> mk n Flat o
         n:'#':o -> mk n Sharp o
         n:o -> mk n Natural o

-- | Variant of 'parse_iso_pitch_oct' requiring octave.
parse_iso_pitch :: String -> Maybe Pitch
parse_iso_pitch = parse_iso_pitch_oct (error "parse_iso_pitch: no octave")

-- * Pretty printers

-- | Pretty printer for 'Pitch' (unicode, see 'alteration_symbol').
-- Option selects if 'Natural's are printed.
--
-- > pitch_pp' True (Pitch E Natural 4) == "E♮4"
pitch_pp' :: Bool -> Pitch -> String
pitch_pp' show_nat (Pitch n a o) =
    let a' = if a == Natural && not show_nat then "" else [alteration_symbol a]
    in show n ++ a' ++ show o

-- | 'pitch_pp'' 'False'.
--
-- > pitch_pp (Pitch E Flat 4) == "E♭4"
-- > pitch_pp (Pitch F QuarterToneSharp 3) == "F𝄲3"
pitch_pp :: Pitch -> String
pitch_pp = pitch_pp' False

-- | 'Pitch' printed without octave.
pitch_class_pp' :: Bool -> Pitch -> String
pitch_class_pp' opt =
    let f c = isDigit c || c == '-' -- negative octave values...
    in T.dropWhileRight f . pitch_pp' opt

-- | 'pitch_class_pp'' 'False'.
--
-- > pitch_class_pp (Pitch C ThreeQuarterToneSharp 0) == "C𝄰"
pitch_class_pp :: Pitch -> String
pitch_class_pp = pitch_class_pp' False

-- | Sequential list of /n/ pitch class names starting from /k/.
--
-- > unwords (pitch_class_names_12et 0 12) == "C C♯ D E♭ E F F♯ G A♭ A B♭ B"
-- > pitch_class_names_12et 11 2 == ["B","C"]
pitch_class_names_12et :: Integral n => n -> n -> [String]
pitch_class_names_12et k n =
    let f = pitch_class_pp . midi_to_pitch T.pc_spell_ks
    in map f [60 + k .. 60 + k + n - 1]

-- | Pretty printer for 'Pitch' (ISO, ASCII, see 'alteration_iso').
--
-- > pitch_pp_iso (Pitch E Flat 4) == "Eb4"
-- > pitch_pp_iso (Pitch F DoubleSharp 3) == "Fx3"
pitch_pp_iso :: Pitch -> String
pitch_pp_iso (Pitch n a o) = show n ++ alteration_iso a ++ show o

-- | Pretty printer for 'Pitch' (ASCII, see 'alteration_tonh').
--
-- > pitch_pp_hly (Pitch E Flat 4) == "ees4"
-- > pitch_pp_hly (Pitch F QuarterToneSharp 3) == "fih3"
-- > pitch_pp_hly (Pitch B Natural 6) == "b6"
pitch_pp_hly :: Pitch -> String
pitch_pp_hly (Pitch n a o) =
    let n' = map toLower (show n)
    in n' ++ alteration_tonh a ++ show o

-- | Pretty printer for 'Pitch' (Tonhöhe, see 'alteration_tonh').
--
-- > pitch_pp_tonh (Pitch E Flat 4) == "Es4"
-- > pitch_pp_tonh (Pitch F QuarterToneSharp 3) == "Fih3"
-- > pitch_pp_tonh (Pitch B Natural 6) == "H6"
pitch_pp_tonh :: Pitch -> String
pitch_pp_tonh (Pitch n a o) =
    let o' = show o
    in case (n,a) of
         (B,Natural) -> "H" ++ o'
         (B,Flat) -> "B" ++ o'
         (B,DoubleFlat) -> "Heses" ++ o'
         (A,Flat) -> "As" ++ o'
         (E,Flat) -> "Es" ++ o'
         _ -> show n ++ alteration_tonh a ++ o'

-- * 24ET

{-  The 24ET pitch-class universe, with /sharp/ spellings, in octave '4'.

> length pc24et_univ == 24

> let r = "C C𝄲 C♯ C𝄰 D D𝄲 D♯ D𝄰 E E𝄲 F F𝄲 F♯ F𝄰 G G𝄲 G♯ G𝄰 A A𝄲 A♯ A𝄰 B B𝄲"
> in unwords (map pitch_class_pp pc24et_univ) == r

-}
pc24et_univ :: [Pitch]
pc24et_univ =
    let a = [Natural,QuarterToneSharp,Sharp,ThreeQuarterToneSharp]
        f (n,k) = map (\i -> Pitch n (a !! i) 4) [0 .. k - 1]
    in concatMap f (zip [C,D,E,F,G,A,B] [4,4,2,4,4,4,2])

-- | 'genericIndex' into 'pc24et_univ'.
--
-- > pitch_class_pp (pc24et_to_pitch 13) == "F𝄰"
pc24et_to_pitch :: Integral i => i -> Pitch
pc24et_to_pitch = genericIndex pc24et_univ
