-- | Common music notation pitch values.
module Music.Theory.Pitch where

import Data.Char {- base -}
import Data.Function {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Text.Printf {- base -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Math as T {- hmt -}
import qualified Music.Theory.Pitch.Note as T {- hmt -}

-- * Octave & pitch-class (generic)

-- | 'Octave' and 'PitchClass' duple.
type Octave_PitchClass i = (i,i)

-- | Normalise 'Octave_PitchClass' value, ie. ensure pitch-class is in (0,11).
octave_pitchclass_nrm :: Integral i => Octave_PitchClass i -> Octave_PitchClass i
octave_pitchclass_nrm (o,pc) =
    if pc > 11
    then octave_pitchclass_nrm (o+1,pc-12)
    else if pc < 0
         then octave_pitchclass_nrm (o-1,pc+12)
         else (o,pc)

-- | Transpose 'Octave_PitchClass' value.
octave_pitchclass_trs :: Integral i => i -> Octave_PitchClass i -> Octave_PitchClass i
octave_pitchclass_trs n (o,pc) =
    let pc' = fromIntegral pc
        k = pc' + n
        (i,j) = k `divMod` 12
    in (fromIntegral o + fromIntegral i,fromIntegral j)

-- | 'Octave_PitchClass' value to integral /midi/ note number.
octave_pitchclass_to_midi :: Integral i => Octave_PitchClass i -> i
octave_pitchclass_to_midi (o,pc) = 60 + ((o - 4) * 12) + pc

-- | Inverse of 'octave_pitchclass_to_midi'.
--
  -- > map midi_to_octave_pitchclass [60,84,91] == [(4,0),(6,0),(6,7)]
midi_to_octave_pitchclass :: Integral i => i -> Octave_PitchClass i
midi_to_octave_pitchclass n = (n - 12) `divMod` 12

-- * Octave & PitchClass

-- | Pitch classes are modulo twelve integers.
type PitchClass = Int

-- | Octaves are integers, the octave of middle C is @4@.
type Octave = Int

-- | 'Octave' and 'PitchClass' duple.
type OctPC = (Octave,PitchClass)

-- | Translate from generic octave & pitch-class duple.
to_octpc :: (Integral pc, Integral oct) => (oct,pc) -> OctPC
to_octpc (oct,pc) = (fromIntegral oct,fromIntegral pc)

-- | Normalise 'OctPC'.
--
-- > octpc_nrm (4,16) == (5,4)
octpc_nrm :: OctPC -> OctPC
octpc_nrm = octave_pitchclass_nrm

-- | Transpose 'OctPC'.
--
-- > octpc_trs 7 (4,9) == (5,4)
-- > octpc_trs (-11) (4,9) == (3,10)
octpc_trs :: Int -> OctPC -> OctPC
octpc_trs = octave_pitchclass_trs

-- | Enumerate range, inclusive.
--
-- > octpc_range ((3,8),(4,1)) == [(3,8),(3,9),(3,10),(3,11),(4,0),(4,1)]
octpc_range :: (OctPC,OctPC) -> [OctPC]
octpc_range (l,r) =
    let (l',r') = (octpc_to_midi l,octpc_to_midi r)
    in map midi_to_octpc [l' .. r']

-- * Midi note number

-- | Midi note number
type Midi = Int

-- | 'OctPC' value to integral /midi/ note number.
--
-- > map octpc_to_midi [(0,0),(2,6),(4,9),(9,0)] == [12,42,69,120]
-- > map octpc_to_midi [(0,9),(8,0)] == [21,108]
octpc_to_midi :: OctPC -> Midi
octpc_to_midi = octave_pitchclass_to_midi

-- | Inverse of 'octpc_to_midi'.
--
-- > map midi_to_octpc [40,69] == [(2,4),(4,9)]
midi_to_octpc :: Midi -> OctPC
midi_to_octpc = midi_to_octave_pitchclass

-- * Octave & fractional pitch-class

-- | Fractional midi note number.
type FMidi = Double

-- | Fractional octave pitch-class (octave is integral, pitch-class is fractional).
type FOctPC = (Int,Double)

-- | 'fromIntegral' of 'octpc_to_midi'.
octpc_to_fmidi :: (Integral i,Num n) => Octave_PitchClass i -> n
octpc_to_fmidi = fromIntegral . octave_pitchclass_to_midi

-- | Fractional midi to fractional octave pitch-class.
--
-- > fmidi_to_foctpc 69.5 == (4,9.5)
fmidi_to_foctpc :: RealFrac f => f -> (Octave,f)
fmidi_to_foctpc n = let o = (floor n - 12) `div` 12 in (o,n - (fromIntegral (o + 1) * 12))

-- | Octave of fractional midi note number.
fmidi_octave :: RealFrac f => f -> Octave
fmidi_octave = fst . fmidi_to_foctpc

foctpc_to_fmidi :: RealFrac f => (Octave,f) -> f
foctpc_to_fmidi (o,pc) = (fromIntegral (o + 1) * 12) + pc

-- | Move fractional midi note number to indicated octave.
--
-- > map (fmidi_in_octave 1) [59.5,60.5] == [35.5,24.5]
fmidi_in_octave :: RealFrac f => Octave -> f -> f
fmidi_in_octave o m = let (_,pc) = fmidi_to_foctpc m in foctpc_to_fmidi (o,pc)

-- * Pitch

-- | Common music notation pitch value.
data Pitch = Pitch {note :: T.Note_T
                   ,alteration :: T.Alteration_T
                   ,octave :: Octave}
           deriving (Eq,Show)

instance Ord Pitch where
    compare = pitch_compare

-- | Simplify 'Pitch' to standard 12ET by deleting quarter tones.
--
-- > let p = Pitch A QuarterToneSharp 4
-- > in alteration (pitch_clear_quarter_tone p) == Sharp
pitch_clear_quarter_tone :: Pitch -> Pitch
pitch_clear_quarter_tone p =
    let Pitch n a o = p
    in Pitch n (T.alteration_clear_quarter_tone a) o

-- | 'Pitch' to 'Octave' and 'PitchClass' notation.
--
-- > pitch_to_octpc (Pitch F Sharp 4) == (4,6)
pitch_to_octpc :: Integral i => Pitch -> Octave_PitchClass i
pitch_to_octpc = midi_to_octave_pitchclass . pitch_to_midi

-- | Is 'Pitch' 12-ET.
pitch_is_12et :: Pitch -> Bool
pitch_is_12et = T.alteration_is_12et . alteration

-- | 'Pitch' to midi note number notation.
--
-- > pitch_to_midi (Pitch A Natural 4) == 69
pitch_to_midi :: Integral i => Pitch -> i
pitch_to_midi (Pitch n a o) =
    let a' = T.alteration_to_diff_err a
        n' = T.note_to_pc n
        o' = fromIntegral o
    in 12 + o' * 12 + n' + a'

-- | 'Pitch' to fractional midi note number notation.
--
-- > pitch_to_fmidi (Pitch A QuarterToneSharp 4) == 69.5
pitch_to_fmidi :: Fractional n => Pitch -> n
pitch_to_fmidi (Pitch n a o) =
    let a' = T.alteration_to_fdiff a
        o' = fromIntegral o
        n' = fromInteger (T.note_to_pc n)
    in 12 + o' * 12 + n' + a'

-- | Extract 'PitchClass' of 'Pitch'
--
-- > map pitch_to_pc [Pitch A Natural 4,Pitch F Sharp 4] == [9,6]
-- > map pitch_to_pc [Pitch C Flat 4,Pitch B Sharp 5] == [11,0]
pitch_to_pc :: Pitch -> PitchClass
pitch_to_pc (Pitch n a _) = (T.note_to_pc n + T.alteration_to_diff_err a) `mod` 12

-- | 'Pitch' comparison, implemented via 'pitch_to_fmidi'.
--
-- > pitch_compare (Pitch A Natural 4) (Pitch A QuarterToneSharp 4) == LT
pitch_compare :: Pitch -> Pitch -> Ordering
pitch_compare =
    let f = pitch_to_fmidi :: Pitch -> Double
    in compare `on` f

-- * Spelling

-- | Function to spell a 'PitchClass'.
type Spelling n = n -> (T.Note_T,T.Alteration_T)

-- | Variant of 'Spelling' for incomplete functions.
type Spelling_M i = i -> Maybe (T.Note_T,T.Alteration_T)

-- | Given 'Spelling' function translate from 'OctPC' notation to 'Pitch'.
--
-- > octpc_to_pitch T.pc_spell_sharp (4,6) == Pitch T.F T.Sharp 4
octpc_to_pitch :: Integral i => Spelling i -> Octave_PitchClass i -> Pitch
octpc_to_pitch sp (o,pc) =
    let (n,a) = sp pc
    in Pitch n a (fromIntegral o)

-- | Midi note number to 'Pitch'.
--
-- > let r = ["C4","E♭4","F♯4"]
-- > in map (pitch_pp . midi_to_pitch pc_spell_ks) [60,63,66] == r
midi_to_pitch :: Integral i => Spelling i -> i -> Pitch
midi_to_pitch sp = octpc_to_pitch sp . midi_to_octave_pitchclass

-- | Print fractional midi note number as ET12 pitch with cents detune in parentheses.
--
-- > fmidi_et12_cents_pp 66.5 == "F♯4(+50)"
fmidi_et12_cents_pp :: Spelling PitchClass -> Double -> String
fmidi_et12_cents_pp sp =
    let f (m,c) =
            let d = T.num_diff_str (round c :: Int)
                d' = if null d then "" else "(" ++ d ++ ")"
            in pitch_pp (midi_to_pitch sp m) ++ d'
    in f . midi_detune_normalise . fmidi_to_midi_detune

-- | Fractional midi note number to 'Pitch'.
--
-- > fmidi_to_pitch pc_spell_ks 69.25 == Nothing
fmidi_to_pitch :: RealFrac n => Spelling PitchClass -> n -> Maybe Pitch
fmidi_to_pitch sp m =
    let m' = round m
        (Pitch n a o) = midi_to_pitch sp m'
        q = m - fromIntegral m'
    in case T.alteration_edit_quarter_tone q a of
         Nothing -> Nothing
         Just a' -> Just (Pitch n a' o)

-- | Erroring variant.
--
-- > import Music.Theory.Pitch.Spelling
-- > pitch_pp (fmidi_to_pitch_err pc_spell_ks 65.5) == "F𝄲4"
-- > pitch_pp (fmidi_to_pitch_err pc_spell_ks 66.5) == "F𝄰4"
-- > pitch_pp (fmidi_to_pitch_err pc_spell_ks 67.5) == "A𝄭4"
-- > pitch_pp (fmidi_to_pitch_err pc_spell_ks 69.5) == "B𝄭4"
fmidi_to_pitch_err :: (Show n,RealFrac n) => Spelling Int -> n -> Pitch
fmidi_to_pitch_err sp m = fromMaybe (error (show ("fmidi_to_pitch",m))) (fmidi_to_pitch sp m)

-- | Composition of 'pitch_to_fmidi' and then 'fmidi_to_pitch'.
--
-- > import Music.Theory.Pitch.Name as T
-- > import Music.Theory.Pitch.Spelling as T
--
-- > pitch_tranpose T.pc_spell_ks 2 T.ees5 == T.f5
pitch_tranpose :: (RealFrac n,Show n) => Spelling Int -> n -> Pitch -> Pitch
pitch_tranpose sp n p =
    let m = pitch_to_fmidi p
    in fmidi_to_pitch_err sp (m + n)

-- | Displacement of /q/ into octave of /p/.
fmidi_in_octave_of :: RealFrac f => f -> f -> f
fmidi_in_octave_of p = fmidi_in_octave (fmidi_octave p)

-- | Octave displacement of /m2/ that is nearest /m1/.
--
-- > let {p = octpc_to_fmidi (2,1);q = map octpc_to_fmidi [(4,11),(4,0),(4,1)]}
-- > in map (fmidi_in_octave_nearest p) q == [35,36,37]
fmidi_in_octave_nearest :: RealFrac n => n -> n -> n
fmidi_in_octave_nearest m1 m2 =
    let m2' = fmidi_in_octave (fmidi_octave m1) m2
        m2'' = [m2' - 12,m2',m2' + 12]
        d = map (abs . (m1 -)) m2''
        z = sortOn snd (zip m2'' d)
    in fst (head z)

-- | Displacement of /q/ into octave above /p/.
--
-- > fmidi_in_octave_of 69 51 == 63
-- > fmidi_in_octave_nearest 69 51 == 63
-- > fmidi_in_octave_above 69 51 == 75
fmidi_in_octave_above :: RealFrac a => a -> a -> a
fmidi_in_octave_above p q = let r = fmidi_in_octave_nearest p q in if r < p then r + 12 else r

-- | Displacement of /q/ into octave below /p/.
--
-- > fmidi_in_octave_of 69 85 == 61
-- > fmidi_in_octave_nearest 69 85 == 73
-- > fmidi_in_octave_below 69 85 == 61
fmidi_in_octave_below :: RealFrac a => a -> a -> a
fmidi_in_octave_below p q = let r = fmidi_in_octave_nearest p q in if r > p then r - 12 else r

cps_in_octave' :: Floating f => (f -> f -> f) -> f -> f -> f
cps_in_octave' f p = fmidi_to_cps . f (cps_to_fmidi p) . cps_to_fmidi

-- | CPS form of 'fmidi_in_octave_nearest'.
--
-- > map cps_octave [440,256] == [4,4]
-- > round (cps_in_octave_nearest 440 256) == 512
cps_in_octave_nearest :: (Floating f,RealFrac f) => f -> f -> f
cps_in_octave_nearest = cps_in_octave' fmidi_in_octave_nearest

-- | Raise or lower the frequency /q/ by octaves until it is in the
-- octave starting at /p/.
--
-- > cps_in_octave_above 55.0 392.0 == 98.0
cps_in_octave_above :: (Ord a, Fractional a) => a -> a -> a
cps_in_octave_above p =
    let go q = if q > p * 2 then go (q / 2) else if q < p then go (q * 2) else q
    in go

-- > cps_in_octave_above' 55.0 392.0 == 97.99999999999999
cps_in_octave_above' :: (Floating f,RealFrac f) => f -> f -> f
cps_in_octave_above' = cps_in_octave' fmidi_in_octave_above

cps_in_octave_below :: (Floating f,RealFrac f) => f -> f -> f
cps_in_octave_below = cps_in_octave' fmidi_in_octave_below

-- | Set octave of /p2/ so that it nearest to /p1/.
--
-- > import Music.Theory.Pitch.Name as T
--
-- > let {r = ["B1","C2","C#2"];f = pitch_in_octave_nearest T.cis2}
-- > in map (pitch_pp_iso . f) [T.b4,T.c4,T.cis4] == r
pitch_in_octave_nearest :: Pitch -> Pitch -> Pitch
pitch_in_octave_nearest p1 p2 =
    let f = pitch_to_fmidi :: Pitch -> Double
        o = fmidi_octave (fmidi_in_octave_nearest (f p1) (f p2))
    in p2 {octave = o}

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
      T.ThreeQuarterToneFlat -> pitch_note_lower (Pitch n T.QuarterToneSharp o)
      T.ThreeQuarterToneSharp -> pitch_note_raise (Pitch n T.QuarterToneFlat o)
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

-- | Frequency (cps = cycles per second) to fractional /midi/ note
-- number, given frequency of ISO A4 (mnn = 69).
cps_to_fmidi_f0 :: Floating a => a -> a -> a
cps_to_fmidi_f0 f0 a = (logBase 2 (a * (1 / f0)) * 12) + 69

-- | 'cps_to_fmidi_f0' @440@.
--
-- > cps_to_fmidi 440 == 69
-- > cps_to_fmidi (fmidi_to_cps 60.25) == 60.25
cps_to_fmidi :: Floating a => a -> a
cps_to_fmidi = cps_to_fmidi_f0 440

-- | Frequency (cycles per second) to /midi/ note number, ie. 'round'
-- of 'cps_to_fmidi'.
--
-- > map cps_to_midi [261.6,440] == [60,69]
cps_to_midi :: (Integral i,Floating f,RealFrac f) => f -> i
cps_to_midi = round . cps_to_fmidi

-- | 'midi_to_cps_f0' of 'octpc_to_midi', given frequency of ISO A4.
octpc_to_cps_f0 :: (Integral i,Floating n) => n -> Octave_PitchClass i -> n
octpc_to_cps_f0 f0 = midi_to_cps_f0 f0 . octave_pitchclass_to_midi

-- | 'octpc_to_cps_f0' 440.
--
-- > octpc_to_cps (4,9) == 440
octpc_to_cps :: (Integral i,Floating n) => Octave_PitchClass i -> n
octpc_to_cps = octpc_to_cps_f0 440

-- | 'midi_to_octpc' of 'cps_to_midi'.
cps_to_octpc :: (Floating f,RealFrac f,Integral i) => f -> Octave_PitchClass i
cps_to_octpc = midi_to_octave_pitchclass . cps_to_midi

cps_octave :: (Floating f,RealFrac f) => f -> Octave
cps_octave = fst . cps_to_octpc

-- * MIDI detune (cents)

-- | Midi note number with cents detune.
type Midi_Detune' c = (Int,c)

-- | Is cents in (-50,+50].
--
-- > map cents_is_normal [-250,-75,75,250] == replicate 4 False
cents_is_normal :: (Num c, Ord c) => c -> Bool
cents_is_normal c = c > (-50) && c <= 50

-- | 'cents_is_normal' of 'snd'.
midi_detune_is_normal :: (Num c, Ord c) => Midi_Detune' c -> Bool
midi_detune_is_normal = cents_is_normal . snd

-- | In normal form the detune is in the range (-50,+50] instead of [0,100) or wider.
--
-- > map midi_detune_normalise [(60,-250),(60,-75),(60,75),(60,250)]
midi_detune_normalise :: (Ord c,Num c) => Midi_Detune' c -> Midi_Detune' c
midi_detune_normalise (m,c) =
    if c > 50
    then midi_detune_normalise (m + 1,c - 100)
    else if c > (-50)
         then (m,c)
         else midi_detune_normalise (m - 1,c + 100)

-- | Inverse of 'cps_to_midi_detune', given frequency of ISO @A4@.
midi_detune_to_cps_f0 :: Real c => Double -> Midi_Detune' c -> Double
midi_detune_to_cps_f0 f0 (m,c) = fmidi_to_cps_f0 f0 (fromIntegral m + (realToFrac c / 100))

-- | Inverse of 'cps_to_midi_detune'.
--
-- > map midi_detune_to_cps [(69,0),(68,100)] == [440,440]
midi_detune_to_cps :: Real c => Midi_Detune' c -> Double
midi_detune_to_cps = midi_detune_to_cps_f0 440

-- | 'Midi_Detune' to fractional midi note number.
--
-- > midi_detune_to_fmidi (60,50.0) == 60.50
midi_detune_to_fmidi :: Real c => Midi_Detune' c -> Double
midi_detune_to_fmidi (mnn,c) = fromIntegral mnn + (realToFrac c / 100)

-- | 'Midi_Detune' to 'Pitch', detune must be precisely at a notateable Pitch.
--
-- > let p = Pitch {note = C, alteration = QuarterToneSharp, octave = 4}
-- > in midi_detune_to_pitch T.pc_spell_ks (midi_detune_nearest_24et (60,35)) == p
midi_detune_to_pitch :: Real c => Spelling Int -> Midi_Detune' c -> Pitch
midi_detune_to_pitch sp = fmidi_to_pitch_err sp . cps_to_fmidi . midi_detune_to_cps

-- | Midi note number with real-valued cents detune.
type Midi_Detune = Midi_Detune' Double

-- | Fractional midi note number to 'Midi_Detune'.
--
-- > fmidi_to_midi_detune 60.50 == (60,50.0)
fmidi_to_midi_detune :: Double -> Midi_Detune
fmidi_to_midi_detune mnn =
    let (n,c) = T.integral_and_fractional_parts mnn
    in (n,c * 100)

-- | Frequency (in hertz) to 'Midi_Detune'.
--
-- > map (fmap round . cps_to_midi_detune) [440.00,508.35] == [(69,0),(71,50)]
cps_to_midi_detune :: Double -> Midi_Detune
cps_to_midi_detune = fmidi_to_midi_detune . cps_to_fmidi

-- | Round /detune/ value to nearest multiple of @50@, normalised.
--
-- > map midi_detune_nearest_24et [(59,70),(59,80)] == [(59,50),(60,00)]
midi_detune_nearest_24et :: Midi_Detune -> Midi_Detune
midi_detune_nearest_24et (m,dt) = midi_detune_normalise (m,T.round_to 50 dt)

-- * MIDI cents

-- | Midi note number with integral cents detune.
type Midi_Cents = Midi_Detune' Int

midi_detune_to_midi_cents :: Midi_Detune -> Midi_Cents
midi_detune_to_midi_cents (m,c) = (m,round c)

-- | Printed as /fmidi/ value with cents to two places.  Must be normal.
--
-- > map midi_cents_pp [(60,0),(60,25)] == ["60.00","60.25"]
midi_cents_pp :: Midi_Cents -> String
midi_cents_pp (m,c) = if cents_is_normal c then printf "%d.%02d" m c else error "midi_cents_pp"

-- * Parsers

-- | Parse possible octave from single integer.
--
-- > map (parse_octave 2) ["","4","x","11"]
parse_octave :: Num a => a -> String -> Maybe a
parse_octave def_o o =
    case o of
      [] -> Just def_o
      [n] -> if isDigit n
             then Just (fromIntegral (digitToInt n))
             else Nothing
      _ -> Nothing

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
    let mk n a o = case T.parse_note_t True n of
                   Nothing -> Nothing
                   Just n' -> fmap (Pitch n' a) (parse_octave def_o o)
    in case s of
         [] -> Nothing
         n:'b':'b':o -> mk n T.DoubleFlat o
         n:'#':'#':o -> mk n T.DoubleSharp o
         n:'x':o -> mk n T.DoubleSharp o
         n:'b':o -> mk n T.Flat o
         n:'#':o -> mk n T.Sharp o
         n:o -> mk n T.Natural o

-- | Variant of 'parse_iso_pitch_oct' requiring octave.
parse_iso_pitch :: String -> Maybe Pitch
parse_iso_pitch = parse_iso_pitch_oct (error "parse_iso_pitch: no octave")

-- | 'error' variant.
parse_iso_pitch_err :: String -> Pitch
parse_iso_pitch_err = fromMaybe (error "parse_iso_pitch") . parse_iso_pitch

-- * Pretty printers

-- | Pretty printer for 'Pitch' (unicode, see 'alteration_symbol').
-- Option selects if 'Natural's are printed.
--
-- > pitch_pp_opt (True,True) (Pitch T.E T.Natural 4) == "E♮4"
pitch_pp_opt :: (Bool,Bool) -> Pitch -> String
pitch_pp_opt (show_nat,show_oct) (Pitch n a o) =
    let a' = if a == T.Natural && not show_nat then "" else [T.alteration_symbol a]
        rem_oct_f c = isDigit c || c == '-' -- negative octave values...
        rem_oct = if show_oct then id else T.dropWhileRight rem_oct_f
    in rem_oct (show n ++ a' ++ show o)

-- | 'pitch_pp_opt' with default options, ie. (False,True).
--
-- > pitch_pp (Pitch T.E T.Natural 4) == "E4"
-- > pitch_pp (Pitch T.E T.Flat 4) == "E♭4"
-- > pitch_pp (Pitch T.F T.QuarterToneSharp 3) == "F𝄲3"
pitch_pp :: Pitch -> String
pitch_pp = pitch_pp_opt (False,True)

-- | 'pitch_pp_opt' with options (False,False).
--
-- > pitch_class_pp (Pitch T.C T.ThreeQuarterToneSharp 0) == "C𝄰"
pitch_class_pp :: Pitch -> String
pitch_class_pp = pitch_pp_opt (False,False)

-- | Sequential list of /n/ pitch class names starting from /k/.
--
-- > import Music.Theory.Pitch.Spelling.Table
-- > unwords (pitch_class_names_12et pc_spell_ks 0 12) == "C C♯ D E♭ E F F♯ G A♭ A B♭ B"
-- > pitch_class_names_12et pc_spell_ks 11 2 == ["B","C"]
pitch_class_names_12et :: Integral n => Spelling n -> n -> n -> [String]
pitch_class_names_12et sp k n =
    let f = pitch_class_pp . midi_to_pitch sp
    in map f [60 + k .. 60 + k + n - 1]

-- | Pretty printer for 'Pitch' (ISO, ASCII, see 'alteration_iso').
--
-- > pitch_pp_iso (Pitch E Flat 4) == "Eb4"
-- > pitch_pp_iso (Pitch F DoubleSharp 3) == "Fx3"
-- > pitch_pp_iso (Pitch C ThreeQuarterToneSharp 4) -- error
pitch_pp_iso :: Pitch -> String
pitch_pp_iso (Pitch n a o) = show n ++ T.alteration_iso a ++ show o

-- | Pretty printer for 'Pitch' (ASCII, see 'alteration_tonh').
--
-- > pitch_pp_hly (Pitch E Flat 4) == "ees4"
-- > pitch_pp_hly (Pitch F QuarterToneSharp 3) == "fih3"
-- > pitch_pp_hly (Pitch B Natural 6) == "b6"
pitch_pp_hly :: Pitch -> String
pitch_pp_hly (Pitch n a o) =
    let n' = map toLower (show n)
    in n' ++ T.alteration_tonh a ++ show o

-- | Pretty printer for 'Pitch' (Tonhöhe, see 'alteration_tonh').
--
-- > pitch_pp_tonh (Pitch E Flat 4) == "Es4"
-- > pitch_pp_tonh (Pitch F QuarterToneSharp 3) == "Fih3"
-- > pitch_pp_tonh (Pitch B Natural 6) == "H6"
pitch_pp_tonh :: Pitch -> String
pitch_pp_tonh (Pitch n a o) =
    let o' = show o
    in case (n,a) of
         (T.B,T.Natural) -> "H" ++ o'
         (T.B,T.Flat) -> "B" ++ o'
         (T.B,T.DoubleFlat) -> "Heses" ++ o'
         (T.A,T.Flat) -> "As" ++ o'
         (T.E,T.Flat) -> "Es" ++ o'
         _ -> show n ++ T.alteration_tonh a ++ o'

-- * 24ET

{-  The 24ET pitch-class universe, with /sharp/ spellings, in octave '4'.

> length pc24et_univ == 24

> let r = "C C𝄲 C♯ C𝄰 D D𝄲 D♯ D𝄰 E E𝄲 F F𝄲 F♯ F𝄰 G G𝄲 G♯ G𝄰 A A𝄲 A♯ A𝄰 B B𝄲"
> in unwords (map pitch_class_pp pc24et_univ) == r

-}
pc24et_univ :: [Pitch]
pc24et_univ =
    let a = [T.Natural,T.QuarterToneSharp,T.Sharp,T.ThreeQuarterToneSharp]
        f (n,k) = map (\i -> Pitch n (a !! i) 4) [0 .. k - 1]
    in concatMap f (zip T.note_seq [4,4,2,4,4,4,2])

-- | 'genericIndex' into 'pc24et_univ'.
--
-- > pitch_class_pp (pc24et_to_pitch 13) == "F𝄰"
pc24et_to_pitch :: Integral i => i -> Pitch
pc24et_to_pitch = genericIndex pc24et_univ

-- * Pitch, rational alteration.

-- | Generalised pitch, given by a generalised alteration.
data Pitch_R = Pitch_R T.Note_T T.Alteration_R Octave
               deriving (Eq,Show)

-- | Pretty printer for 'Pitch_R'.
pitch_r_pp :: Pitch_R -> String
pitch_r_pp (Pitch_R n (_,a) o) = show n ++ a ++ show o

-- | 'Pitch_R' printed without octave.
pitch_r_class_pp :: Pitch_R -> String
pitch_r_class_pp = T.dropWhileRight isDigit . pitch_r_pp

