-- | Common music notation pitch values.
module Music.Theory.Pitch where

import Data.Char {- base -}
import Data.Function {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Text.Printf {- base -}

import qualified Text.Parsec as Parsec {- parsec -}

import qualified Music.Theory.List as List {- hmt-base -}
import qualified Music.Theory.Math as Math {- hmt-base -}
import qualified Music.Theory.Math.Convert as Math.Convert {- hmt-base -}
import qualified Music.Theory.Parse as Parse {- hmt-base -}
import qualified Music.Theory.Show as Show {- hmt-base -}

import qualified Music.Theory.Pitch.Note as Note {- hmt -}
import qualified Music.Theory.Pitch.Spelling.Table as Pitch.Spelling.Table {- hmt -}
import qualified Music.Theory.Tuning as Tuning {- hmt -}

-- * Octave pitch-class (generic)

-- | 'Octave' and 'PitchClass' duple.
type Octave_PitchClass i = (i, i)

-- | Normalise 'Octave_PitchClass' value, ie. ensure pitch-class is in (0,11).
octave_pitchclass_nrm :: (Ord i, Num i) => Octave_PitchClass i -> Octave_PitchClass i
octave_pitchclass_nrm (o, pc) =
  if pc > 11
    then octave_pitchclass_nrm (o + 1, pc - 12)
    else
      if pc < 0
        then octave_pitchclass_nrm (o - 1, pc + 12)
        else (o, pc)

-- | Transpose 'Octave_PitchClass' value.
octave_pitchclass_trs :: Integral i => i -> Octave_PitchClass i -> Octave_PitchClass i
octave_pitchclass_trs n (o, pc) =
  let k = pc + n
      (i, j) = k `divMod` 12
  in (o + i, j)

{- | 'Octave_PitchClass' value to integral /midi/ note number.

>>> map octave_pitchclass_to_midi [(-1,9),(8,0)] == map (+ 9) [0,99]
True
-}
octave_pitchclass_to_midi :: Num i => Octave_PitchClass i -> i
octave_pitchclass_to_midi (o, pc) = 60 + ((o - 4) * 12) + pc

{- | Inverse of 'octave_pitchclass_to_midi'.

>>> map midi_to_octave_pitchclass [0,36,60,84,91]
[(-1,0),(2,0),(4,0),(6,0),(6,7)]
-}
midi_to_octave_pitchclass :: (Integral m, Integral i) => m -> Octave_PitchClass i
midi_to_octave_pitchclass n = (fromIntegral n - 12) `divMod` 12

{- | One-indexed piano key number (for standard 88 key piano) to pitch class.
     This has the mnemonic that 49 maps to (4,9).

>>> map pianokey_to_octave_pitchclass [1,49,88]
[(0,9),(4,9),(8,0)]
-}
pianokey_to_octave_pitchclass :: (Integral m, Integral i) => m -> Octave_PitchClass i
pianokey_to_octave_pitchclass = midi_to_octave_pitchclass . (+) 20

-- * Octave & PitchClass

-- | Pitch classes are modulo twelve integers (0-11)
type PitchClass = Int

-- | Octaves are integers, the octave of middle C is @4@.
type Octave = Int

-- | 'Octave' and 'PitchClass' duple.
type OctPc = (Octave, PitchClass)

-- | Translate from generic octave & pitch-class duple.
octave_pitchclass_to_octpc :: (Integral pc, Integral oct) => (oct, pc) -> OctPc
octave_pitchclass_to_octpc (oct, pc) = (fromIntegral oct, fromIntegral pc)

{- | Normalise 'OctPc'.

>>> octpc_nrm (4,16)
(5,4)
-}
octpc_nrm :: OctPc -> OctPc
octpc_nrm = octave_pitchclass_nrm

{- | Transpose 'OctPc'.

>>> octpc_trs 7 (4,9)
(5,4)

>>> octpc_trs (-11) (4,9)
(3,10)
-}
octpc_trs :: Int -> OctPc -> OctPc
octpc_trs = octave_pitchclass_trs

{- | Enumerate range, inclusive.

>>> octpc_range ((3,8),(4,1))
[(3,8),(3,9),(3,10),(3,11),(4,0),(4,1)]
-}
octpc_range :: (OctPc, OctPc) -> [OctPc]
octpc_range (l, r) =
  let (l', r') = (octpc_to_midi l, octpc_to_midi r)
  in map midi_to_octpc [l' .. r']

-- * Midi note number (0 - 127)

{- | Midi note number (0 - 127).
     Midi data values are unsigned 7-bit integers, however using an unsigned type would be problematic.
     It would make transposition, for instance, awkward.
     x - 12 would transpose down an octave, but the transposition interval itself could not be negative.
-}
type Midi = Int

-- | Type conversion
midi_to_int :: Midi -> Int
midi_to_int = id

-- | Type-specialise /f/, ie. round, ceiling, truncate
double_to_midi :: (Double -> Midi) -> Double -> Midi
double_to_midi = Math.Convert.double_to_int

{- | 'OctPc' value to integral /midi/ note number.

>>> map octpc_to_midi [(0,0),(2,6),(4,9),(6,2),(9,0)]
[12,42,69,86,120]

>>> map octpc_to_midi [(0,9),(8,0)]
[21,108]
-}
octpc_to_midi :: OctPc -> Midi
octpc_to_midi = octave_pitchclass_to_midi

{- | Inverse of 'octpc_to_midi'.

>>> map midi_to_octpc [40,69]
[(2,4),(4,9)]
-}
midi_to_octpc :: Midi -> OctPc
midi_to_octpc = midi_to_octave_pitchclass

-- * Octave & fractional pitch-class

{- | (octave,pitch-class) to fractional octave.
This is an odd notation, but can be useful for writing pitch data where a float is required.
Note this is not a linear octave, for that see 'Sound.Sc3.Common.Math.oct_to_cps'.

>>> map octpc_to_foct [(4,0),(4,7),(5,11)]
[4.0,4.07,5.11]
-}
octpc_to_foct :: (Integral i, Fractional r) => (i, i) -> r
octpc_to_foct (o, pc) = fromIntegral o + (fromIntegral pc / 100)

{- | Inverse of 'octpc_to_foct'.

>>> map foct_to_octpc [3.11,4.00,4.07,5.11]
[(3,11),(4,0),(4,7),(5,11)]
-}
foct_to_octpc :: (Integral i, RealFrac r) => r -> (i, i)
foct_to_octpc x =
  let (p, q) = Math.integral_and_fractional_parts x
  in (p, round (q * 100))

-- | 'octpc_to_midi' of 'foct_to_octpc'.
foct_to_midi :: (Integral i, RealFrac r) => r -> i
foct_to_midi = octave_pitchclass_to_midi . foct_to_octpc

-- * FMidi

-- | Fractional midi note number.
type FMidi = Double

-- | Fractional octave pitch-class (octave is integral, pitch-class is fractional).
type FOctPc = (Int, Double)

-- | 'fromIntegral' of 'octpc_to_midi'.
octpc_to_fmidi :: (Integral i, Num n) => Octave_PitchClass i -> n
octpc_to_fmidi = fromIntegral . octave_pitchclass_to_midi

{- | Fractional midi to fractional octave pitch-class.

>>> fmidi_to_foctpc 69.5
(4,9.5)
-}
fmidi_to_foctpc :: RealFrac f => f -> (Octave, f)
fmidi_to_foctpc n = let o = (floor n - 12) `div` 12 in (o, n - (fromIntegral (o + 1) * 12))

-- | Octave of fractional midi note number.
fmidi_octave :: RealFrac f => f -> Octave
fmidi_octave = fst . fmidi_to_foctpc

foctpc_to_fmidi :: RealFrac f => (Octave, f) -> f
foctpc_to_fmidi (o, pc) = (fromIntegral (o + 1) * 12) + pc

{- | Move fractional midi note number to indicated octave.

>>> map (fmidi_in_octave 1) [59.5,60.5]
[35.5,24.5]
-}
fmidi_in_octave :: RealFrac f => Octave -> f -> f
fmidi_in_octave o m = let (_, pc) = fmidi_to_foctpc m in foctpc_to_fmidi (o, pc)

{- | Print fractional midi note number as ET12 pitch with cents detune in parentheses.

>>> fmidi_et12_cents_pp pc_spell_ks 66.5 == "F♯4(+50)"
True
-}
fmidi_et12_cents_pp :: Spelling PitchClass -> FMidi -> String
fmidi_et12_cents_pp sp =
  let f (m, c) =
        let d = Show.num_diff_str (round c :: Int)
            d' = if null d then "" else "(" ++ d ++ ")"
        in pitch_pp (midi_to_pitch sp m) ++ d'
  in f . midi_detune_normalise . fmidi_to_midi_detune

-- * Pitch

-- | Common music notation pitch value.
data Pitch = Pitch
  { note :: Note.Note
  , alteration :: Note.Alteration
  , octave :: Octave
  }
  deriving (Eq, Show)

instance Ord Pitch where
  compare = pitch_compare

{- | Simplify 'Pitch' to standard 12ET by deleting quarter tones.

>>> alteration (pitch_clear_quarter_tone (Pitch Note.A Note.QuarterToneSharp 4))
Sharp
-}
pitch_clear_quarter_tone :: Pitch -> Pitch
pitch_clear_quarter_tone p =
  let Pitch n a o = p
  in Pitch n (Note.alteration_clear_quarter_tone a) o

{- | 'Pitch' to 'Octave' and 'PitchClass' notation.

>>> pitch_to_octpc (Pitch Note.F Note.Sharp 4)
(4,6)
-}
pitch_to_octpc :: Integral i => Pitch -> Octave_PitchClass i
pitch_to_octpc = midi_to_octave_pitchclass . Math.int_id . pitch_to_midi

-- | Is 'Pitch' 12-Et.
pitch_is_12et :: Pitch -> Bool
pitch_is_12et = Note.alteration_is_12et . alteration

{- | 'Pitch' to midi note number notation.

>>> pitch_to_midi (Pitch Note.A Note.Natural 4)
69
-}
pitch_to_midi :: Integral i => Pitch -> i
pitch_to_midi (Pitch n a o) =
  let a' = Note.alteration_to_diff_err a
      n' = Note.note_to_pc n
      o' = fromIntegral o
  in 12 + o' * 12 + n' + a'

{- | 'Pitch' to fractional midi note number notation.

>>> pitch_to_fmidi (Pitch Note.A Note.QuarterToneSharp 4)
69.5
-}
pitch_to_fmidi :: Fractional n => Pitch -> n
pitch_to_fmidi (Pitch n a o) =
  let a' = Note.alteration_to_fdiff a
      o' = fromIntegral o
      n' = fromInteger (Note.note_to_pc n)
  in 12 + o' * 12 + n' + a'

{- | Extract 'PitchClass' of 'Pitch'

>>> map pitch_to_pc [Pitch Note.A Note.Natural 4,Pitch Note.F Note.Sharp 4]
[9,6]

>>> map pitch_to_pc [Pitch Note.C Note.Flat 4,Pitch Note.B Note.Sharp 5]
[11,0]
-}
pitch_to_pc :: Pitch -> PitchClass
pitch_to_pc (Pitch n a _) = (Note.note_to_pc n + Note.alteration_to_diff_err a) `mod` 12

{- | 'Pitch' comparison, implemented via 'pitch_to_fmidi'.

>>> pitch_compare (Pitch Note.A Note.Natural 4) (Pitch Note.A Note.QuarterToneSharp 4)
LT
-}
pitch_compare :: Pitch -> Pitch -> Ordering
pitch_compare =
  let f = pitch_to_fmidi :: Pitch -> Double
  in compare `on` f

-- * Spelling

-- | Function to spell a 'PitchClass'.
type Spelling n = n -> (Note.Note, Note.Alteration)

-- | Variant of 'Spelling' for incomplete functions.
type Spelling_M i = i -> Maybe (Note.Note, Note.Alteration)

{- | Lookup 'pc_spell_ks_tbl'.

>>> map pc_spell_ks [6,8]
[(F,Sharp),(A,Flat)]
-}
pc_spell_ks :: Integral i => Spelling i
pc_spell_ks = pc_spell_tbl_ks []

pc_spell_tbl :: Integral i => Pitch.Spelling.Table.Spelling_Table i -> Spelling i
pc_spell_tbl tbl = fromMaybe (error "pc_spell_tbl") . flip lookup tbl

-- | Spell using indicated table prepended to and 'pc_spell_natural_tbl' and 'pc_spell_ks_tbl'
pc_spell_tbl_ks :: Integral i => Pitch.Spelling.Table.Spelling_Table i -> Spelling i
pc_spell_tbl_ks tbl = pc_spell_tbl (tbl ++ Pitch.Spelling.Table.pc_spell_natural_tbl ++ Pitch.Spelling.Table.pc_spell_ks_tbl)

{- | Spelling for natural (♮) notes only.

>>> map pc_spell_natural_m [0,1]
[Just (C,Natural),Nothing]
-}
pc_spell_natural_m :: Integral i => Spelling_M i
pc_spell_natural_m = flip lookup Pitch.Spelling.Table.pc_spell_natural_tbl

{- | Erroring variant of 'pc_spell_natural_m'.

>>> map pc_spell_natural [0,5,7]
[(C,Natural),(F,Natural),(G,Natural)]
-}
pc_spell_natural :: Integral i => Spelling i
pc_spell_natural = pc_spell_tbl Pitch.Spelling.Table.pc_spell_natural_tbl

{- | Use always sharp (♯) spelling.

>>> map pc_spell_sharp [6,8]
[(F,Sharp),(G,Sharp)]

>>> Data.List.nub (map (snd . pc_spell_sharp) [1,3,6,8,10])
[Sharp]
-}
pc_spell_sharp :: Integral i => Spelling i
pc_spell_sharp = pc_spell_tbl (Pitch.Spelling.Table.pc_spell_sharp_tbl ++ Pitch.Spelling.Table.pc_spell_natural_tbl)

{- | Use always flat (♭) spelling.

>>>  map pc_spell_flat [6,8]
[(G,Flat),(A,Flat)]

>>>  Data.List.nub (map (snd . pc_spell_flat) [1,3,6,8,10])
[Flat]
-}
pc_spell_flat :: Integral i => Spelling i
pc_spell_flat = pc_spell_tbl (Pitch.Spelling.Table.pc_spell_flat_tbl ++ Pitch.Spelling.Table.pc_spell_natural_tbl)

octpc_to_pitch_ks :: Integral i => Octave_PitchClass i -> Pitch
octpc_to_pitch_ks = octpc_to_pitch pc_spell_ks

-- | 'midi_to_pitch' 'pc_spell_ks'.
midi_to_pitch_ks :: Integral i => i -> Pitch
midi_to_pitch_ks = midi_to_pitch (pc_spell_ks :: Spelling Int)

fmidi_to_pitch_ks :: (Show n, RealFrac n) => n -> Pitch
fmidi_to_pitch_ks = fmidi_to_pitch_err pc_spell_ks

midi_detune_to_pitch_ks :: (Integral m, Real c) => (m, c) -> Pitch
midi_detune_to_pitch_ks = midi_detune_to_pitch pc_spell_ks

-- | 'midi_to_pitch' 'pc_spell_sharp'
midi_to_pitch_sharp :: Integral i => i -> Pitch
midi_to_pitch_sharp = midi_to_pitch (pc_spell_sharp :: Spelling Int)

{- | Given 'Spelling' function translate from 'OctPc' notation to 'Pitch'.

>>> octpc_to_pitch pc_spell_sharp (4,6) == Pitch Note.F Note.Sharp 4
True
-}
octpc_to_pitch :: Integral i => Spelling i -> Octave_PitchClass i -> Pitch
octpc_to_pitch sp (o, pc) =
  let (n, a) = sp pc
  in Pitch n a (fromIntegral o)

{- | Midi note number to 'Pitch'.

>>> map (pitch_pp . midi_to_pitch pc_spell_ks) [60,63,66] == ["C4","E♭4","F♯4"]
True
-}
midi_to_pitch :: (Integral i, Integral k) => Spelling k -> i -> Pitch
midi_to_pitch sp = octpc_to_pitch sp . midi_to_octave_pitchclass

{- | Fractional midi note number to 'Pitch'.

>>> let p = Pitch Note.B Note.ThreeQuarterToneFlat 4
>>> map (fmidi_to_pitch pc_spell_ks) [69.25,69.5] == [Nothing,Just p]
True
-}
fmidi_to_pitch :: RealFrac n => Spelling PitchClass -> n -> Maybe Pitch
fmidi_to_pitch sp m =
  let m' = Math.real_round_int m
      (Pitch n a o) = midi_to_pitch sp m'
      q = m - fromIntegral m'
  in case Note.alteration_edit_quarter_tone q a of
      Nothing -> Nothing
      Just a' -> Just (Pitch n a' o)

{- | Erroring variant.

>>> pitch_pp (fmidi_to_pitch_err pc_spell_ks 65.5) == "F𝄲4"
True

>>> pitch_pp (fmidi_to_pitch_err pc_spell_ks 66.5) == "F𝄰4"
True

>>> pitch_pp (fmidi_to_pitch_err pc_spell_ks 67.5) == "A𝄭4"
True

>>> pitch_pp (fmidi_to_pitch_err pc_spell_ks 69.5) == "B𝄭4"
True
-}
fmidi_to_pitch_err :: (Show n, RealFrac n) => Spelling Int -> n -> Pitch
fmidi_to_pitch_err sp m = fromMaybe (error (show ("fmidi_to_pitch", m))) (fmidi_to_pitch sp m)

{- | Composition of 'pitch_to_fmidi' and then 'fmidi_to_pitch'.

>>> pitch_transpose_fmidi pc_spell_ks 2 (Pitch Note.E Note.Sharp 5) == Pitch Note.G Note.Natural 5
True
-}
pitch_transpose_fmidi :: (RealFrac n, Show n) => Spelling Int -> n -> Pitch -> Pitch
pitch_transpose_fmidi sp n p =
  let m = pitch_to_fmidi p
  in fmidi_to_pitch_err sp (m + n)

-- | Displacement of /q/ into octave of /p/.
fmidi_in_octave_of :: RealFrac f => f -> f -> f
fmidi_in_octave_of p = fmidi_in_octave (fmidi_octave p)

{- | Octave displacement of /m2/ that is nearest /m1/.

>>> let p = octpc_to_fmidi (2,1)
>>> let q = map octpc_to_fmidi [(4,11),(4,0),(4,1)]
>>> map (fmidi_in_octave_nearest p) q
[35.0,36.0,37.0]
-}
fmidi_in_octave_nearest :: RealFrac n => n -> n -> n
fmidi_in_octave_nearest m1 m2 =
  let m2' = fmidi_in_octave (fmidi_octave m1) m2
      m2'' = [m2' - 12, m2', m2' + 12]
      d = map (abs . (m1 -)) m2''
      z = sortOn snd (zip m2'' d)
  in fst (List.head_err z)

{- | Displacement of /q/ into octave above /p/.

>>> fmidi_in_octave_of 69 51
63.0

>>> fmidi_in_octave_nearest 69 51
63.0

>>> fmidi_in_octave_above 69 51
75.0
-}
fmidi_in_octave_above :: RealFrac a => a -> a -> a
fmidi_in_octave_above p q = let r = fmidi_in_octave_nearest p q in if r < p then r + 12 else r

{- | Displacement of /q/ into octave below /p/.

>>> fmidi_in_octave_of 69 85
61.0

>>> fmidi_in_octave_nearest 69 85
73.0

>>> fmidi_in_octave_below 69 85
61.0
-}
fmidi_in_octave_below :: RealFrac a => a -> a -> a
fmidi_in_octave_below p q = let r = fmidi_in_octave_nearest p q in if r > p then r - 12 else r

-- | Cps form of binary /fmidi/ function /f/.
lift_fmidi_binop_to_cps :: Floating f => (f -> f -> f) -> f -> f -> f
lift_fmidi_binop_to_cps f p = Tuning.fmidi_to_cps . f (cps_to_fmidi p) . cps_to_fmidi

{- | Cps form of 'fmidi_in_octave_nearest'.

>>> map cps_octave [440,256]
[4,4]

>>> round (cps_in_octave_nearest 440 256)
512
-}
cps_in_octave_nearest :: (Floating f, RealFrac f) => f -> f -> f
cps_in_octave_nearest = lift_fmidi_binop_to_cps fmidi_in_octave_nearest

{- | Cps form of 'fmidi_in_octave_above'.

>>> cps_in_octave_above 55.0 392.0
97.99999999999999
-}
cps_in_octave_above :: (Floating f, RealFrac f) => f -> f -> f
cps_in_octave_above = lift_fmidi_binop_to_cps fmidi_in_octave_above

-- | CPS form of 'fmidi_in_octave_above'.
cps_in_octave_below :: (Floating f, RealFrac f) => f -> f -> f
cps_in_octave_below = lift_fmidi_binop_to_cps fmidi_in_octave_below

{- | Direct implementation of 'cps_in_octave_above'.
Raise or lower the frequency /q/ by octaves until it is in the octave starting at /p/.

>>> cps_in_octave_above_direct 55.0 392.0
98.0
-}
cps_in_octave_above_direct :: (Ord a, Fractional a) => a -> a -> a
cps_in_octave_above_direct p q =
  let f = cps_in_octave_above_direct p
  in if q > p * 2 then f (q / 2) else if q < p then f (q * 2) else q

{- | Set octave of /p2/ so that it nearest to /p1/.

>>> let f = pitch_in_octave_nearest (Pitch Note.C Note.Flat 2)
>>> map (pitch_pp_iso . f) [Pitch Note.B Note.Natural 4,Pitch Note.C Note.Natural 4,Pitch Note.C Note.Sharp 4]
["B1","C2","C#2"]
-}
pitch_in_octave_nearest :: Pitch -> Pitch -> Pitch
pitch_in_octave_nearest p1 p2 =
  let f = pitch_to_fmidi :: Pitch -> Double
      o = fmidi_octave (fmidi_in_octave_nearest (f p1) (f p2))
  in p2 {octave = o}

{- | Raise 'Note' of 'Pitch', account for octave transposition.

>>> pitch_note_raise (Pitch Note.B Note.Natural 3) == Pitch Note.C Note.Natural 4
True
-}
pitch_note_raise :: Pitch -> Pitch
pitch_note_raise (Pitch n a o) =
  if n == maxBound
    then Pitch minBound a (o + 1)
    else Pitch (succ n) a o

{- | Lower 'Note' of 'Pitch', account for octave transposition.

>>> pitch_note_lower (Pitch Note.C Note.Flat 4) == Pitch Note.B Note.Flat 3
True
-}
pitch_note_lower :: Pitch -> Pitch
pitch_note_lower (Pitch n a o) =
  if n == minBound
    then Pitch maxBound a (o - 1)
    else Pitch (pred n) a o

{- | Rewrite 'Pitch' to not use @3/4@ tone alterations, ie. re-spell to @1/4@ alteration.

>>> let p = Pitch Note.A Note.ThreeQuarterToneFlat 4
>>> let q = Pitch Note.G Note.QuarterToneSharp 4
>>> pitch_rewrite_threequarter_alteration p == q
True
-}
pitch_rewrite_threequarter_alteration :: Pitch -> Pitch
pitch_rewrite_threequarter_alteration (Pitch n a o) =
  case a of
    Note.ThreeQuarterToneFlat -> pitch_note_lower (Pitch n Note.QuarterToneSharp o)
    Note.ThreeQuarterToneSharp -> pitch_note_raise (Pitch n Note.QuarterToneFlat o)
    _ -> Pitch n a o

{- | Apply function to 'octave' of 'PitchClass'.

>>> pitch_edit_octave (+ 1) (Pitch Note.A Note.Natural 4) == Pitch Note.A Note.Natural 5
True
-}
pitch_edit_octave :: (Octave -> Octave) -> Pitch -> Pitch
pitch_edit_octave f (Pitch n a o) = Pitch n a (f o)

-- * Frequency (Cps)

-- | 'fmidi_to_cps' of 'pitch_to_fmidi', given (k0,f0).
pitch_to_cps_k0 :: Floating n => (n, n) -> Pitch -> n
pitch_to_cps_k0 o = Tuning.fmidi_to_cps_k0 o . pitch_to_fmidi

-- | 'fmidi_to_cps' of 'pitch_to_fmidi', given frequency of ISO A4.
pitch_to_cps_f0 :: Floating n => n -> Pitch -> n
pitch_to_cps_f0 f0 = pitch_to_cps_k0 (69, f0)

-- | 'pitch_to_cps_k0' (60,440).
pitch_to_cps :: Floating n => Pitch -> n
pitch_to_cps = pitch_to_cps_k0 (69, 440)

-- | Frequency (cps = cycles per second) to fractional /midi/ note number, given frequency of ISO A4 (mnn = 69).
cps_to_fmidi_k0 :: Floating a => (a, a) -> a -> a
cps_to_fmidi_k0 (k0, f0) a = (logBase 2 (a * (1 / f0)) * 12) + k0

{- | 'cps_to_fmidi_k0' @(69,440)@.

>>> cps_to_fmidi 440
69.0

>>> cps_to_fmidi (Tuning.fmidi_to_cps 60.25)
60.25
-}
cps_to_fmidi :: Floating a => a -> a
cps_to_fmidi = cps_to_fmidi_k0 (69, 440)

{- | Frequency (cycles per second) to /midi/ note number, ie. 'round' of 'cps_to_fmidi'.

>>> map cps_to_midi [261.6,440]
[60,69]
-}
cps_to_midi :: (Integral i, Floating f, RealFrac f) => f -> i
cps_to_midi = round . cps_to_fmidi

-- | 'midi_to_cps_f0' of 'octpc_to_midi', given (k0,f0)
octpc_to_cps_k0 :: (Integral i, Floating n) => (n, n) -> Octave_PitchClass i -> n
octpc_to_cps_k0 o = Tuning.midi_to_cps_k0 o . octave_pitchclass_to_midi

{- | 'octpc_to_cps_k0' (69,440).

>>> map (round . octpc_to_cps) [(-1,0),(0,0),(4,9),(9,0)]
[8,16,440,8372]
-}
octpc_to_cps :: (Integral i, Floating n) => Octave_PitchClass i -> n
octpc_to_cps = octpc_to_cps_k0 (69, 440)

-- | 'midi_to_octpc' of 'cps_to_midi'.
cps_to_octpc :: (Floating f, RealFrac f, Integral i) => f -> Octave_PitchClass i
cps_to_octpc = midi_to_octave_pitchclass . Math.real_round_int . cps_to_fmidi

cps_octave :: (Floating f, RealFrac f) => f -> Octave
cps_octave = fst . cps_to_octpc

-- * Midi detune (cents)

{- | Is cents in (-50,+50].

>>> map cents_is_normal [-250,-75,75,250] == replicate 4 False
True
-}
cents_is_normal :: (Num c, Ord c) => c -> Bool
cents_is_normal c = c > (-50) && c <= 50

-- | 'cents_is_normal' of 'snd'.
midi_detune_is_normal :: (Num c, Ord c) => (x, c) -> Bool
midi_detune_is_normal = cents_is_normal . snd

{- | In normal form the detune is in the range (-50,+50] instead of [0,100) or wider.

>>> map midi_detune_normalise [(60,-250),(60,-75),(60,75),(60,250)]
[(57,50),(59,25),(61,-25),(62,50)]
-}
midi_detune_normalise :: (Num m, Ord c, Num c) => (m, c) -> (m, c)
midi_detune_normalise =
  let recur (m, c) =
        if c > 50
          then recur (m + 1, c - 100)
          else
            if c > (-50)
              then (m, c)
              else recur (m - 1, c + 100)
  in recur

{- | In normal-positive form the detune is in the range (0,+100].

>>> map midi_detune_normalise_positive [(60,-250),(60,-75),(60,75),(60,250)]
[(57,50),(59,25),(60,75),(62,50)]
-}
midi_detune_normalise_positive :: (Num m, Ord m, Ord c, Num c) => (m, c) -> (m, c)
midi_detune_normalise_positive =
  let recur (m, c) =
        if c < 0
          then recur (m - 1, c + 100)
          else
            if c > 100
              then recur (m + 1, c - 100)
              else (m, c)
  in recur

-- | Inverse of 'cps_to_midi_detune', given frequency of ISO @A4@.
midi_detune_to_cps_f0 :: (Integral m, Real c) => Double -> (m, c) -> Double
midi_detune_to_cps_f0 f0 (m, c) = Tuning.fmidi_to_cps_f0 f0 (fromIntegral m + (realToFrac c / 100))

{- | Inverse of 'cps_to_midi_detune'.

>>> map midi_detune_to_cps [(69,0),(68,100)]
[440.0,440.0]
-}
midi_detune_to_cps :: (Integral m, Real c) => (m, c) -> Double
midi_detune_to_cps = midi_detune_to_cps_f0 440

{- | 'Midi_Detune' to fractional midi note number.

>>> midi_detune_to_fmidi (60,50)
60.5
-}
midi_detune_to_fmidi :: (Integral m, Real c) => (m, c) -> Double
midi_detune_to_fmidi (mnn, c) = fromIntegral mnn + (realToFrac c / 100)

{- | 'Midi_Detune' to 'Pitch', detune must be precisely at a notateable Pitch.

>>> midi_detune_to_pitch pc_spell_ks (midi_detune_nearest_24et (60,35))
Pitch {note = C, alteration = QuarterToneSharp, octave = 4}
-}
midi_detune_to_pitch :: (Integral m, Real c) => Spelling Int -> (m, c) -> Pitch
midi_detune_to_pitch sp = fmidi_to_pitch_err sp . cps_to_fmidi . midi_detune_to_cps

-- | Midi note number with real-valued cents detune.
type Midi_Detune = (Midi, Double)

{- | Fractional midi note number to 'Midi_Detune'.

>>> fmidi_to_midi_detune 60.50
(60,50.0)
-}
fmidi_to_midi_detune :: Double -> Midi_Detune
fmidi_to_midi_detune mnn =
  let (n, c) = Math.integral_and_fractional_parts mnn
  in (n, c * 100)

{- | Frequency (in hertz) to 'Midi_Detune'.

>>> map (fmap round . cps_to_midi_detune) [440.00,508.35]
[(69,0),(71,50)]
-}
cps_to_midi_detune :: Double -> Midi_Detune
cps_to_midi_detune = fmidi_to_midi_detune . cps_to_fmidi

{- | Round /detune/ value to nearest multiple of @50@, normalised.

>>> map midi_detune_nearest_24et [(59,70),(59,80)]
[(59,50.0),(60,0.0)]
-}
midi_detune_nearest_24et :: Midi_Detune -> Midi_Detune
midi_detune_nearest_24et (m, dt) = midi_detune_normalise (m, Math.round_to 50 dt)

-- * Midi cents

-- | Midi note number with integral cents detune.
type Midi_Cents = (Midi, Int)

midi_detune_to_midi_cents :: Midi_Detune -> Midi_Cents
midi_detune_to_midi_cents (m, c) = (m, round c)

{- | Printed as /fmidi/ value with cents to two places.  Must be normal.

>>> map midi_cents_pp [(60,0),(60,25)]
["60.00","60.25"]
-}
midi_cents_pp :: Midi_Cents -> String
midi_cents_pp (m, c) = if cents_is_normal c then printf "%d.%02d" m c else error "midi_cents_pp"

-- * 24et

{- | The 24ET pitch-class universe, with /sharp/ spellings, in octave '4'.

>>> length pc24et_univ
24

>>> let r = "C C𝄲 C♯ C𝄰 D D𝄲 D♯ D𝄰 E E𝄲 F F𝄲 F♯ F𝄰 G G𝄲 G♯ G𝄰 A A𝄲 A♯ A𝄰 B B𝄲"
>>> unwords (map pitch_class_pp pc24et_univ) == r
True
-}
pc24et_univ :: [Pitch]
pc24et_univ =
  let a = [Note.Natural, Note.QuarterToneSharp, Note.Sharp, Note.ThreeQuarterToneSharp]
      f (n, k) = map (\i -> Pitch n (a !! i) 4) [0 .. k - 1]
  in concatMap f (zip Note.note_seq [4, 4, 2, 4, 4, 4, 2])

{- | 'genericIndex' into 'pc24et_univ'.

>>> pitch_class_pp (pc24et_to_pitch 13) == "F𝄰"
True
-}
pc24et_to_pitch :: Integral i => i -> Pitch
pc24et_to_pitch = genericIndex pc24et_univ

-- * Pitch, rational alteration.

-- | Generalised pitch, given by a generalised alteration.
data Pitch_R = Pitch_R Note.Note Note.Alteration_R Octave
  deriving (Eq, Show)

-- | Pretty printer for 'Pitch_R'.
pitch_r_pp :: Pitch_R -> String
pitch_r_pp (Pitch_R n (_, a) o) = show n ++ a ++ show o

-- | 'Pitch_R' printed without octave.
pitch_r_class_pp :: Pitch_R -> String
pitch_r_class_pp = List.dropWhileRight isDigit . pitch_r_pp

-- * Parsers

-- | Parser for single digit ISO octave (C4 = middle-C)
p_octave_iso :: Parse.P Octave
p_octave_iso = fmap digitToInt Parsec.digit

-- | Parser for single digit ISO octave with default value in case of no parse.
p_octave_iso_opt :: Octave -> Parse.P Octave
p_octave_iso_opt def_o = do
  o <- Parsec.optionMaybe p_octave_iso
  return (fromMaybe def_o o)

-- | Parser for ISO pitch notation.
p_iso_pitch_strict :: Parse.P Pitch
p_iso_pitch_strict = do
  n <- Note.p_note_t
  a <- Note.p_alteration_t_iso True
  o <- p_octave_iso
  return (Pitch n a o)

-- | Parser for extended form of ISO pitch notation.
p_iso_pitch_oct :: Octave -> Parse.P Pitch
p_iso_pitch_oct def_o = do
  n <- Note.p_note_t_ci -- Iso requires upper case note names
  a <- Note.p_alteration_t_iso False -- Iso does not allow ##
  o <- p_octave_iso_opt def_o -- Iso requires octave
  return (Pitch n a o)

{- | Parse possible octave from single integer.

>>> map (parse_octave 2) ["","4","x","11"]
[2,4,2,1]
-}
parse_octave :: Octave -> String -> Octave
parse_octave def_o = Parse.run_parser_error (p_octave_iso_opt def_o)

{- | Generalisation of ISO pitch representation.
Allows octave to be elided, pitch names to be lower case, and double sharps written as @##@.
See <http://www.musiccog.ohio-state.edu/Humdrum/guide04.html>

>>> let r = [Pitch Note.C Note.Natural 4,Pitch Note.A Note.Flat 5,Pitch Note.F Note.DoubleSharp 6]
>>> mapMaybe (parse_iso_pitch_oct 4) ["C","Ab5","f##6",""] == r
True
-}
parse_iso_pitch_oct :: Octave -> String -> Maybe Pitch
parse_iso_pitch_oct def_o = Parse.run_parser_maybe (p_iso_pitch_oct def_o)

-- | Variant of 'parse_iso_pitch_oct' requiring octave.
parse_iso_pitch :: String -> Maybe Pitch
parse_iso_pitch = parse_iso_pitch_oct (error "parse_iso_pitch: no octave")

-- | 'error' variant.
parse_iso_pitch_err :: String -> Pitch
parse_iso_pitch_err = fromMaybe (error "parse_iso_pitch") . parse_iso_pitch

-- * Pretty printers

{- | Pretty printer for 'Pitch' (unicode, see 'alteration_symbol').
Option selects if 'Natural's are printed.

>>> pitch_pp_opt (True,True) (Pitch Note.E Note.Natural 4) == "E♮4"
True
-}
pitch_pp_opt :: (Bool, Bool) -> Pitch -> String
pitch_pp_opt (show_nat, show_oct) (Pitch n a o) =
  let a' = if a == Note.Natural && not show_nat then "" else [Note.alteration_symbol a]
      rem_oct_f c = isDigit c || c == '-' -- negative octave values...
      rem_oct = if show_oct then id else List.dropWhileRight rem_oct_f
  in rem_oct (show n ++ a' ++ show o)

{- | 'pitch_pp_opt' with default options, ie. (False,True).

>>> pitch_pp (Pitch Note.E Note.Natural 4)
"E4"

>>> pitch_pp (Pitch Note.E Note.Flat 4) == "E♭4"
True

>>> pitch_pp (Pitch Note.F Note.QuarterToneSharp 3) == "F𝄲3"
True
-}
pitch_pp :: Pitch -> String
pitch_pp = pitch_pp_opt (False, True)

{- | 'pitch_pp_opt' with options (False,False).

>>> pitch_class_pp (Pitch Note.C Note.ThreeQuarterToneSharp 0) == "C𝄰"
True
-}
pitch_class_pp :: Pitch -> String
pitch_class_pp = pitch_pp_opt (False, False)

{- | Sequential list of /n/ pitch class names starting from /k/.

>>> unwords (pitch_class_names_12et pc_spell_ks 0 12) == "C C♯ D E♭ E F F♯ G A♭ A B♭ B"
True

>>> pitch_class_names_12et pc_spell_ks 11 2
["B","C"]
-}
pitch_class_names_12et :: Integral n => Spelling n -> n -> n -> [String]
pitch_class_names_12et sp k n =
  let f = pitch_class_pp . midi_to_pitch sp . Math.from_integral_to_int
  in map f [60 + k .. 60 + k + n - 1]

{- | Pretty printer for 'Pitch' (ISO, ASCII, see 'alteration_iso').

>>> pitch_pp_iso (Pitch Note.E Note.Flat 4)
"Eb4"

>>> pitch_pp_iso (Pitch Note.F Note.DoubleSharp 3)
"Fx3"

> pitch_pp_iso (Pitch Note.C Note.ThreeQuarterToneSharp 4) -- error
-}
pitch_pp_iso :: Pitch -> String
pitch_pp_iso (Pitch n a o) = show n ++ Note.alteration_iso a ++ show o

-- | Lilypond octave syntax.
ly_octave_tbl :: [(Octave, String)]
ly_octave_tbl =
  [ (-1, ",,,,")
  , (0, ",,,")
  , (1, ",,")
  , (2, ",")
  , (3, "")
  , (4, "'")
  , (5, "''")
  , (6, "'''")
  , (7, "''''")
  , (8, "'''''")
  ]

-- | Lookup 'ly_octave_tbl'.
octave_pp_ly :: Octave -> String
octave_pp_ly o = List.lookup_err o ly_octave_tbl

-- | Parse lilypond octave indicator.
octave_parse_ly :: String -> Maybe Octave
octave_parse_ly s = List.reverse_lookup s ly_octave_tbl

{- | Pretty printer for 'Pitch' (ASCII, see 'alteration_tonh').

>>> pitch_pp_hly (Pitch Note.E Note.Flat 4)
"ees4"

>>> pitch_pp_hly (Pitch Note.F Note.QuarterToneSharp 3)
"fih3"

>>> pitch_pp_hly (Pitch Note.B Note.Natural 6)
"b6"
-}
pitch_pp_hly :: Pitch -> String
pitch_pp_hly (Pitch n a o) =
  let n' = map toLower (show n)
  in n' ++ Note.alteration_tonh a ++ show o

{- | Pretty printer for 'Pitch' (Tonhöhe, see 'alteration_tonh').

>>> pitch_pp_tonh (Pitch Note.E Note.Flat 4)
"Es4"

>>> pitch_pp_tonh (Pitch Note.F Note.QuarterToneSharp 3)
"Fih3"

>>> pitch_pp_tonh (Pitch Note.B Note.Natural 6)
"H6"
-}
pitch_pp_tonh :: Pitch -> String
pitch_pp_tonh (Pitch n a o) =
  let o' = show o
  in case (n, a) of
      (Note.B, Note.Natural) -> "H" ++ o'
      (Note.B, Note.Flat) -> "B" ++ o'
      (Note.B, Note.DoubleFlat) -> "Heses" ++ o'
      (Note.A, Note.Flat) -> "As" ++ o'
      (Note.E, Note.Flat) -> "Es" ++ o'
      _ -> show n ++ Note.alteration_tonh a ++ o'

-- * Parsers

p_octave_ly :: Parse.P Octave
p_octave_ly =
  fmap
    (fromMaybe (error "p_octave_ly") . octave_parse_ly)
    (Parsec.many1 (Parsec.oneOf ",'"))

p_pitch_ly :: Parse.P Pitch
p_pitch_ly = do
  (n, a) <- Note.p_note_alteration_ly
  o <- Parsec.optionMaybe p_octave_ly
  return (Pitch n (fromMaybe Note.Natural a) (fromMaybe 3 o))

{- | Run 'p_pitch_ly'.

>>> map (pitch_pp . pitch_parse_ly_err) ["c","d'","ees,","fisis''"] == ["C3","D4","E♭2","F𝄪5"]
True
-}
pitch_parse_ly_err :: String -> Pitch
pitch_parse_ly_err = Parse.run_parser_error p_pitch_ly

-- | Parser for hly notation.
p_pitch_hly :: Parse.P Pitch
p_pitch_hly = do
  (n, a) <- Note.p_note_alteration_ly
  fmap (Pitch n (fromMaybe Note.Natural a)) p_octave_iso

{- | Run 'p_pitch_hly'.

>>> map (pitch_pp . pitch_parse_hly) ["ees4","fih3","b6"] == ["E♭4","F𝄲3","B6"]
True
-}
pitch_parse_hly :: String -> Pitch
pitch_parse_hly = Parse.run_parser_error p_pitch_hly
