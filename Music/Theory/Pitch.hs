-- | Common music notation pitch values.
module Music.Theory.Pitch where

import Data.Function
import Data.Maybe

-- | Pitch classes are modulo twelve integers.
type PitchClass = Integer

-- | Octaves are integers, the octave of middle C is @4@.
type Octave = Integer

-- | 'Octave' and 'PitchClass' duple.
type OctPC = (Octave,PitchClass)

-- | Enumeration of common music notation note names (@C@ to @B@).
data Note_T = C | D | E | F | G | A | B
              deriving (Eq,Enum,Bounded,Ord,Show)

-- | Enumeration of common music notation note alterations.
data Alteration_T = DoubleFlat
                  | ThreeQuarterToneFlat | Flat | QuarterToneFlat
                  | Natural
                  | QuarterToneSharp | Sharp | ThreeQuarterToneSharp
                  | DoubleSharp
                    deriving (Eq,Enum,Bounded,Ord,Show)

-- | Common music notation pitch value.
data Pitch = Pitch {note :: Note_T
                   ,alteration :: Alteration_T
                   ,octave :: Octave}
           deriving (Eq, Show)

instance Ord Pitch where
    compare = pitch_compare

-- | Transform 'Note_T' to 'PitchClass'.
--
-- > map note_to_pc [C,E,G] == [0,4,7]
note_to_pc :: Note_T -> PitchClass
note_to_pc n =
    case n of
      C -> 0
      D -> 2
      E -> 4
      F -> 5
      G -> 7
      A -> 9
      B -> 11

-- | Transform 'Alteration_T' to semitone alteration.  Returns
-- 'Nothing' for non-semitone alterations.
--
-- > map alteration_to_diff [Flat,QuarterToneSharp] == [Just (-1),Nothing]
alteration_to_diff :: Alteration_T -> Maybe Integer
alteration_to_diff a =
    case a of
      DoubleFlat -> Just (-2)
      Flat -> Just (-1)
      Natural -> Just 0
      Sharp -> Just 1
      DoubleSharp -> Just 2
      _ -> Nothing

-- | Transform 'Alteration_T' to semitone alteration.
--
-- > map alteration_to_diff_err [Flat,Sharp] == [-1,1]
alteration_to_diff_err :: Alteration_T -> Integer
alteration_to_diff_err =
    let err = error "alteration_to_diff: quarter tone"
    in fromMaybe err . alteration_to_diff

-- | Transform 'Alteration_T' to fractional semitone alteration,
-- ie. allow quarter tones.
--
-- > alteration_to_fdiff QuarterToneSharp == 0.5
alteration_to_fdiff :: Alteration_T -> Double
alteration_to_fdiff a =
    case a of
      ThreeQuarterToneFlat -> -1.5
      QuarterToneFlat -> -0.5
      QuarterToneSharp -> 0.5
      ThreeQuarterToneSharp -> 1.5
      _ -> fromIntegral (alteration_to_diff_err a)

-- | Unicode has entries for /Musical Symbols/ in the range @U+1D100@
-- through @U+1D1FF@.  The @3/4@ symbols are non-standard, here they
-- correspond to @MUSICAL SYMBOL FLAT DOWN@ and @MUSICAL SYMBOL SHARP
-- UP@.
--
-- > map alteration_symbol [minBound .. maxBound] == "𝄫𝄭♭𝄳♮𝄲♯𝄰𝄪"
alteration_symbol :: Alteration_T -> Char
alteration_symbol a =
    case a of
      DoubleFlat -> '𝄫'
      ThreeQuarterToneFlat -> '𝄭'
      Flat -> '♭'
      QuarterToneFlat -> '𝄳'
      Natural -> '♮'
      QuarterToneSharp -> '𝄲'
      Sharp -> '♯'
      ThreeQuarterToneSharp -> '𝄰'
      DoubleSharp -> '𝄪'

-- | 'Pitch' to 'Octave' and 'PitchClass' notation.
--
-- > pitch_to_octpc (Pitch F Sharp 4) == (4,6)
pitch_to_octpc :: Pitch -> OctPC
pitch_to_octpc = midi_to_octpc . pitch_to_midi

-- | 'Pitch' to midi note number notation.
--
-- > pitch_to_midi (Pitch A Natural 4) == 69
pitch_to_midi :: Pitch -> Integer
pitch_to_midi (Pitch n a o) =
    let a' = alteration_to_diff_err a
        n' = note_to_pc n
    in 12 + o * 12 + n' + a'

-- | 'Pitch' to fractional midi note number notation.
--
-- > pitch_to_fmidi (Pitch A QuarterToneSharp 4) == 69.5
pitch_to_fmidi :: Pitch -> Double
pitch_to_fmidi (Pitch n a o) =
    let a' = alteration_to_fdiff a
        o' = fromIntegral o
        n' = fromIntegral (note_to_pc n)
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
pitch_compare = compare `on` pitch_to_fmidi

-- | Function to spell a 'PitchClass'.
type Spelling = PitchClass -> (Note_T, Alteration_T)

-- | Given 'Spelling' function translate from 'OctPC' notation to
-- 'Pitch'.
octpc_to_pitch :: Spelling -> OctPC -> Pitch
octpc_to_pitch sp (o,pc) =
    let (n,a) = sp pc
    in Pitch n a o

-- | Normalise 'OctPC' value, ie. ensure 'PitchClass' is in (0,11).
--
-- > octpc_nrm (4,16) == (5,4)
octpc_nrm :: OctPC -> OctPC
octpc_nrm (o,pc) =
    if pc > 11
    then octpc_nrm (o+1,pc-12)
    else if pc < 0
         then octpc_nrm (o-1,pc+12)
         else (o,pc)

-- | Transpose 'OctPC' value.
--
-- > octpc_trs 7 (4,9) == (5,4)
octpc_trs :: Integer -> OctPC -> OctPC
octpc_trs n (o,pc) = octpc_nrm (o,pc+n)

-- | 'OctPC' value to integral /midi/ note number.
--
-- > octpc_to_midi (4,9) == 69
octpc_to_midi :: OctPC -> Integer
octpc_to_midi (o,pc) = 60 + ((o - 4) * 12) + pc

-- | Inverse of 'octpc_to_midi'.
--
-- > midi_to_octpc 69 == (4,9)
midi_to_octpc :: Integer -> OctPC
midi_to_octpc n = (n - 12) `divMod` 12

-- | Apply function to 'octave' of 'PitchClass'.
--
-- > pitch_edit_octave (+ 1) (Pitch A Natural 4) == Pitch A Natural 5
pitch_edit_octave :: (Integer -> Integer) -> Pitch -> Pitch
pitch_edit_octave f (Pitch n a o) = Pitch n a (f o)

-- | Modal transposition of 'Note_T' value.
--
-- > note_t_transpose C 2 == E
note_t_transpose :: Note_T -> Int -> Note_T
note_t_transpose x n =
    let x' = fromEnum x
        n' = fromEnum (maxBound::Note_T) + 1
    in toEnum ((x' + n) `mod` n')

-- * Frequency

-- | Fractional /midi/ note number to cycles per second.
--
-- > map midi_to_cps [69,69.1] == [440.0,442.5488940698553]
midi_to_cps :: Floating a => a -> a
midi_to_cps i = 440 * (2 ** ((i - 69) * (1 / 12)))

-- | Frequency (cycles per second) to fractional /midi/ note number.
--
-- > cps_to_midi 440 == 69
-- > cps_to_midi (midi_to_cps 60.25) == 60.25
cps_to_midi :: Floating a => a -> a
cps_to_midi a = (logBase 2 (a * (1 / 440)) * 12) + 69

-- | 'midi_to_cps' of 'octpc_to_midi'.
--
-- > octpc_to_cps (4,9) == 440
octpc_to_cps :: Floating n => OctPC -> n
octpc_to_cps = midi_to_cps . fromIntegral . octpc_to_midi

-- | Convert from cents invterval to frequency ratio.
--
-- > map cents_to_ratio [0,701.9550008653874,1200] == [1,3/2,2]
cents_to_ratio :: Floating a => a -> a
cents_to_ratio n = 2 ** (n / 1200)

-- | Convert from frequency ratio to cents interval.
--
-- > map ratio_to_cents [1,4/3,2] == [0.0,498.04499913461245,1200.0]
ratio_to_cents :: Floating a => a -> a
ratio_to_cents n = logBase 2 n * 1200

-- | Frequency /n/ cents from /f/.
--
-- > map (cps_shift_cents 440) [-100,100] == map octpc_to_cps [(4,8),(4,10)]
cps_shift_cents :: Floating a => a -> a -> a
cps_shift_cents f = (* f) . cents_to_ratio

-- | Interval in /cents/ from /p/ to /q/, ie. 'ratio_to_cents' of /p/
-- '/' /q/.
--
-- > cps_difference_cents 440 (octpc_to_cps (5,2)) == 500
--
-- > let abs_dif i j = abs (i - j)
-- > in cps_difference_cents 440 (midi_to_cps 69.1) `abs_dif` 10 < 1e9
cps_difference_cents :: Floating a => a -> a -> a
cps_difference_cents p q = ratio_to_cents (q / p)
