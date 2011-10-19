-- | Common music notation pitch values.
module Music.Theory.Pitch where

import Data.Function

-- | Pitch classes are modulo twelve integers.
type PitchClass = Integer

-- | Octaves are integers, the octave of middle C is @4@.
type Octave = Integer

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

-- | Transform 'Alteration_T' to semitone alteration.
--
-- > map alteration_to_diff [Flat,Sharp] == [-1,1]
alteration_to_diff :: Alteration_T -> Integer
alteration_to_diff a =
    case a of
      DoubleFlat -> -2
      Flat -> -1
      Natural -> 0
      Sharp -> 1
      DoubleSharp -> 2
      _ -> error "alteration_to_diff: quarter tone"

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
      _ -> fromIntegral (alteration_to_diff a)

-- | Unicode has entries for /Musical Symbols/ in the range @U+1D100@
-- through @U+1D1FF@.  The @3/4@ symbols are non-standard, here they
-- correspond to @MUSICAL SYMBOL FLAT DOWN@ and @MUSICAL SYMBOL SHARP
-- UP@.
--
-- > map alteration_symbol [minBound .. maxBound]
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
pitch_to_octpc :: Pitch -> (Octave,PitchClass)
pitch_to_octpc = midi_to_octpc . pitch_to_midi

-- | 'Pitch' to midi note number notation.
--
-- > pitch_to_midi (Pitch A Natural 4) == 69
pitch_to_midi :: Pitch -> Integer
pitch_to_midi (Pitch n a o) =
    let a' = alteration_to_diff a
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
pitch_to_pc (Pitch n a _) = note_to_pc n + alteration_to_diff a

-- | 'Pitch' comparison, implemented via 'pitch_to_fmidi'.
--
-- > pitch_compare (Pitch A Natural 4) (Pitch A QuarterToneSharp 4) == LT
pitch_compare :: Pitch -> Pitch -> Ordering
pitch_compare = compare `on` pitch_to_fmidi

-- | Function to spell a 'PitchClass'.
type Spelling = PitchClass -> (Note_T, Alteration_T)

-- | Given 'Spelling' function translate from /octpc/ notation to
-- 'Pitch'.
octpc_to_pitch :: Spelling -> (Octave, PitchClass) -> Pitch
octpc_to_pitch sp (o,pc) =
    let (n,a) = sp pc
    in Pitch n a o

-- | Normalise /octpc/ value, ie. ensure 'PitchClass' is in (0,11).
--
-- > octpc_nrm (4,16) == (5,4)
octpc_nrm :: (Octave, PitchClass) -> (Octave, PitchClass)
octpc_nrm (o,pc) =
    if pc > 11
    then octpc_nrm (o+1,pc-12)
    else if pc < 0
         then octpc_nrm (o-1,pc+12)
         else (o,pc)

-- | Transpose /octpc/ value.
--
-- > octpc_trs 7 (4,9) == (5,4)
octpc_trs :: Integer -> (Octave, PitchClass) -> (Octave, PitchClass)
octpc_trs n (o,pc) = octpc_nrm (o,pc+n)

-- | /octpc/ value to /midi/ value.
--
-- > octpc_to_midi (4,9) == 69
octpc_to_midi :: (Octave, PitchClass) -> Integer
octpc_to_midi (o,pc) = 60 + ((o - 4) * 12) + pc

-- | Inverse of 'octpc_to_midi'.
--
-- > midi_to_octpc 69 == (4,9)
midi_to_octpc :: Integer -> (Octave, PitchClass)
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
