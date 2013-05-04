-- | Common music notation pitch values.
module Music.Theory.Pitch where

import Data.Char
import Data.Function
import Data.Maybe

-- | Pitch classes are modulo twelve integers.
type PitchClass = Integer

-- | Octaves are 'Integer's, the octave of middle C is @4@.
type Octave = Integer

-- | 'Octave' and 'PitchClass' duple.
type Octave_PitchClass i = (i,i)
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
           deriving (Eq,Show)

instance Ord Pitch where
    compare = pitch_compare

-- | Transform 'Note_T' to pitch-class number.
--
-- > map note_to_pc [C,E,G] == [0,4,7]
note_to_pc :: Integral i => Note_T -> i
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
alteration_to_diff :: Integral i => Alteration_T -> Maybe i
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
alteration_to_diff_err :: Integral i => Alteration_T -> i
alteration_to_diff_err =
    let err = error "alteration_to_diff: quarter tone"
    in fromMaybe err . alteration_to_diff

-- | Transform 'Alteration_T' to fractional semitone alteration,
-- ie. allow quarter tones.
--
-- > alteration_to_fdiff QuarterToneSharp == 0.5
alteration_to_fdiff :: Fractional n => Alteration_T -> n
alteration_to_fdiff a =
    case a of
      ThreeQuarterToneFlat -> -1.5
      QuarterToneFlat -> -0.5
      QuarterToneSharp -> 0.5
      ThreeQuarterToneSharp -> 1.5
      _ -> fromInteger (alteration_to_diff_err a)

-- | Transform fractional semitone alteration to 'Alteration_T',
-- ie. allow quarter tones.
--
-- > map fdiff_to_alteration [-0.5,0.5] == [Just QuarterToneFlat
-- >                                       ,Just QuarterToneSharp]
fdiff_to_alteration :: (Fractional n,Eq n) => n -> Maybe Alteration_T
fdiff_to_alteration d =
    case d of
      -2 -> Just DoubleFlat
      -1.5 -> Just ThreeQuarterToneFlat
      -1 -> Just Flat
      -0.5 -> Just QuarterToneFlat
      0 -> Just Natural
      0.5 -> Just QuarterToneSharp
      1 -> Just Sharp
      1.5 -> Just ThreeQuarterToneSharp
      2 -> Just DoubleSharp
      _ -> undefined

-- | Raise 'Alteration_T' by a quarter tone where possible.
--
-- > alteration_raise_quarter_tone Flat == Just QuarterToneFlat
-- > alteration_raise_quarter_tone DoubleSharp == Nothing
alteration_raise_quarter_tone :: Alteration_T -> Maybe Alteration_T
alteration_raise_quarter_tone a =
    if a == maxBound then Nothing else Just (toEnum (fromEnum a + 1))

-- | Lower 'Alteration_T' by a quarter tone where possible.
--
-- > alteration_lower_quarter_tone Sharp == Just QuarterToneSharp
-- > alteration_lower_quarter_tone DoubleFlat == Nothing
alteration_lower_quarter_tone :: Alteration_T -> Maybe Alteration_T
alteration_lower_quarter_tone a =
    if a == minBound then Nothing else Just (toEnum (fromEnum a - 1))

-- | Edit 'Alteration_T' by a quarter tone where possible, @-0.5@
-- lowers, @0@ retains, @0.5@ raises.
alteration_edit_quarter_tone :: (Fractional n,Eq n) =>
                                n -> Alteration_T -> Maybe Alteration_T
alteration_edit_quarter_tone n a =
    case n of
      -0.5 -> alteration_lower_quarter_tone a
      0 -> Just a
      0.5 -> alteration_raise_quarter_tone a
      _ -> Nothing

-- | Simplify 'Alteration_T' to standard 12ET by deleting quarter tones.
--
-- > Data.List.nub (map alteration_clear_quarter_tone [minBound..maxBound])
alteration_clear_quarter_tone :: Alteration_T -> Alteration_T
alteration_clear_quarter_tone x =
    case x of
      ThreeQuarterToneFlat -> Flat
      QuarterToneFlat -> Flat
      QuarterToneSharp -> Sharp
      ThreeQuarterToneSharp -> Sharp
      _ -> x

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
pitch_to_fmidi :: Pitch -> Double
pitch_to_fmidi (Pitch n a o) =
    let a' = alteration_to_fdiff a
        o' = fromInteger o
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
pitch_compare = compare `on` pitch_to_fmidi

-- | Function to spell a 'PitchClass'.
type Spelling n = n -> (Note_T,Alteration_T)

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
-- > octpc_to_midi (4,9) == 69
octpc_to_midi :: Integral i => Octave_PitchClass i -> i
octpc_to_midi (o,pc) = 60 + ((fromIntegral o - 4) * 12) + pc

-- | Inverse of 'octpc_to_midi'.
--
-- > midi_to_octpc 69 == (4,9)
midi_to_octpc :: Integral i => i -> Octave_PitchClass i
midi_to_octpc n = (n - 12) `divMod` 12

-- | Midi note number to 'Pitch'.
--
-- > let r = ["C4","E♭4","F♯4"]
-- > in map (pitch_pp . midi_to_pitch pc_spell_ks) [60,63,66] == r
midi_to_pitch :: Integral i => Spelling i -> i -> Pitch
midi_to_pitch sp = octpc_to_pitch sp . midi_to_octpc

-- | Fractional midi note number to 'Pitch'.
--
-- > import Music.Theory.Pitch.Spelling
-- > pitch_pp (fmidi_to_pitch pc_spell_ks 65.5) == "F𝄲4"
-- > pitch_pp (fmidi_to_pitch pc_spell_ks 66.5) == "F𝄰4"
-- > pitch_pp (fmidi_to_pitch pc_spell_ks 67.5) == "A𝄭4"
-- > pitch_pp (fmidi_to_pitch pc_spell_ks 69.5) == "B𝄭4"
fmidi_to_pitch :: RealFrac n => Spelling Integer -> n -> Pitch
fmidi_to_pitch sp m =
    let m' = round m
        (Pitch n a o) = midi_to_pitch sp m'
        Just a' = alteration_edit_quarter_tone (m - fromIntegral m') a
    in Pitch n a' o

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

-- * Frequency (CPS)

-- | /Midi/ note number to cycles per second.
--
-- > map midi_to_cps [60,69] == [261.6255653005986,440.0]
midi_to_cps :: (Integral i,Floating f) => i -> f
midi_to_cps = fmidi_to_cps . fromIntegral

-- | Fractional /midi/ note number to cycles per second.
--
-- > map fmidi_to_cps [69,69.1] == [440.0,442.5488940698553]
fmidi_to_cps :: Floating a => a -> a
fmidi_to_cps i = 440 * (2 ** ((i - 69) * (1 / 12)))

-- | Frequency (cycles per second) to /midi/ note number.
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

-- | 'midi_to_cps' of 'octpc_to_midi'.
--
-- > octpc_to_cps (4,9) == 440
octpc_to_cps :: (Integral i,Floating n) => Octave_PitchClass i -> n
octpc_to_cps = midi_to_cps . octpc_to_midi

-- * Parsers

-- | Slight generalisation of ISO pitch representation.  Allows octave
-- to be elided, pitch names to be lower case, and double sharps
-- written as @##@.
--
-- See <http://www.musiccog.ohio-state.edu/Humdrum/guide04.html>
--
-- > let r = [Pitch C Natural 4,Pitch A Flat 5,Pitch F DoubleSharp 6]
-- > in mapMaybe (parse_iso_pitch_ext 4) ["C","Ab5","f##6",""] == r
parse_iso_pitch_oct :: Octave -> String -> Maybe Pitch
parse_iso_pitch_oct def_o s =
    let nte n = let tb = zip "cdefgab" [C,D,E,F,G,A,B]
                in lookup (toLower n) tb
        oct o = case o of {[] -> def_o;_ -> read o}
        mk n a o = case nte n of
                   Nothing -> Nothing
                   Just n' -> Just (Pitch n' a (oct o))
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

-- | Unicode has entries for /Musical Symbols/ in the range @U+1D100@
-- through @U+1D1FF@.  The @3/4@ symbols are non-standard, here they
-- correspond to @MUSICAL SYMBOL FLAT DOWN@ and @MUSICAL SYMBOL SHARP
-- UP@.
--
-- > map alteration_symbol [minBound .. maxBound] == "𝄫𝄭♭𝄳♮𝄲♯𝄰𝄪"
alteration_symbol :: Alteration_T -> Char
alteration_symbol a =    case a of
      DoubleFlat -> '𝄫'
      ThreeQuarterToneFlat -> '𝄭'
      Flat -> '♭'
      QuarterToneFlat -> '𝄳'
      Natural -> '♮'
      QuarterToneSharp -> '𝄲'
      Sharp -> '♯'
      ThreeQuarterToneSharp -> '𝄰'
      DoubleSharp -> '𝄪'

-- | The @ISO@ ASCII spellings for alterations.  Naturals as written
-- as the empty string.
--
-- > mapMaybe alteration_iso_m [Flat .. Sharp] == ["b","","#"]
alteration_iso_m :: Alteration_T -> Maybe String
alteration_iso_m a =
    case a of
      DoubleFlat -> Just "bb"
      ThreeQuarterToneFlat -> Nothing
      Flat -> Just "b"
      QuarterToneFlat -> Nothing
      Natural -> Just ""
      QuarterToneSharp -> Nothing
      Sharp -> Just "#"
      ThreeQuarterToneSharp -> Nothing
      DoubleSharp -> Just "x"

-- | The @ISO@ ASCII spellings for alterations.
alteration_iso :: Alteration_T -> String
alteration_iso =
    let qt = error "alteration_iso: quarter tone"
    in fromMaybe qt . alteration_iso_m

-- | The /Tonhöhe/ ASCII spellings for alterations.
--
-- See <http://www.musiccog.ohio-state.edu/Humdrum/guide04.html> and
-- <http://lilypond.org/doc/v2.16/Documentation/notation/writing-pitches>
--
-- > map alteration_tonh [Flat .. Sharp] == ["es","eh","","ih","is"]
alteration_tonh :: Alteration_T -> String
alteration_tonh a =
    case a of
      DoubleFlat -> "eses"
      ThreeQuarterToneFlat -> "eseh"
      Flat -> "es"
      QuarterToneFlat -> "eh"
      Natural -> ""
      QuarterToneSharp -> "ih"
      Sharp -> "is"
      ThreeQuarterToneSharp -> "isih"
      DoubleSharp -> "isis"

-- | Pretty printer for 'Pitch' (unicode, see 'alteration_symbol').
--
-- > pitch_pp (Pitch E Flat 4) == "E♭4"
-- > pitch_pp (Pitch F QuarterToneSharp 3) == "F𝄲3"
pitch_pp :: Pitch -> String
pitch_pp (Pitch n a o) =
    let a' = if a == Natural then "" else [alteration_symbol a]
    in show n ++ a' ++ show o

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
