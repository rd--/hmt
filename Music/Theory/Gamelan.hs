-- | Gamelan instruments and pitch structures.
module Music.Theory.Gamelan where

import Data.Char {- base -}
import Data.Function {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}
import Text.Printf {- base -}

import qualified Music.Theory.Enum as Enum {- hmt-base -}
import qualified Music.Theory.List as List {- hmt-base -}

import qualified Music.Theory.Clef as Clef {- hmt -}
import qualified Music.Theory.Pitch as Pitch {- hmt -}
import qualified Music.Theory.Tuning as Tuning {- hmt -}
import qualified Music.Theory.Tuning.Et as Et {- hmt-diagrams -}

-- | 'fromJust' with error message.
fromJust_err :: String -> Maybe a -> a
fromJust_err err = fromMaybe (error err)

-- | 'approxRational' of 0.01.
near_rat :: Double -> Rational
near_rat = flip approxRational 0.01

-- * Gamelan

-- | Enumeration of gamelan instrument families.
data Instrument_Family
    = Bonang
    | Gambang
    | Gender
    | Gong
    | Saron
      deriving (Enum,Bounded,Eq,Ord,Show,Read)

-- | Universe
instrument_family_set :: [Instrument_Family]
instrument_family_set = Enum.enum_univ

-- | Enumeration of Gamelan instruments.
data Instrument_Name
    = Bonang_Barung -- ^ Bonang Barung (horizontal gong, middle)
    | Bonang_Panerus -- ^ Bonang Panerus (horizontal gong, high)
    | Gambang_Kayu -- ^ Gambang Kayu (wooden key&resonator)
    | Gender_Barung -- ^ Gender Barung (key&resonator, middle)
    | Gender_Panerus -- ^ Gender Panembung (key&resonator, high)
    | Gender_Panembung -- ^ Gender Panembung, Slenthem (key&resonator, low)
    | Gong_Ageng -- ^ Gong Ageng (hanging gong, low)
    | Gong_Suwukan -- ^ Gong Suwukan (hanging gong, middle)
    | Kempul -- ^ Kempul (hanging gong, middle)
    | Kempyang -- ^ Kempyang (horizontal gong, high)
    | Kenong -- ^ Kenong (horizontal gong, low)
    | Ketuk -- ^ Ketuk, Kethuk (horizontal gong, middle)
    | Saron_Barung -- ^ Saron Barung, Saron (key, middle)
    | Saron_Demung -- ^ Saron Demung, Demung (key, low)
    | Saron_Panerus -- ^ Saron Panerus, Peking (key, high)
      deriving (Enum,Bounded,Eq,Ord,Show,Read)

instrument_family :: Instrument_Name -> Instrument_Family
instrument_family nm =
    case nm of
      Bonang_Barung -> Bonang
      Bonang_Panerus -> Bonang
      Gambang_Kayu -> Gambang
      Gender_Barung -> Gender
      Gender_Panerus -> Gender
      Gender_Panembung -> Gender
      Gong_Ageng -> Gong
      Gong_Suwukan -> Gong
      Kempul -> Gong
      Kempyang -> Gong
      Kenong -> Gong
      Ketuk -> Gong
      Saron_Barung -> Saron
      Saron_Demung -> Saron
      Saron_Panerus -> Saron

instrument_name_pp :: Instrument_Name -> String
instrument_name_pp =
    let f c = if c == '_' then ' ' else c
    in map f . show

-- | 'Clef' appropriate for 'Instrument_Name'.
instrument_name_clef :: Integral i => Instrument_Name -> Clef.Clef i
instrument_name_clef nm =
    case nm of
      Bonang_Barung -> Clef.Clef Clef.Treble 0
      Bonang_Panerus -> Clef.Clef Clef.Treble 1
      Gambang_Kayu -> Clef.Clef Clef.Treble 0
      Gender_Barung -> Clef.Clef Clef.Treble 0
      Gender_Panerus -> Clef.Clef Clef.Treble 1
      Gender_Panembung -> Clef.Clef Clef.Bass 0
      Gong_Ageng -> Clef.Clef Clef.Bass 0
      Gong_Suwukan -> Clef.Clef Clef.Bass 0
      Kempul -> Clef.Clef Clef.Bass 0
      Kempyang -> Clef.Clef Clef.Treble 1
      Kenong -> Clef.Clef Clef.Treble 0
      Ketuk -> Clef.Clef Clef.Alto 0
      Saron_Barung -> Clef.Clef Clef.Treble 0
      Saron_Demung -> Clef.Clef Clef.Treble 0
      Saron_Panerus -> Clef.Clef Clef.Treble 1

instrument_name_clef_plain :: Integral i => Instrument_Name -> Clef.Clef i
instrument_name_clef_plain = Clef.clef_zero . instrument_name_clef

-- | Enumeration of Gamelan scales.
data Scale = Pelog | Slendro deriving (Enum,Eq,Ord,Show,Read)

-- | Octaves are zero-indexed and may be negative.
type Octave = Integer

-- | Degrees are one-indexed.
type Degree = Integer

-- | Frequency in hertz.
type Frequency = Double

-- | A text annotation.
type Annotation = String

-- | 'Octave' and 'Degree'.
data Pitch = Pitch {pitch_octave :: Octave
                   ,pitch_degree :: Degree}
             deriving (Eq,Ord,Show)

{- | Octaves are written as repeated @-@ or @+@, degrees are printed ordinarily.

>>> map pitch_pp_ascii (zipWith Pitch [-2 .. 2] [1 .. 5])
["--1","-2","3","+4","++5"]
-}
pitch_pp_ascii :: Pitch -> String
pitch_pp_ascii (Pitch o d) =
    let d' = intToDigit (fromIntegral d)
        o' = if o < 0
             then genericReplicate (abs o) '-'
             else genericReplicate o '+'
    in o' ++ [d']

pitch_pp_duple :: Pitch -> String
pitch_pp_duple (Pitch o d) = printf "(%d,%d)" o d

-- | 'Scale' and 'Pitch'.
data Note = Note {note_scale :: Scale
                 ,note_pitch :: Pitch}
             deriving (Eq,Show)

-- | 'pitch_degree' of 'note_pitch'.
note_degree :: Note -> Degree
note_degree = pitch_degree . note_pitch

-- | It is an error to compare notes from different scales.
note_compare :: Note -> Note -> Ordering
note_compare (Note s1 p1) (Note s2 p2) =
  if s1 /= s2
  then error "note_compare?"
  else compare p1 p2

-- | Orderable if scales are equal.
instance Ord Note where compare = note_compare

-- | Ascending sequence of 'Note' for 'Scale' from /p1/ to /p2/ inclusive.
note_range_elem :: Scale -> Pitch -> Pitch -> [Note]
note_range_elem scl p1@(Pitch o1 _d1) p2@(Pitch o2 _d2) =
  let univ = [Note scl (Pitch o d) | o <- [o1 .. o2], d <- scale_degrees scl]
  in filter (\n -> note_pitch n >= p1 && note_pitch n <= p2) univ

{- | Ascending sequence of 'Note' from /n1/ to /n2/ inclusive.

>>> map (pitch_degree . note_pitch) (note_gamut_elem (Note Slendro (Pitch 0 5)) (Note Slendro (Pitch 1 2)))
[5,6,1,2]
-}
note_gamut_elem :: Note -> Note -> [Note]
note_gamut_elem (Note s1 p1) (Note s2 p2) =
  if s1 /= s2
  then error "note_gamut_elem?"
  else note_range_elem s1 p1 p2

data Tone t = Tone {tone_instrument_name :: Instrument_Name
                   ,tone_note :: Maybe Note
                   ,tone_frequency :: Maybe Frequency
                   ,tone_annotation :: Maybe t}
              deriving (Eq,Show)

tone_frequency_err :: Tone t -> Frequency
tone_frequency_err = fromJust_err "tone_frequency" . tone_frequency

-- | Orderable if frequency is given.
instance Eq t => Ord (Tone t) where compare = tone_compare_frequency

-- | Constructor for 'Tone' without /frequency/ or /annotation/.
plain_tone :: Instrument_Name -> Scale -> Octave -> Degree -> Tone t
plain_tone nm sc o d = Tone nm (Just (Note sc (Pitch o d))) Nothing Nothing

-- | Tones are considered /equivalent/ if they have the same
-- 'Instrument_Name' and 'Note'.
tone_equivalent :: Tone t -> Tone t -> Bool
tone_equivalent p q =
    let Tone nm nt _ _ = p
        Tone nm' nt' _ _ = q
    in nm == nm' && nt == nt'

tone_24et_pitch :: Tone t -> Maybe Pitch.Pitch
tone_24et_pitch =
    let f i = let (_,pt,_,_,_) = Et.nearest_24et_tone_k0 (69,440) i in pt
    in fmap f . tone_frequency

tone_24et_pitch' :: Tone t -> Pitch.Pitch
tone_24et_pitch' = fromJust_err "tone_24et_pitch" . tone_24et_pitch

tone_24et_pitch_detune :: Tone t -> Maybe Et.Pitch_Detune
tone_24et_pitch_detune = fmap (Et.nearest_pitch_detune_24et_k0 (69,440)) . tone_frequency

tone_24et_pitch_detune' :: Tone t -> Et.Pitch_Detune
tone_24et_pitch_detune' = fromJust_err "tone_24et_pitch_detune" . tone_24et_pitch_detune

tone_fmidi :: Tone t -> Double
tone_fmidi = Pitch.cps_to_fmidi . tone_frequency_err

-- | Fractional (rational) 24-et midi note number of 'Tone'.
tone_24et_fmidi :: Tone t -> Rational
tone_24et_fmidi = near_rat . Pitch.pitch_to_fmidi . tone_24et_pitch'

tone_12et_pitch :: Tone t -> Maybe Pitch.Pitch
tone_12et_pitch =
    let f i = let (_,pt,_,_,_) = Et.nearest_12et_tone_k0 (69,440) i in pt
    in fmap f . tone_frequency

tone_12et_pitch' :: Tone t -> Pitch.Pitch
tone_12et_pitch' = fromJust_err "tone_12et_pitch" . tone_12et_pitch

tone_12et_pitch_detune :: Tone t -> Maybe Et.Pitch_Detune
tone_12et_pitch_detune = fmap (Et.nearest_pitch_detune_12et_k0 (69,440)) . tone_frequency

tone_12et_pitch_detune' :: Tone t -> Et.Pitch_Detune
tone_12et_pitch_detune' = fromJust_err "tone_12et_pitch_detune" . tone_12et_pitch_detune

-- | Fractional (rational) 24-et midi note number of 'Tone'.
tone_12et_fmidi :: Tone t -> Rational
tone_12et_fmidi = near_rat . Pitch.pitch_to_fmidi . tone_12et_pitch'

tone_family :: Tone t -> Instrument_Family
tone_family = instrument_family . tone_instrument_name

tone_in_family :: Instrument_Family -> Tone t -> Bool
tone_in_family c t = tone_family t == c

select_tones :: Instrument_Family -> [Tone t] -> [Maybe (Tone t)]
select_tones c =
    let f t = if tone_family t == c then Just t else Nothing
    in map f

-- | Specify subset as list of families and scales.
type Tone_Subset = ([Instrument_Family],[Scale])

-- | Extract subset of 'Tone_Set'.
tone_subset :: Tone_Subset -> Tone_Set t -> Tone_Set t
tone_subset (fm,sc) =
    let f t = tone_family t `elem` fm &&
              fromJust_err "tone_subset" (tone_scale t) `elem` sc
    in filter f

data Instrument = Instrument {instrument_name :: Instrument_Name
                             ,instrument_scale :: Maybe Scale
                             ,instrument_pitches :: Maybe [Pitch]
                             ,instrument_frequencies :: Maybe [Frequency]}
                  deriving (Eq,Show)

type Tone_Set t = [Tone t]
type Tone_Group t = [Tone_Set t]
type Gamelan = [Instrument]

tone_scale :: Tone t -> Maybe Scale
tone_scale = fmap note_scale . tone_note

tone_pitch :: Tone t -> Maybe Pitch
tone_pitch = fmap note_pitch . tone_note

tone_degree :: Tone t -> Maybe Degree
tone_degree = fmap pitch_degree . tone_pitch

tone_degree' :: Tone t -> Degree
tone_degree' = fromJust_err "tone_degree" . tone_degree

tone_octave :: Tone t -> Maybe Octave
tone_octave = fmap pitch_octave . tone_pitch

tone_class :: Tone t -> (Instrument_Name,Maybe Scale)
tone_class t = (tone_instrument_name t,tone_scale t)

instrument_class :: Instrument -> (Instrument_Name,Maybe Scale)
instrument_class i = (instrument_name i,instrument_scale i)

tone_class_p :: (Instrument_Name, Scale) -> Tone t -> Bool
tone_class_p (nm,sc) t =
    tone_instrument_name t == nm &&
    tone_scale t == Just sc

tone_family_class_p :: (Instrument_Family,Scale) -> Tone t -> Bool
tone_family_class_p (fm,sc) t =
    instrument_family (tone_instrument_name t) == fm &&
    tone_scale t == Just sc

-- | Given a 'Tone_Set', find those 'Tone's that are within 'Pitch.Cents' of 'Frequency'.
tone_set_near_frequency :: Tone_Set t -> Tuning.Cents -> Frequency -> Tone_Set t
tone_set_near_frequency t k n =
    let near i = abs (Tuning.cps_difference_cents i n) <= k
        near_t i = maybe False near (tone_frequency i)
    in filter near_t t

-- | Compare 'Tone's by frequency.  'Tone's without frequency compare
-- as if at frequency @0@.
tone_compare_frequency :: Tone t -> Tone t -> Ordering
tone_compare_frequency = compare `on` (fromMaybe 0 . tone_frequency)

-- | If all /f/ of /a/ are 'Just' /b/, then 'Just' /[b]/, else
-- 'Nothing'.
map_maybe_uniform :: (a -> Maybe b) -> [a] -> Maybe [b]
map_maybe_uniform f x =
    let x' = map f x
    in if any isNothing x' then Nothing else Just (catMaybes x')

instrument :: Tone_Set t -> Instrument
instrument c =
    let sf = fmap note_scale . tone_note
        pf = fmap note_pitch . tone_note
        pm = map_maybe_uniform pf c
        fm = map_maybe_uniform tone_frequency c
        (p,f) = case (pm,fm) of
                  (Just i,Just j) -> let (i',j') = unzip (sort (zip i j))
                                     in (Just i',Just j')
                  _ -> (pm,fm)
    in case c of
         t:_ -> Instrument (tone_instrument_name t) (sf t) p f
         [] -> undefined

instruments :: Tone_Set t -> [Instrument]
instruments c =
    let c' = sortBy (compare `on` tone_instrument_name) c
        c'' = groupBy ((==) `on` tone_class) c'
    in map instrument c''

instrument_gamut :: Instrument -> Maybe (Pitch,Pitch)
instrument_gamut =
    let f p = (List.head_err p,last p)
    in fmap f . instrument_pitches

{- | Pelog has seven degrees, numbered one to seven.
Slendro has five degrees, numbered one to six excluding four.

>>> map scale_degrees [Pelog,Slendro]
[[1,2,3,4,5,6,7],[1,2,3,5,6]]
-}
scale_degrees :: Scale -> [Degree]
scale_degrees s =
    case s of
      Pelog -> [1..7]
      Slendro -> [1,2,3,5,6]

{- | Zero based index of scale degree, or Nothing.

>>> degree_index Slendro 4
Nothing

>>> degree_index Pelog 4
Just 3
-}
degree_index :: Scale -> Degree -> Maybe Int
degree_index s d = elemIndex d (scale_degrees s)

-- * Tone set

tone_set_gamut :: Tone_Set t -> Maybe (Pitch,Pitch)
tone_set_gamut g =
    case mapMaybe (fmap note_pitch . tone_note) g of
      [] -> Nothing
      p -> Just (minimum p,maximum p)

tone_set_instrument :: Tone_Set t -> (Instrument_Name,Maybe Scale) -> Tone_Set t
tone_set_instrument db (i,s) =
    let f t = tone_class t == (i,s)
    in filter f db
