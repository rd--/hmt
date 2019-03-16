module Music.Theory.Gamelan where

import Data.Char {- base -}
import Data.Function {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}
import Text.Printf {- base -}

import qualified Music.Theory.Clef as T {- hmt -}
import qualified Music.Theory.Enum as T {- hmt -}
import qualified Music.Theory.Pitch as T {- hmt -}
import qualified Music.Theory.Tuning as T {- hmt -}
import qualified Music.Theory.Tuning.ET as T {- hmt-diagrams -}

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
instrument_family_set = T.enum_univ

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
instrument_name_clef :: Integral i => Instrument_Name -> T.Clef i
instrument_name_clef nm =
    case nm of
      Bonang_Barung -> T.Clef T.Treble 0
      Bonang_Panerus -> T.Clef T.Treble 1
      Gambang_Kayu -> T.Clef T.Treble 0
      Gender_Barung -> T.Clef T.Treble 0
      Gender_Panerus -> T.Clef T.Treble 1
      Gender_Panembung -> T.Clef T.Bass 0
      Gong_Ageng -> T.Clef T.Bass 0
      Gong_Suwukan -> T.Clef T.Bass 0
      Kempul -> T.Clef T.Bass 0
      Kempyang -> T.Clef T.Treble 1
      Kenong -> T.Clef T.Treble 0
      Ketuk -> T.Clef T.Alto 0
      Saron_Barung -> T.Clef T.Treble 0
      Saron_Demung -> T.Clef T.Treble 0
      Saron_Panerus -> T.Clef T.Treble 1

instrument_name_clef_plain :: Integral i => Instrument_Name -> T.Clef i
instrument_name_clef_plain = T.clef_zero . instrument_name_clef

-- | Enumeration of Gamelan scales.
data Scale = Pelog | Slendro deriving (Enum,Eq,Ord,Show,Read)

type Octave = Integer
type Degree = Integer
type Frequency = Double
type Annotation = String

data Pitch = Pitch {pitch_octave :: Octave
                   ,pitch_degree :: Degree}
             deriving (Eq,Ord,Show)

pitch_pp_ascii :: Pitch -> String
pitch_pp_ascii (Pitch o d) =
    let d' = intToDigit (fromIntegral d)
        o' = if o < 0
             then genericReplicate (abs o) '-'
             else genericReplicate o '+'
    in o' ++ [d']

pitch_pp_duple :: Pitch -> String
pitch_pp_duple (Pitch o d) = printf "(%d,%d)" o d

data Note = Note {note_scale :: Scale
                 ,note_pitch :: Pitch}
             deriving (Eq,Ord,Show)

note_degree :: Note -> Degree
note_degree = pitch_degree . note_pitch

data Tone = Tone {tone_instrument_name :: Instrument_Name
                 ,tone_note :: Maybe Note
                 ,tone_frequency :: Maybe Frequency
                 ,tone_annotation :: Maybe Annotation}
             deriving (Eq,Show)

tone_frequency_err :: Tone -> Frequency
tone_frequency_err = fromJust_err "tone_frequency" . tone_frequency

-- | Orderable if frequency is given.
instance Ord Tone where compare = tone_compare_frequency

-- | Constructor for 'Tone' without /frequency/ or /annotation/.
plain_tone :: Instrument_Name -> Scale -> Octave -> Degree -> Tone
plain_tone nm sc o d = Tone nm (Just (Note sc (Pitch o d))) Nothing Nothing

-- | Tones are considered /equivalent/ if they have the same
-- 'Instrument_Name' and 'Note'.
tone_equivalent :: Tone -> Tone -> Bool
tone_equivalent p q =
    let Tone nm nt _ _ = p
        Tone nm' nt' _ _ = q
    in nm == nm' && nt == nt'

tone_24et_pitch :: Tone -> Maybe T.Pitch
tone_24et_pitch =
    let f i = let (_,pt,_,_,_) = T.nearest_24et_tone i in pt
    in fmap f . tone_frequency

tone_24et_pitch' :: Tone -> T.Pitch
tone_24et_pitch' = fromJust_err "tone_24et_pitch" . tone_24et_pitch

tone_24et_pitch_detune :: Tone -> Maybe T.Pitch_Detune
tone_24et_pitch_detune = fmap T.nearest_pitch_detune_24et . tone_frequency

tone_24et_pitch_detune' :: Tone -> T.Pitch_Detune
tone_24et_pitch_detune' = fromJust_err "tone_24et_pitch_detune" . tone_24et_pitch_detune

tone_fmidi :: Tone -> Double
tone_fmidi = T.cps_to_fmidi . tone_frequency_err

-- | Fractional (rational) 24-et midi note number of 'Tone'.
tone_24et_fmidi :: Tone -> Rational
tone_24et_fmidi = near_rat . T.pitch_to_fmidi . tone_24et_pitch'

tone_12et_pitch :: Tone -> Maybe T.Pitch
tone_12et_pitch =
    let f i = let (_,pt,_,_,_) = T.nearest_12et_tone i in pt
    in fmap f . tone_frequency

tone_12et_pitch' :: Tone -> T.Pitch
tone_12et_pitch' = fromJust_err "tone_12et_pitch" . tone_12et_pitch

tone_12et_pitch_detune :: Tone -> Maybe T.Pitch_Detune
tone_12et_pitch_detune = fmap T.nearest_pitch_detune_12et . tone_frequency

tone_12et_pitch_detune' :: Tone -> T.Pitch_Detune
tone_12et_pitch_detune' = fromJust_err "tone_12et_pitch_detune" . tone_12et_pitch_detune

-- | Fractional (rational) 24-et midi note number of 'Tone'.
tone_12et_fmidi :: Tone -> Rational
tone_12et_fmidi = near_rat . T.pitch_to_fmidi . tone_12et_pitch'

tone_family :: Tone -> Instrument_Family
tone_family = instrument_family . tone_instrument_name

tone_in_family :: Instrument_Family -> Tone -> Bool
tone_in_family c t = tone_family t == c

select_tones :: Instrument_Family -> [Tone] -> [Maybe Tone]
select_tones c =
    let f t = if tone_family t == c then Just t else Nothing
    in map f

-- | Specify subset as list of families and scales.
type Tone_Subset = ([Instrument_Family],[Scale])

-- | Extract subset of 'Tone_Set'.
tone_subset :: Tone_Subset -> Tone_Set -> Tone_Set
tone_subset (fm,sc) =
    let f t = tone_family t `elem` fm &&
              fromJust_err "tone_subset" (tone_scale t) `elem` sc
    in filter f

data Instrument = Instrument {instrument_name :: Instrument_Name
                             ,instrument_scale :: Maybe Scale
                             ,instrument_pitches :: Maybe [Pitch]
                             ,instrument_frequencies :: Maybe [Frequency]}
                  deriving (Eq,Show)

type Tone_Set = [Tone]
type Tone_Group = [Tone_Set]
type Gamelan = [Instrument]

tone_scale :: Tone -> Maybe Scale
tone_scale = fmap note_scale . tone_note

tone_pitch :: Tone -> Maybe Pitch
tone_pitch = fmap note_pitch . tone_note

tone_degree :: Tone -> Maybe Degree
tone_degree = fmap pitch_degree . tone_pitch

tone_degree' :: Tone -> Degree
tone_degree' = fromJust_err "tone_degree" . tone_degree

tone_octave :: Tone -> Maybe Octave
tone_octave = fmap pitch_octave . tone_pitch

tone_class :: Tone -> (Instrument_Name,Maybe Scale)
tone_class t = (tone_instrument_name t,tone_scale t)

instrument_class :: Instrument -> (Instrument_Name,Maybe Scale)
instrument_class i = (instrument_name i,instrument_scale i)

tone_class_p :: (Instrument_Name, Scale) -> Tone -> Bool
tone_class_p (nm,sc) t =
    tone_instrument_name t == nm &&
    tone_scale t == Just sc

tone_family_class_p :: (Instrument_Family,Scale) -> Tone -> Bool
tone_family_class_p (fm,sc) t =
    instrument_family (tone_instrument_name t) == fm &&
    tone_scale t == Just sc

-- | Given a 'Tone_Set', find those 'Tone's that are within 'T.Cents' of 'Frequency'.
tone_set_near_frequency :: Tone_Set -> T.Cents -> Frequency -> Tone_Set
tone_set_near_frequency t k n =
    let near i = abs (T.cps_difference_cents i n) <= k
        near_t i = maybe False near (tone_frequency i)
    in filter near_t t

-- | Compare 'Tone's by frequency.  'Tone's without frequency compare
-- as if at frequency @0@.
tone_compare_frequency :: Tone -> Tone -> Ordering
tone_compare_frequency = compare `on` (maybe 0 id . tone_frequency)

-- | If all /f/ of /a/ are 'Just' /b/, then 'Just' /[b]/, else
-- 'Nothing'.
map_maybe_uniform :: (a -> Maybe b) -> [a] -> Maybe [b]
map_maybe_uniform f x =
    let x' = map f x
    in if any isNothing x' then Nothing else Just (catMaybes x')

instrument :: Tone_Set -> Instrument
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

instruments :: Tone_Set -> [Instrument]
instruments c =
    let c' = sortBy (compare `on` tone_instrument_name) c
        c'' = groupBy ((==) `on` tone_class) c'
    in map instrument c''

instrument_gamut :: Instrument -> Maybe (Pitch,Pitch)
instrument_gamut =
    let f p = (head p,last p)
    in fmap f . instrument_pitches

scale_degrees :: Scale -> [Degree]
scale_degrees s =
    case s of
      Pelog -> [1..7]
      Slendro -> [1,2,3,5,6]

-- > degree_index Slendro 4 == Nothing
-- > degree_index Pelog 4 == Just 3
degree_index :: Scale -> Degree -> Maybe Int
degree_index s d = findIndex (== d) (scale_degrees s)

-- * Tone set

tone_set_gamut :: Tone_Set -> Maybe (Pitch,Pitch)
tone_set_gamut g =
    case mapMaybe (fmap note_pitch . tone_note) g of
      [] -> Nothing
      p -> Just (minimum p,maximum p)

tone_set_instrument :: Tone_Set -> (Instrument_Name,Maybe Scale) -> Tone_Set
tone_set_instrument db (i,s) =
    let f t = tone_class t == (i,s)
    in filter f db
