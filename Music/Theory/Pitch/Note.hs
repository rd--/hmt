-- | Common music notation note and alteration values.
module Music.Theory.Pitch.Note where

import Data.Char {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.List as T {- hmt -}

-- * Note

-- | Enumeration of common music notation note names (@C@ to @B@).
data Note_T = C | D | E | F | G | A | B
              deriving (Eq,Enum,Bounded,Ord,Read,Show)

note_pp :: Note_T -> Char
note_pp = head . show

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

-- | Modal transposition of 'Note_T' value.
--
-- > note_t_transpose C 2 == E
note_t_transpose :: Note_T -> Int -> Note_T
note_t_transpose x n =
    let x' = fromEnum x
        n' = fromEnum (maxBound::Note_T) + 1
    in toEnum ((x' + n) `mod` n')

-- | Parser from 'Char', case insensitive flag.
--
-- > mapMaybe (parse_note True) "CDEFGab" == [C,D,E,F,G,A,B]
parse_note :: Bool -> Char -> Maybe Note_T
parse_note ci c =
    let tbl = zip "CDEFGAB" [C,D,E,F,G,A,B]
    in lookup (if ci then toUpper c else c) tbl

-- * Alteration

-- | Enumeration of common music notation note alterations.
data Alteration_T = DoubleFlat
                  | ThreeQuarterToneFlat | Flat | QuarterToneFlat
                  | Natural
                  | QuarterToneSharp | Sharp | ThreeQuarterToneSharp
                  | DoubleSharp
                    deriving (Eq,Enum,Bounded,Ord,Show)

-- | Generic form.
generic_alteration_to_diff :: Integral i => Alteration_T -> Maybe i
generic_alteration_to_diff a =
    case a of
      DoubleFlat -> Just (-2)
      Flat -> Just (-1)
      Natural -> Just 0
      Sharp -> Just 1
      DoubleSharp -> Just 2
      _ -> Nothing

-- | Transform 'Alteration_T' to semitone alteration.  Returns
-- 'Nothing' for non-semitone alterations.
--
-- > map alteration_to_diff [Flat,QuarterToneSharp] == [Just (-1),Nothing]
alteration_to_diff :: Alteration_T -> Maybe Int
alteration_to_diff = generic_alteration_to_diff

-- | Is 'Alteration_T' 12-ET.
alteration_is_12et :: Alteration_T -> Bool
alteration_is_12et = isJust . alteration_to_diff

-- | Transform 'Alteration_T' to semitone alteration.
--
-- > map alteration_to_diff_err [Flat,Sharp] == [-1,1]
alteration_to_diff_err :: Integral i => Alteration_T -> i
alteration_to_diff_err =
    let err = error "alteration_to_diff: quarter tone"
    in fromMaybe err . generic_alteration_to_diff

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
--
-- > import Data.Ratio
-- > alteration_edit_quarter_tone (-1 % 2) Flat == Just ThreeQuarterToneFlat
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

alteration_symbol_tbl :: [(Alteration_T,Char)]
alteration_symbol_tbl =
    [(DoubleFlat,'𝄫')
    ,(ThreeQuarterToneFlat,'𝄭')
    ,(Flat,'♭')
    ,(QuarterToneFlat,'𝄳')
    ,(Natural,'♮')
    ,(QuarterToneSharp,'𝄲')
    ,(Sharp,'♯')
    ,(ThreeQuarterToneSharp,'𝄰')
    ,(DoubleSharp,'𝄪')]

-- | Unicode has entries for /Musical Symbols/ in the range @U+1D100@
-- through @U+1D1FF@.  The @3/4@ symbols are non-standard, here they
-- correspond to @MUSICAL SYMBOL FLAT DOWN@ and @MUSICAL SYMBOL SHARP
-- UP@.
--
-- > map alteration_symbol [minBound .. maxBound] == "𝄫𝄭♭𝄳♮𝄲♯𝄰𝄪"
alteration_symbol :: Alteration_T -> Char
alteration_symbol a = fromMaybe (error "alteration_symbol") (lookup a alteration_symbol_tbl)

-- | Inverse of 'alteration_symbol'.
--
-- > mapMaybe symbol_to_alteration "♭♮♯" == [Flat,Natural,Sharp]
symbol_to_alteration :: Char -> Maybe Alteration_T
symbol_to_alteration c = T.reverse_lookup c alteration_symbol_tbl

-- | The @ISO@ ASCII spellings for alterations.  Naturals are written
-- as the empty string.
--
-- > mapMaybe alteration_iso_m [Flat .. Sharp] == ["b","","#"]
-- > mapMaybe alteration_iso_m [DoubleFlat,DoubleSharp] == ["bb","x"]
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

-- * Generalised Alteration

-- | Generalised alteration, given as a rational semitone difference
-- and a string representation of the alteration.
type Alteration_T' = (Rational,String)

-- | Transform 'Alteration_T' to 'Alteration_T''.
--
-- > let r = [(-1,"♭"),(0,"♮"),(1,"♯")]
-- > in map alteration_t' [Flat,Natural,Sharp] == r
alteration_t' :: Alteration_T -> Alteration_T'
alteration_t' a = (alteration_to_fdiff a,[alteration_symbol a])

-- | Function to spell a 'PitchClass'.
type Spelling n = n -> (Note_T,Alteration_T)

