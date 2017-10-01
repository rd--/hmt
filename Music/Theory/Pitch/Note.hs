-- | Common music notation note and alteration values.
module Music.Theory.Pitch.Note where

import Data.Char {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.List as T {- hmt -}

-- * Note_T

-- | Enumeration of common music notation note names (@C@ to @B@).
data Note_T = C | D | E | F | G | A | B
              deriving (Eq,Enum,Bounded,Ord,Read,Show)

-- | Note sequence as usually understood, ie. 'C' - 'B'.
note_seq :: [Note_T]
note_seq = [C .. B]

-- | Char variant of 'show'.
note_pp :: Note_T -> Char
note_pp = head . show

-- | Table of 'Note_T' and corresponding pitch-classes.
note_pc_tbl :: Num i => [(Note_T,i)]
note_pc_tbl = zip [C .. B] [0,2,4,5,7,9,11]

-- | Transform 'Note_T' to pitch-class number.
--
-- > map note_to_pc [C,E,G] == [0,4,7]
note_to_pc :: Num i => Note_T -> i
note_to_pc n = fromMaybe (error "note_to_pc") (lookup n note_pc_tbl)

-- | Inverse of 'note_to_pc'.
--
-- > mapMaybe pc_to_note [0,4,7] == [C,E,G]
pc_to_note :: (Eq i,Num i) => i -> Maybe Note_T
pc_to_note i = T.reverse_lookup i note_pc_tbl

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
parse_note_t :: Bool -> Char -> Maybe Note_T
parse_note_t ci c =
    let tbl = zip "CDEFGAB" [C,D,E,F,G,A,B]
    in lookup (if ci then toUpper c else c) tbl

-- | Inclusive set of 'Note_T' within indicated interval.  This is not
-- equal to 'enumFromTo' which is not circular.
--
-- > note_span E B == [E,F,G,A,B]
-- > note_span B D == [B,C,D]
-- > enumFromTo B D == []
note_span :: Note_T -> Note_T -> [Note_T]
note_span n1 n2 =
    let fn x = toEnum (x `mod` 7)
        n1' = fromEnum n1
        n2' = fromEnum n2
        n2'' = if n1' > n2' then n2' + 7 else n2'
    in map fn [n1' .. n2'']

-- * Alteration

-- | Enumeration of common music notation note alterations.
data Alteration_T =
    DoubleFlat
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

-- | Variant of 'symbol_to_alteration' that /also/ recognises @b@ for 'Flat'
-- and @#@ for 'Sharp' and 'x' for double sharp.
symbol_to_alteration_iso :: Char -> Maybe Alteration_T
symbol_to_alteration_iso c =
    case c of
      'b' -> Just Flat
      '#' -> Just Sharp
      'x' -> Just DoubleSharp
      _ -> symbol_to_alteration c

alteration_iso_tbl :: [(Alteration_T,String)]
alteration_iso_tbl =
    [(DoubleFlat,"bb")
    ,(Flat,"b")
    ,(Natural,"")
    ,(Sharp,"#")
    ,(DoubleSharp,"x")]

-- | The @ISO@ ASCII spellings for alterations.  Naturals are written
-- as the empty string.
--
-- > mapMaybe alteration_iso_m [Flat .. Sharp] == ["b","","#"]
-- > mapMaybe alteration_iso_m [DoubleFlat,DoubleSharp] == ["bb","x"]
alteration_iso_m :: Alteration_T -> Maybe String
alteration_iso_m a = lookup a alteration_iso_tbl

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

-- * 12-ET

note_alteration_to_pc :: (Note_T,Alteration_T) -> Maybe Int
note_alteration_to_pc (n,a) =
    let n_pc = note_to_pc n
    in fmap ((`mod` 12) . (+ n_pc)) (alteration_to_diff a)

-- > map note_alteration_to_pc_err [(A,DoubleSharp),(B,Sharp),(C,Flat),(C,DoubleFlat)]
note_alteration_to_pc_err :: (Note_T, Alteration_T) -> Int
note_alteration_to_pc_err = fromMaybe (error "note_alteration_to_pc") . note_alteration_to_pc

-- | Note & alteration sequence in key-signature spelling.
note_alteration_ks :: [(Note_T, Alteration_T)]
note_alteration_ks =
    [(C,Natural),(C,Sharp),(D,Natural),(E,Flat),(E,Natural),(F,Natural)
    ,(F,Sharp),(G,Natural),(A,Flat),(A,Natural),(B,Flat),(B,Natural)]

-- | Table connecting pitch class number with 'note_alteration_ks'.
pc_note_alteration_ks_tbl :: Integral i => [((Note_T,Alteration_T),i)]
pc_note_alteration_ks_tbl = zip note_alteration_ks [0..11]

-- | 'T.reverse_lookup' of 'pc_note_alteration_ks_tbl'.
pc_to_note_alteration_ks :: Integral i => i -> Maybe (Note_T,Alteration_T)
pc_to_note_alteration_ks i = T.reverse_lookup i pc_note_alteration_ks_tbl

-- * Rational Alteration

-- | Alteration given as a rational semitone difference
-- and a string representation of the alteration.
type Alteration_R = (Rational,String)

-- | Transform 'Alteration_T' to 'Alteration_R'.
--
-- > let r = [(-1,"♭"),(0,"♮"),(1,"♯")]
-- > in map alteration_t' [Flat,Natural,Sharp] == r
alteration_r :: Alteration_T -> Alteration_R
alteration_r a = (alteration_to_fdiff a,[alteration_symbol a])
