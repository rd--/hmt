-- | Common music notation note and alteration values.
module Music.Theory.Pitch.Note where

import Data.Char {- base -}
import Data.Maybe {- base -}

import qualified Text.Parsec as P {- parsec -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Parse as T {- hmt -}

-- * Note

-- | Enumeration of common music notation note names (@C@ to @B@).
data Note = C | D | E | F | G | A | B
              deriving (Eq,Enum,Bounded,Ord,Read,Show)

-- | Note sequence as usually understood, ie. 'C' - 'B'.
note_seq :: [Note]
note_seq = [C .. B]

-- | Char variant of 'show'.
note_pp :: Note -> Char
note_pp = head . show

-- | Note name in lilypond syntax (ie. lower case).
note_pp_ly :: Note -> String
note_pp_ly = map toLower . show

-- | Table of 'Note' and corresponding pitch-classes.
note_pc_tbl :: Num i => [(Note,i)]
note_pc_tbl = zip [C .. B] [0,2,4,5,7,9,11]

-- | Transform 'Note' to pitch-class number.
--
-- > map note_to_pc [C,E,G] == [0,4,7]
note_to_pc :: Num i => Note -> i
note_to_pc n = T.lookup_err_msg "note_to_pc" n note_pc_tbl

-- | Inverse of 'note_to_pc'.
--
-- > mapMaybe pc_to_note [0,4,7] == [C,E,G]
pc_to_note :: (Eq i,Num i) => i -> Maybe Note
pc_to_note i = T.reverse_lookup i note_pc_tbl

-- | Modal transposition of 'Note' value.
--
-- > note_t_transpose C 2 == E
note_t_transpose :: Note -> Int -> Note
note_t_transpose x n =
    let x' = fromEnum x
        n' = fromEnum (maxBound::Note) + 1
    in toEnum ((x' + n) `mod` n')

-- | Parser from 'Char', case insensitive flag.
--
-- > mapMaybe (parse_note True) "CDEFGab" == [C,D,E,F,G,A,B]
parse_note_t :: Bool -> Char -> Maybe Note
parse_note_t ci c =
    let tbl = zip "CDEFGAB" [C,D,E,F,G,A,B]
    in lookup (if ci then toUpper c else c) tbl

char_to_note_t :: Bool -> Char -> Note
char_to_note_t ci = fromMaybe (error "char_to_note_t") . parse_note_t ci

-- | Inclusive set of 'Note' within indicated interval.  This is not
-- equal to 'enumFromTo' which is not circular.
--
-- > note_span E B == [E,F,G,A,B]
-- > note_span B D == [B,C,D]
-- > enumFromTo B D == []
note_span :: Note -> Note -> [Note]
note_span n1 n2 =
    let fn x = toEnum (x `mod` 7)
        n1' = fromEnum n1
        n2' = fromEnum n2
        n2'' = if n1' > n2' then n2' + 7 else n2'
    in map fn [n1' .. n2'']

-- * Alteration

-- | Enumeration of common music notation note alterations.
data Alteration =
    DoubleFlat
  | ThreeQuarterToneFlat | Flat | QuarterToneFlat
  | Natural
  | QuarterToneSharp | Sharp | ThreeQuarterToneSharp
  | DoubleSharp
    deriving (Eq,Enum,Bounded,Ord,Show)

-- | Generic form.
generic_alteration_to_diff :: Integral i => Alteration -> Maybe i
generic_alteration_to_diff a =
    case a of
      DoubleFlat -> Just (-2)
      Flat -> Just (-1)
      Natural -> Just 0
      Sharp -> Just 1
      DoubleSharp -> Just 2
      _ -> Nothing

-- | Transform 'Alteration' to semitone alteration.  Returns
-- 'Nothing' for non-semitone alterations.
--
-- > map alteration_to_diff [Flat,QuarterToneSharp] == [Just (-1),Nothing]
alteration_to_diff :: Alteration -> Maybe Int
alteration_to_diff = generic_alteration_to_diff

-- | Is 'Alteration' 12-ET.
alteration_is_12et :: Alteration -> Bool
alteration_is_12et = isJust . alteration_to_diff

-- | Transform 'Alteration' to semitone alteration.
--
-- > map alteration_to_diff_err [Flat,Sharp] == [-1,1]
alteration_to_diff_err :: Integral i => Alteration -> i
alteration_to_diff_err =
    let err = error "alteration_to_diff: quarter tone"
    in fromMaybe err . generic_alteration_to_diff

-- | Transform 'Alteration' to fractional semitone alteration,
-- ie. allow quarter tones.
--
-- > alteration_to_fdiff QuarterToneSharp == 0.5
alteration_to_fdiff :: Fractional n => Alteration -> n
alteration_to_fdiff a =
    case a of
      ThreeQuarterToneFlat -> -1.5
      QuarterToneFlat -> -0.5
      QuarterToneSharp -> 0.5
      ThreeQuarterToneSharp -> 1.5
      _ -> fromInteger (alteration_to_diff_err a)

-- | Transform fractional semitone alteration to 'Alteration',
-- ie. allow quarter tones.
--
-- > map fdiff_to_alteration [-0.5,0.5] == [Just QuarterToneFlat
-- >                                       ,Just QuarterToneSharp]
fdiff_to_alteration :: (Fractional n,Eq n) => n -> Maybe Alteration
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

-- | Raise 'Alteration' by a quarter tone where possible.
--
-- > alteration_raise_quarter_tone Flat == Just QuarterToneFlat
-- > alteration_raise_quarter_tone DoubleSharp == Nothing
alteration_raise_quarter_tone :: Alteration -> Maybe Alteration
alteration_raise_quarter_tone a =
    if a == maxBound then Nothing else Just (toEnum (fromEnum a + 1))

-- | Lower 'Alteration' by a quarter tone where possible.
--
-- > alteration_lower_quarter_tone Sharp == Just QuarterToneSharp
-- > alteration_lower_quarter_tone DoubleFlat == Nothing
alteration_lower_quarter_tone :: Alteration -> Maybe Alteration
alteration_lower_quarter_tone a =
    if a == minBound then Nothing else Just (toEnum (fromEnum a - 1))

-- | Edit 'Alteration' by a quarter tone where possible, @-0.5@
-- lowers, @0@ retains, @0.5@ raises.
--
-- > import Data.Ratio
-- > alteration_edit_quarter_tone (-1 % 2) Flat == Just ThreeQuarterToneFlat
alteration_edit_quarter_tone :: (Fractional n,Eq n) =>
                                n -> Alteration -> Maybe Alteration
alteration_edit_quarter_tone n a =
    case n of
      -0.5 -> alteration_lower_quarter_tone a
      0 -> Just a
      0.5 -> alteration_raise_quarter_tone a
      _ -> Nothing

-- | Simplify 'Alteration' to standard 12ET by deleting quarter tones.
--
-- > Data.List.nub (map alteration_clear_quarter_tone [minBound..maxBound])
alteration_clear_quarter_tone :: Alteration -> Alteration
alteration_clear_quarter_tone x =
    case x of
      ThreeQuarterToneFlat -> Flat
      QuarterToneFlat -> Flat
      QuarterToneSharp -> Sharp
      ThreeQuarterToneSharp -> Sharp
      _ -> x

-- | Table of Unicode characters for alterations.
alteration_symbol_tbl :: [(Alteration,Char)]
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
alteration_symbol :: Alteration -> Char
alteration_symbol a = fromMaybe (error "alteration_symbol") (lookup a alteration_symbol_tbl)

-- | Inverse of 'alteration_symbol'.
--
-- > mapMaybe symbol_to_alteration "♭♮♯" == [Flat,Natural,Sharp]
symbol_to_alteration :: Char -> Maybe Alteration
symbol_to_alteration c = T.reverse_lookup c alteration_symbol_tbl

-- | ISO alteration notation.  When not strict extended to allow ## for x.
symbol_to_alteration_iso :: Bool -> String -> Maybe Alteration
symbol_to_alteration_iso strict txt =
    case txt of
      "bb" -> Just DoubleFlat
      "b" -> Just Flat
      "#" -> Just Sharp
      "##" -> if strict then Nothing else Just DoubleSharp
      "x" -> Just DoubleSharp
      "" -> Just Natural
      _ -> Nothing

symbol_to_alteration_iso_err :: Bool -> String -> Alteration
symbol_to_alteration_iso_err strict =
  fromMaybe (error "symbol_to_alteration_iso") .
  symbol_to_alteration_iso strict

-- | 'symbol_to_alteration' extended to allow single character ISO notations.
symbol_to_alteration_unicode_plus_iso :: Char -> Maybe Alteration
symbol_to_alteration_unicode_plus_iso c =
    case c of
      'b' -> Just Flat
      '#' -> Just Sharp
      'x' -> Just DoubleSharp
      _ -> symbol_to_alteration c

-- | ISO alteration table, strings not characters because of double flat.
alteration_iso_tbl :: [(Alteration,String)]
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
alteration_iso_m :: Alteration -> Maybe String
alteration_iso_m a = lookup a alteration_iso_tbl

-- | The @ISO@ ASCII spellings for alterations.
alteration_iso :: Alteration -> String
alteration_iso =
    let qt = error "alteration_iso: quarter tone"
    in fromMaybe qt . alteration_iso_m

-- | The /Tonhöhe/ ASCII spellings for alterations.
alteration_tonh_tbl :: [(Alteration, String)]
alteration_tonh_tbl =
  [(DoubleFlat,"eses")
  ,(ThreeQuarterToneFlat,"eseh")
  ,(Flat,"es")
  ,(QuarterToneFlat,"eh")
  ,(Natural,"")
  ,(QuarterToneSharp,"ih")
  ,(Sharp,"is")
  ,(ThreeQuarterToneSharp,"isih")
  ,(DoubleSharp,"isis")]

-- | The /Tonhöhe/ ASCII spellings for alterations.
--
-- See <http://www.musiccog.ohio-state.edu/Humdrum/guide04.html> and
-- <http://lilypond.org/doc/v2.16/Documentation/notation/writing-pitches>
--
-- > map alteration_tonh [Flat .. Sharp] == ["es","eh","","ih","is"]
alteration_tonh :: Alteration -> String
alteration_tonh a = T.lookup_err a alteration_tonh_tbl

-- | Inverse of 'alteration_tonh'.
--
-- > mapMaybe tonh_to_alteration ["es","eh","","ih","is"] == [Flat .. Sharp]
tonh_to_alteration :: String -> Maybe Alteration
tonh_to_alteration s = T.reverse_lookup s alteration_tonh_tbl

tonh_to_alteration_err :: String -> Alteration
tonh_to_alteration_err = fromMaybe (error "tonh_to_alteration") . tonh_to_alteration

-- * 12-ET

-- | Note and alteration to pitch-class, or not.
note_alteration_to_pc :: (Note,Alteration) -> Maybe Int
note_alteration_to_pc (n,a) =
    let n_pc = note_to_pc n
    in fmap ((`mod` 12) . (+ n_pc)) (alteration_to_diff a)

-- | Error variant.
--
-- > map note_alteration_to_pc_err [(A,DoubleSharp),(B,Sharp),(C,Flat),(C,DoubleFlat)]
note_alteration_to_pc_err :: (Note, Alteration) -> Int
note_alteration_to_pc_err = fromMaybe (error "note_alteration_to_pc") . note_alteration_to_pc

-- | Note & alteration sequence in key-signature spelling.
note_alteration_ks :: [(Note, Alteration)]
note_alteration_ks =
    [(C,Natural),(C,Sharp),(D,Natural),(E,Flat),(E,Natural),(F,Natural)
    ,(F,Sharp),(G,Natural),(A,Flat),(A,Natural),(B,Flat),(B,Natural)]

-- | Table connecting pitch class number with 'note_alteration_ks'.
pc_note_alteration_ks_tbl :: Integral i => [((Note,Alteration),i)]
pc_note_alteration_ks_tbl = zip note_alteration_ks [0..11]

-- | 'T.reverse_lookup' of 'pc_note_alteration_ks_tbl'.
pc_to_note_alteration_ks :: Integral i => i -> Maybe (Note,Alteration)
pc_to_note_alteration_ks i = T.reverse_lookup i pc_note_alteration_ks_tbl

-- * Rational Alteration

-- | Alteration given as a rational semitone difference
-- and a string representation of the alteration.
type Alteration_R = (Rational,String)

-- | Transform 'Alteration' to 'Alteration_R'.
--
-- > let r = [(-1,"♭"),(0,"♮"),(1,"♯")]
-- > map alteration_r [Flat,Natural,Sharp] == r
alteration_r :: Alteration -> Alteration_R
alteration_r a = (alteration_to_fdiff a,[alteration_symbol a])

-- * Parsers

-- | Parser for ISO note name, upper case.
--
-- > map (T.run_parser_error p_note_t . return) "ABCDEFG"
p_note_t :: T.P Note
p_note_t = fmap (char_to_note_t False) (P.oneOf "ABCDEFG")

-- | Note name in lower case (not ISO)
p_note_t_lc :: T.P Note
p_note_t_lc = fmap (char_to_note_t True) (P.oneOf "abcdefg")

-- | Case-insensitive note name (not ISO).
p_note_t_ci :: T.P Note
p_note_t_ci = fmap (char_to_note_t True) (P.oneOf "abcdefgABCDEFG")

-- | Parser for ISO alteration name.
--
-- > map (T.run_parser_error p_alteration_t_iso) (words "bb b # x ##")
p_alteration_t_iso :: Bool -> T.P Alteration
p_alteration_t_iso strict = fmap (symbol_to_alteration_iso_err strict) (P.many (P.oneOf "b#x"))

-- > map (T.run_parser_error p_alteration_t_tonh) ["eses","es","is","isis"]
p_alteration_t_tonh :: T.P Alteration
p_alteration_t_tonh = fmap tonh_to_alteration_err (P.many1 (P.oneOf "ehis"))

-- > map (T.run_parser_error p_note_alteration_ly) ["c","ees","fis","aeses"]
p_note_alteration_ly :: T.P (Note,Maybe Alteration)
p_note_alteration_ly = do
  n <- p_note_t_lc
  a <- P.optionMaybe p_alteration_t_tonh
  return (n,a)
