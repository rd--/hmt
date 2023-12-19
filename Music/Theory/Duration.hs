-- | Common music notation duration model.
module Music.Theory.Duration where

import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}
import Text.Printf {- base -}

import qualified Music.Theory.List as List {- hmt-base -}
import qualified Music.Theory.Ord as Ord {- hmt-base -}
import qualified Music.Theory.Show as Show {- hmt-base -}

type Division = Integer
type Dots = Int

-- | Common music notation durational model
data Duration = Duration
  { division :: Division
  -- ^ division of whole note
  , dots :: Int
  -- ^ number of dots
  , multiplier :: Rational
  -- ^ tuplet modifier
  }
  deriving (Eq, Show)

{- | Prettier printer for Duration

>>> duration_pp (Duration 4 0 (1/3))
"(Duration 4 0 (1/3))"
-}
duration_pp :: Duration -> String
duration_pp (Duration d t m) =
  printf "(Duration %d %d (%s))" d t (Show.rational_pp m)

{-
instance Show Duration where show = duration_pp
-}

-- | Are multipliers equal?
duration_meq :: Duration -> Duration -> Bool
duration_meq p q = multiplier p == multiplier q

-- | Is multiplier the identity (ie. @1@)?
duration_m1 :: Duration -> Bool
duration_m1 = (== 1) . multiplier

-- | Compare durations with equal multipliers.
duration_compare_meq :: Duration -> Duration -> Maybe Ordering
duration_compare_meq y0 y1 =
  let (Duration x0 n0 m0) = y0
      (Duration x1 n1 m1) = y1
  in if y0 == y1
      then Just EQ
      else
        if m0 /= m1
          then Nothing
          else
            Just
              ( if x0 == x1
                  then compare n0 n1
                  else compare x1 x0
              )

-- | Erroring variant of 'duration_compare_meq'.
duration_compare_meq_err :: Duration -> Duration -> Ordering
duration_compare_meq_err p =
  let err = error "duration_compare_meq_err: non-equal multipliers"
  in fromMaybe err . duration_compare_meq p

-- | 'Ord' instance in terms of 'duration_compare_meq_err'.
instance Ord Duration where
  compare = duration_compare_meq_err

-- | True if neither duration is dotted.
no_dots :: (Duration, Duration) -> Bool
no_dots (x0, x1) = dots x0 == 0 && dots x1 == 0

-- | Sum undotted divisions, input is required to be sorted.
sum_dur_undotted :: Rational -> (Division, Division) -> Maybe Duration
sum_dur_undotted m (x0, x1)
  | x0 == x1 = Just (Duration (x0 `div` 2) 0 m)
  | x0 == x1 * 2 = Just (Duration x1 1 m)
  | otherwise = Nothing

{- | Sum dotted divisions, input is required to be sorted.

>>> sum_dur_dotted 1 (4,1,4,1) == Just (Duration 2 1 1)
True

>>> sum_dur_dotted 1 (4,0,2,1) == Just (Duration 1 0 1)
True

>>> sum_dur_dotted 1 (8,1,4,0) == Just (Duration 4 2 1)
True

>>> sum_dur_dotted 1 (16,0,4,2) == Just (Duration 2 0 1)
True
-}
sum_dur_dotted :: Rational -> (Division, Dots, Division, Dots) -> Maybe Duration
sum_dur_dotted m (x0, n0, x1, n1)
  | x0 == x1
      && n0 == 1
      && n1 == 1 =
      Just (Duration (x1 `div` 2) 1 m)
  | x0 == x1 * 2
      && n0 == 0
      && n1 == 1 =
      Just (Duration (x1 `div` 2) 0 m)
  | x0 == x1 * 4
      && n0 == 0
      && n1 == 2 =
      Just (Duration (x1 `div` 2) 0 m)
  | x0 == x1 * 2
      && n0 == 1
      && n1 == 0 =
      Just (Duration x1 2 m)
  | otherwise = Nothing

{- | Sum durations.  Not all durations can be summed, and the present
     algorithm is not exhaustive.

> import Music.Theory.Duration
> import Music.Theory.Duration.Name
> sum_dur quarter_note eighth_note == Just dotted_quarter_note
> sum_dur dotted_quarter_note eighth_note == Just half_note
> sum_dur quarter_note dotted_eighth_note == Just double_dotted_quarter_note
-}
sum_dur :: Duration -> Duration -> Maybe Duration
sum_dur y0 y1 =
  let (m0, m1) = (multiplier y0, multiplier y1)
      f (x0, x1) =
        if m0 /= m1
          then Nothing -- cannot sum durations with non-equal multipliers
          else
            if no_dots (x0, x1)
              then sum_dur_undotted m0 (division x0, division x1)
              else
                sum_dur_dotted
                  m0
                  ( division x0
                  , dots x0
                  , division x1
                  , dots x1
                  )
  in Ord.sort_pair_m duration_compare_meq (y0, y1) >>= f

-- | Erroring variant of 'sum_dur'.
sum_dur_err :: Duration -> Duration -> Duration
sum_dur_err y0 y1 =
  let y2 = sum_dur y0 y1
      err = error ("sum_dur': " ++ show (y0, y1))
  in fromMaybe err y2

{- | Standard divisions (from 1 to 256).
MusicXml allows 0 for breve and -1 for long.
Negative divisors can represent any number of longer durations, -2 be a breve, -4 a long, -8 a maximus, &etc.
-}
divisions_std_set :: [Division]
divisions_std_set = [1, 2, 4, 8, 16, 32, 64, 128, 256]

divisions_musicxml_set :: [Division]
divisions_musicxml_set = -1 : 0 : divisions_std_set

-- | Durations set derived from 'divisions_std_set' with up to /k/ dots.  Multiplier of @1@.
duration_set :: Dots -> [Duration]
duration_set k = [Duration dv dt 1 | dv <- divisions_std_set, dt <- [0 .. k]]

-- | Table of number of beams at notated division.
beam_count_tbl :: [(Division, Int)]
beam_count_tbl = zip divisions_musicxml_set [0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6]

{- | Lookup 'beam_count_tbl'.

>>> whole_note_division_to_beam_count 32
Just 3
-}
whole_note_division_to_beam_count :: Division -> Maybe Int
whole_note_division_to_beam_count x = lookup x beam_count_tbl

{- | Calculate number of beams at 'Duration'.

>>> map duration_beam_count [Duration 2 0 1,Duration 16 0 1]
[0,2]
-}
duration_beam_count :: Duration -> Int
duration_beam_count (Duration x _ _) =
  let err = error "duration_beam_count"
      bc = whole_note_division_to_beam_count x
  in fromMaybe err bc

-- * MusicXml

-- | Table giving MusicXml types for divisions.
division_musicxml_tbl :: [(Division, String)]
division_musicxml_tbl =
  let nm =
        [ "long"
        , "breve"
        , "whole"
        , "half"
        , "quarter"
        , "eighth"
        , "16th"
        , "32nd"
        , "64th"
        , "128th"
        , "256th"
        ]
  in zip divisions_musicxml_set nm

{- | Lookup 'division_musicxml_tbl'.

>>> map whole_note_division_to_musicxml_type [2,4]
["half","quarter"]
-}
whole_note_division_to_musicxml_type :: Division -> String
whole_note_division_to_musicxml_type x =
  List.lookup_err_msg "division_musicxml_tbl" x division_musicxml_tbl

{- | Variant of 'whole_note_division_to_musicxml_type' extracting 'division' from 'Duration', dots & multipler are ignored.

>>> duration_to_musicxml_type (Duration 4 0 1)
"quarter"
-}
duration_to_musicxml_type :: Duration -> String
duration_to_musicxml_type = whole_note_division_to_musicxml_type . division

-- * Unicode

-- | Table giving @Unicode@ symbols for divisions.
division_unicode_tbl :: [(Integer, Char)]
division_unicode_tbl = zip [0, 1, 2, 4, 8, 16, 32, 64, 128, 256] "𝅜𝅝𝅗𝅥𝅘𝅥𝅘𝅥𝅮𝅘𝅥𝅯𝅘𝅥𝅰𝅘𝅥𝅱𝅘𝅥𝅲"

{- | Lookup 'division_unicode_tbl'.

>>> map whole_note_division_to_unicode_symbol [1,2,4,8] == "𝅝𝅗𝅥𝅘𝅥𝅘𝅥𝅮"
True
-}
whole_note_division_to_unicode_symbol :: Division -> Char
whole_note_division_to_unicode_symbol x =
  List.lookup_err_msg "division_unicode_tbl" x division_unicode_tbl

{- | Give Unicode string for 'Duration'. The duration multiplier is /not/ written.

>>> map duration_to_unicode [Duration 1 2 1,Duration 4 1 1] == ["𝅝𝅭𝅭","𝅘𝅥𝅭"]
True
-}
duration_to_unicode :: Duration -> String
duration_to_unicode (Duration dv d _) =
  let dv' = whole_note_division_to_unicode_symbol dv
  in dv' : replicate (fromIntegral d) '𝅭'

-- * Lilypond

{- | Give /Lilypond/ notation for 'Duration'.
Note that the duration multiplier is /not/ written.

>>> map duration_to_lilypond_type [Duration 2 0 1,Duration 4 1 1]
["2","4."]
-}
duration_to_lilypond_type :: Duration -> String
duration_to_lilypond_type (Duration dv d _) =
  let dv' = if dv == 0 then "\\breve" else show dv
  in dv' ++ replicate (fromIntegral d) '.'

-- * Humdrum

{- | Duration to @**recip@ notation.

<http://humdrum.org/Humdrum/representations/recip.rep.html>

>>> let d = map (\z -> Duration z 0 1) [0,1,2,4,8,16,32]
>>> map duration_recip_pp d
["0","1","2","4","8","16","32"]

>>> let d = [Duration 1 1 (1/3),Duration 4 1 1,Duration 4 1 (2/3)]
>>> map duration_recip_pp d
["3.","4.","6."]
-}
duration_recip_pp :: Duration -> String
duration_recip_pp (Duration x d m) =
  let (mn, md) = (numerator m, denominator m)
      r = (x % mn) * (md % 1)
  in if denominator r == 1
      then show (numerator r) ++ genericReplicate d '.'
      else error (show ("duration_recip_pp", x, d, m, r))

-- * Letter

{- | Names for note divisions.
Starting from 1/32 these names haev uniqe initial letters that can be used for concise notation.
-}
whole_note_division_name_tbl :: [(Division, String)]
whole_note_division_name_tbl =
  [ (64, "sixtyfourth") -- hemidemisemiquaver
  , (32, "thirtysecond") -- demisemiquaver
  , (16, "sixteenth") -- semiquaver
  , (8, "eighth") -- quaver
  , (4, "quarter") -- crotchet
  , (2, "half") -- minim
  , (1, "whole") -- semibreve
  , (0, "breve")
  , (-1, "longa")
  , (-2, "maxima")
  ]

whole_note_division_name :: Division -> Maybe String
whole_note_division_name = flip lookup whole_note_division_name_tbl

whole_note_division_letter_tbl :: [(Division, Char)]
whole_note_division_letter_tbl = map (\(d, n) -> (d, List.head_err n)) whole_note_division_name_tbl

{- | Letter names for note divisions.

>>> mapMaybe whole_note_division_letter_pp [-2, -1, 0, 1, 2, 4, 8, 16, 32]
"mlbwhqest"
-}
whole_note_division_letter_pp :: Division -> Maybe Char
whole_note_division_letter_pp = flip lookup (List.tail_err whole_note_division_letter_tbl)

{- | Letter names for durations.

>>> mapMaybe duration_letter_pp [Duration 4 0 1,Duration 2 1 1,Duration 8 2 1]
["q","h.","e.."]

>>> mapMaybe duration_letter_pp [Duration 4 1 (2/3)]
["q./2:3"]
-}
duration_letter_pp :: Duration -> Maybe String
duration_letter_pp (Duration x d m) =
  let d' = genericReplicate d '.'
      m' = case (numerator m, denominator m) of
        (1, 1) -> ""
        (i, j) -> '/' : show i ++ ":" ++ show j
  in case whole_note_division_letter_pp x of
      Just x' -> Just (x' : d' ++ m')
      _ -> Nothing
