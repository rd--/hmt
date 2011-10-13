module Music.Theory.Duration where

import Data.Function
import Data.List
import Data.Ratio

-- | Standard music notation durational model
data Duration = Duration {division :: Integer -- ^ division of whole note
                         ,dots :: Integer -- ^ number of dots
                         ,multiplier :: Rational -- ^ tuplet modifier
                         }
                deriving (Eq, Show)

instance Ord Duration where
    compare = duration_compare

-- | Standard music notation durational model annotations
data D_Annotation = Tie_Right | Tie_Left
                  | Begin_Tuplet (Integer,Integer,Duration) | End_Tuplet
                    deriving (Eq,Show)

-- * Operations

-- | 'Ord' 'compare' function for 'Duration'.
--
-- > half_note `duration_compare` quarter_note == GT
duration_compare :: Duration -> Duration -> Ordering
duration_compare = compare `on` duration_to_rq

-- | Compare durations with equal multipliers.
duration_compare_meq :: Duration -> Duration -> Ordering
duration_compare_meq y0 y1 =
    if y0 == y1
    then EQ
    else let (Duration x0 n0 m0) = y0
             (Duration x1 n1 m1) = y1
         in if m0 /= m1
            then error "duration_compare_meq: non-equal multipliers"
            else if x0 == x1
                 then compare n0 n1
                 else compare x1 x0

-- | Sort a pair of equal type values using given comparison function.
--
-- > sort_pair compare ('b','a') == ('a','b')
sort_pair :: (t -> t -> Ordering) -> (t,t) -> (t,t)
sort_pair fn (x,y) =
    case fn x y of
      LT -> (x,y)
      EQ -> (x,y)
      GT -> (y,x)

-- | True if neither duration is dotted.
no_dots :: (Duration, Duration) -> Bool
no_dots (x0,x1) = dots x0 == 0 && dots x1 == 0

-- | Sum undotted divisions, input is required to be sorted.
sum_dur_undotted :: (Integer, Integer) -> Maybe Duration
sum_dur_undotted (x0, x1)
    | x0 == x1 = Just (Duration (x0 `div` 2) 0 1)
    | x0 == x1 * 2 = Just (Duration x1 1 1)
    | otherwise = Nothing

-- | Sum dotted divisions, input is required to be sorted.
sum_dur_dotted :: (Integer,Integer,Integer,Integer) -> Maybe Duration
sum_dur_dotted (x0, n0, x1, n1)
    | x0 == x1 &&
      n0 == 1 &&
      n1 == 1 = Just (Duration (x1 `div` 2) 1 1)
    | x0 == x1 * 2 &&
      n0 == 0 &&
      n1 == 1 = Just (Duration (x1 `div` 2) 0 1)
    | otherwise = Nothing

-- | Sum durations.  Not all durations can be summed, and the present
--   algorithm is not exhaustive.
sum_dur :: Duration -> Duration -> Maybe Duration
sum_dur y0 y1 =
    let (x0,x1) = sort_pair duration_compare_meq (y0,y1)
    in if no_dots (x0,x1)
       then sum_dur_undotted (division x0, division x1)
       else sum_dur_dotted (division x0, dots x0
                           ,division x1, dots x1)

-- | Erroring variant of 'sum_dur'.
sum_dur' :: Duration -> Duration -> Duration
sum_dur' y0 y1 =
    let y2 = sum_dur y0 y1
        err = error ("sum_dur': " ++ show (y0,y1))
    in maybe err id y2

-- * RQ (Rational Quarter-Note)

-- | Rational number of quarter notes to duration value.
--   It is a mistake to hope this could handle tuplets
--   directly, ie. a 3:2 dotted note will be of the same
--   duration as a plain undotted note.
--
-- > rq_to_duration (3/4) == Just dotted_eighth_note
rq_to_duration :: Rational -> Maybe Duration
rq_to_duration x =
    case (numerator x,denominator x) of
      (1,8) -> Just thirtysecond_note
      (3,16) -> Just dotted_thirtysecond_note
      (1,4) -> Just sixteenth_note
      (3,8) -> Just dotted_sixteenth_note
      (1,2) -> Just eighth_note
      (3,4) -> Just dotted_eighth_note
      (1,1) -> Just quarter_note
      (3,2) -> Just dotted_quarter_note
      (2,1) -> Just half_note
      (3,1) -> Just dotted_half_note
      (7,2) -> Just double_dotted_half_note
      (4,1) -> Just whole_note
      (6,1) -> Just dotted_whole_note
      (8,1) -> Just breve
      (12,1) -> Just dotted_breve
      _ -> Nothing

-- | Convert a whole note division integer to a /RQ/ value.
--
-- > map whole_note_division_to_rq [1,2,4,8] == [4,2,1,1/2]
whole_note_division_to_rq :: Integer -> Rational
whole_note_division_to_rq x =
    let f = (* 4) . recip . (%1)
    in case x of
         0 -> 8
         -1 -> 16
         _ -> f x

-- | Apply dots to an /RQ/ duration.
--
-- > map (rq_apply_dots 1) [1,2] == [3/2,7/4]
rq_apply_dots :: Rational -> Integer -> Rational
rq_apply_dots n d =
    let m = iterate (\x -> x / 2) n
    in sum (genericTake (d + 1) m)

-- | Convert 'Duration' to /RQ/ value, see 'rq_to_duration' for
-- partial inverse.
--
-- > map duration_to_rq [half_note,dotted_quarter_note] == [2,3/2]
duration_to_rq :: Duration -> Rational
duration_to_rq (Duration n d m) =
    let x = whole_note_division_to_rq n
    in rq_apply_dots x d * m

-- | Give @MusicXML@ type for division.
--
-- > map whole_note_division_to_musicxml_type [2,4] == ["half","quarter"]
whole_note_division_to_musicxml_type :: Integer -> String
whole_note_division_to_musicxml_type x =
    case x of
      256 -> "256th"
      128 -> "128th"
      64 -> "64th"
      32 -> "32nd"
      16 -> "16th"
      8 -> "eighth"
      4 -> "quarter"
      2 -> "half"
      1 -> "whole"
      0 -> "breve"
      -1 -> "long"
      _ -> error ("whole_note_division_to_musicxml_type: " ++ show x)

-- | Variant of 'whole_note_division_to_musicxml_type' extracting
-- 'division' from 'Duration'.
--
-- > duration_to_musicxml_type quarter_note == "quarter"
duration_to_musicxml_type :: Duration -> String
duration_to_musicxml_type = whole_note_division_to_musicxml_type . division

-- | Give /Lilypond/ notation for 'Duration'.  Note that the duration
-- multiplier is /not/ written.
--
-- > map duration_to_lilypond_type [half_note,dotted_quarter_note] == ["2","4."]
duration_to_lilypond_type :: Duration -> String
duration_to_lilypond_type (Duration dv d _) =
    let dv' = if dv == 0 then "\\breve" else show dv
    in dv' ++ replicate (fromIntegral d) '.'

-- | Calculate number of beams at notated division.
--
-- > whole_note_division_to_beam_count 32 == Just 3
whole_note_division_to_beam_count :: Integer -> Maybe Integer
whole_note_division_to_beam_count x =
    let t = [(256,6),(128,5),(64,4),(32,3),(16,2),(8,1)
            ,(4,0),(2,0),(1,0),(0,0),(-1,0)]
    in lookup x t

-- | Calculate number of beams at 'Duration'.
--
-- > map duration_beam_count [half_note,sixteenth_note] == [0,2]
duration_beam_count :: Duration -> Integer
duration_beam_count (Duration x _ _) =
    case whole_note_division_to_beam_count x of
      Nothing -> error "duration_beam_count"
      Just x' -> x'

-- * Constants

breve,whole_note,half_note,quarter_note,eighth_note,sixteenth_note,thirtysecond_note :: Duration
breve = Duration 0 0 1
whole_note = Duration 1 0 1
half_note = Duration 2 0 1
quarter_note = Duration 4 0 1
eighth_note = Duration 8 0 1
sixteenth_note = Duration 16 0 1
thirtysecond_note = Duration 32 0 1

dotted_breve,dotted_whole_note,dotted_half_note,dotted_quarter_note,dotted_eighth_note,dotted_sixteenth_note,dotted_thirtysecond_note :: Duration
dotted_breve = Duration 0 1 1
dotted_whole_note = Duration 1 1 1
dotted_half_note = Duration 2 1 1
dotted_quarter_note = Duration 4 1 1
dotted_eighth_note = Duration 8 1 1
dotted_sixteenth_note = Duration 16 1 1
dotted_thirtysecond_note = Duration 32 1 1

double_dotted_breve,double_dotted_whole_note,double_dotted_half_note,double_dotted_quarter_note,double_dotted_eighth_note,double_dotted_sixteenth_note,double_dotted_thirtysecond_note :: Duration
double_dotted_breve = Duration 0 2 1
double_dotted_whole_note = Duration 2 2 1
double_dotted_half_note = Duration 2 2 1
double_dotted_quarter_note = Duration 4 2 1
double_dotted_eighth_note = Duration 8 2 1
double_dotted_sixteenth_note = Duration 16 2 1
double_dotted_thirtysecond_note = Duration 32 2 1
