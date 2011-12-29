-- | Rational quarter-note notation for durations.
module Music.Theory.Duration.RQ where

import Data.Function
import Data.List
import Data.Ratio
import Music.Theory.Duration
import Music.Theory.Duration.Name

-- | Rational Quarter-Note
type RQ = Rational

-- | Rational quarter note to duration value.  It is a mistake to hope
-- this could handle tuplets directly since, for instance, a @3:2@
-- dotted note will be of the same duration as a plain undotted note.
--
-- > rq_to_duration (3/4) == Just dotted_eighth_note
rq_to_duration :: RQ -> Maybe Duration
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

-- | Convert a whole note division integer to an 'RQ' value.
--
-- > map whole_note_division_to_rq [1,2,4,8] == [4,2,1,1/2]
whole_note_division_to_rq :: Integer -> RQ
whole_note_division_to_rq x =
    let f = (* 4) . recip . (%1)
    in case x of
         0 -> 8
         -1 -> 16
         _ -> f x

-- | Apply dots to an 'RQ' duration.
--
-- > map (rq_apply_dots 1) [1,2] == [3/2,7/4]
rq_apply_dots :: RQ -> Integer -> RQ
rq_apply_dots n d =
    let m = iterate (/ 2) n
    in sum (genericTake (d + 1) m)

-- | Convert 'Duration' to 'RQ' value, see 'rq_to_duration' for
-- partial inverse.
--
-- > map duration_to_rq [half_note,dotted_quarter_note] == [2,3/2]
duration_to_rq :: Duration -> RQ
duration_to_rq (Duration n d m) =
    let x = whole_note_division_to_rq n
    in rq_apply_dots x d * m

-- | 'compare' function for 'Duration' via 'duration_to_rq'.
--
-- > half_note `duration_compare_rq` quarter_note == GT
duration_compare_rq :: Duration -> Duration -> Ordering
duration_compare_rq = compare `on` duration_to_rq

-- | 'RQ' modulo.
--
-- > map (rq_mod (5/2)) [3/2,3/4,5/2] == [1,1/4,0]
rq_mod :: RQ -> RQ -> RQ
rq_mod i j
    | i == j = 0
    | i < 0 = rq_mod (i + j) j
    | i > j = rq_mod (i - j) j
    | otherwise = i

-- | Is /p/ divisisble by /q/, ie. is the 'denominator' of @p/q@ '==' @1@.
--
-- > map (rq_divisible_by (3%2)) [1%2,1%3] == [True,False]
rq_divisible_by :: RQ -> RQ -> Bool
rq_divisible_by i j = denominator (i / j) == 1
