-- | Rational quarter-note notation for durations.
module Music.Theory.Duration.RQ where

import Data.Function {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}

import Music.Theory.Duration {- hmt -}

-- | Rational Quarter-Note
type RQ = Rational

-- > rq_duration_tbl 2
rq_duration_tbl :: Dots -> [(Rational,Duration)]
rq_duration_tbl k = map (\d -> (duration_to_rq d,d)) (duration_set k)

-- | Rational quarter note to duration value.  It is a mistake to hope
-- this could handle tuplets directly since, for instance, a @3:2@
-- dotted note will be of the same duration as a plain undotted note.
--
-- > rq_to_duration (3/4) == Just dotted_eighth_note
rq_to_duration :: RQ -> Maybe Duration
rq_to_duration x = lookup x (rq_duration_tbl 2)

-- | Is 'RQ' a /cmn/ duration.
--
-- > map rq_is_cmn [1/4,1/5,1/8,3/32] == [True,False,True,False]
rq_is_cmn :: RQ -> Bool
rq_is_cmn = isJust . rq_to_duration

-- | Variant of 'rq_to_duration' with error message.
rq_to_duration_err :: Show a => a -> RQ -> Duration
rq_to_duration_err msg n =
    let err = error ("rq_to_duration:" ++ show (msg,n))
    in fromMaybe err (rq_to_duration n)

-- | Convert a whole note division integer to an 'RQ' value.
--
-- > map whole_note_division_to_rq [1,2,4,8] == [4,2,1,1/2]
whole_note_division_to_rq :: Division -> RQ
whole_note_division_to_rq x =
    let f = (* 4) . recip . (%1)
    in case x of
         0 -> 8
         -1 -> 16
         _ -> f x

-- | Apply dots to an 'RQ' duration.
--
-- > map (rq_apply_dots 1) [1,2] == [1 + 1/2,1 + 1/2 + 1/4]
rq_apply_dots :: RQ -> Dots -> RQ
rq_apply_dots n d =
    let m = iterate (/ 2) n
    in sum (genericTake (d + 1) m)

-- | Convert 'Duration' to 'RQ' value, see 'rq_to_duration' for
-- partial inverse.
--
-- > let d = [half_note,dotted_quarter_note,dotted_whole_note]
-- > in map duration_to_rq d == [2,3/2,6]
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

-- | Is /p/ divisible by /q/, ie. is the 'denominator' of @p/q@ '==' @1@.
--
-- > map (rq_divisible_by (3%2)) [1%2,1%3] == [True,False]
rq_divisible_by :: RQ -> RQ -> Bool
rq_divisible_by i j = denominator (i / j) == 1

-- | Is 'RQ' a whole number (ie. is 'denominator' '==' @1@.
--
-- > map rq_is_integral [1,3/2,2] == [True,False,True]
rq_is_integral :: RQ -> Bool
rq_is_integral = (== 1) . denominator

-- | Return 'numerator' of 'RQ' if 'denominator' '==' @1@.
--
-- > map rq_integral [1,3/2,2] == [Just 1,Nothing,Just 2]
rq_integral :: RQ -> Maybe Integer
rq_integral n = if rq_is_integral n then Just (numerator n) else Nothing

-- | Derive the tuplet structure of a set of 'RQ' values.
--
-- > rq_derive_tuplet_plain [1/2] == Nothing
-- > rq_derive_tuplet_plain [1/2,1/2] == Nothing
-- > rq_derive_tuplet_plain [1/4,1/4] == Nothing
-- > rq_derive_tuplet_plain [1/3,2/3] == Just (3,2)
-- > rq_derive_tuplet_plain [1/2,1/3,1/6] == Just (6,4)
-- > rq_derive_tuplet_plain [1/3,1/6] == Just (6,4)
-- > rq_derive_tuplet_plain [2/5,3/5] == Just (5,4)
-- > rq_derive_tuplet_plain [1/3,1/6,2/5,1/10] == Just (30,16)
--
-- > map rq_derive_tuplet_plain [[1/3,1/6],[2/5,1/10]] == [Just (6,4)
-- >                                                      ,Just (10,8)]
rq_derive_tuplet_plain :: [RQ] -> Maybe (Integer,Integer)
rq_derive_tuplet_plain x =
    let i = foldl lcm 1 (map denominator x)
        j = let z = iterate (* 2) 2
            in fromJust (find (>= i) z) `div` 2
    in if i `rem` j == 0 then Nothing else Just (i,j)

-- | Derive the tuplet structure of a set of 'RQ' values.
--
-- > rq_derive_tuplet [1/4,1/8,1/8] == Nothing
-- > rq_derive_tuplet [1/3,2/3] == Just (3,2)
-- > rq_derive_tuplet [1/2,1/3,1/6] == Just (3,2)
-- > rq_derive_tuplet [2/5,3/5] == Just (5,4)
-- > rq_derive_tuplet [1/3,1/6,2/5,1/10] == Just (15,8)
rq_derive_tuplet :: [RQ] -> Maybe (Integer,Integer)
rq_derive_tuplet =
    let f (i,j) = let k = i % j
                  in (numerator k,denominator k)
    in fmap f . rq_derive_tuplet_plain

-- | Remove tuplet multiplier from value, ie. to give notated
-- duration.  This seems odd but is neccessary to avoid ambiguity.
-- Ie. is @1@ a quarter note or a @3:2@ tuplet dotted-quarter-note etc.
--
-- > map (rq_un_tuplet (3,2)) [1,2/3,1/2,1/3] == [3/2,1,3/4,1/2]
rq_un_tuplet :: (Integer,Integer) -> RQ -> RQ
rq_un_tuplet (i,j) x = x * (i % j)

-- | If an 'RQ' duration is un-representable by a single /cmn/
-- duration, give tied notation.
--
-- > catMaybes (map rq_to_cmn [1..9]) == [(4,1),(4,3),(8,1)]
--
-- > map rq_to_cmn [5/4,5/8] == [Just (1,1/4),Just (1/2,1/8)]
rq_to_cmn :: RQ -> Maybe (RQ,RQ)
rq_to_cmn x =
    let (i,j) = (numerator x,denominator x)
        k = case i of
              5 -> Just (4,1)
              7 -> Just (4,3)
              9 -> Just (8,1)
              _ -> Nothing
        f (n,m) = (n%j,m%j)
    in fmap f k

-- | Predicate to determine if a segment can be notated either without
-- a tuplet or with a single tuplet.
--
-- > rq_can_notate [1/2,1/4,1/4] == True
-- > rq_can_notate [1/3,1/6] == True
-- > rq_can_notate [2/5,1/10] == True
-- > rq_can_notate [1/3,1/6,2/5,1/10] == False
-- > rq_can_notate [4/7,1/7,6/7,3/7] == True
-- > rq_can_notate [4/7,1/7,2/7] == True
rq_can_notate :: [RQ] -> Bool
rq_can_notate x =
    let x' = case rq_derive_tuplet x of
               Nothing -> x
               Just t -> map (rq_un_tuplet t) x
    in all rq_is_cmn x'
