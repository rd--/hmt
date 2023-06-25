-- | Rational quarter-note notation for durations.
module Music.Theory.Duration.Rq where

import Data.Function {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}

import qualified Music.Theory.List as T {- hmt-base -}

import Music.Theory.Duration {- hmt -}

-- | Rational Quarter-Note
type Rq = Rational

{- | Table mapping tuplet Rq values to Durations.
     Only has cases where the duration can be expressed without a tie.
     Currently has entries for 3-,5-,6- and 7-tuplets.

>>> all (\(i,j) -> i == duration_to_rq j) rq_tuplet_duration_table
True
-}
rq_tuplet_duration_table :: [(Rq, Duration)]
rq_tuplet_duration_table =
  [(1/3,Duration 8 0 (2/3))
  ,(2/3,Duration 4 0 (2/3))
  ,(1/5,Duration 16 0 (4/5))
  ,(2/5,Duration 8 0 (4/5))
  ,(3/5,Duration 8 1 (4/5))
  ,(4/5,Duration 4 0 (4/5))
  ,(1/6,Duration 16 0 (2/3))
  ,(1/7,Duration 16 0 (4/7))
  ,(2/7,Duration 8 0 (4/7))
  ,(3/7,Duration 8 1 (4/7))
  ,(4/7,Duration 4 0 (4/7))
  ,(6/7,Duration 4 1 (4/7))
  ]

{- | Lookup rq_tuplet_duration_tbl.

>>> rq_tuplet_to_duration (1/3) == Just (Duration 8 0 (2/3))
True
-}
rq_tuplet_to_duration :: Rq -> Maybe Duration
rq_tuplet_to_duration x = lookup x rq_tuplet_duration_table

{- | Make table of (Rq,Duration) associations.
     Only lists durations with a multiplier of 1.

>>> map (length . rq_plain_duration_tbl) [1,2,3]
[18,27,36]

>>> map (multiplier . snd) (rq_plain_duration_tbl 1) == replicate 18 1
True
-}
rq_plain_duration_tbl :: Dots -> [(Rq,Duration)]
rq_plain_duration_tbl k = map (\d -> (duration_to_rq d,d)) (duration_set k)

rq_plain_to_duration :: Dots -> Rq -> Maybe Duration
rq_plain_to_duration k x = lookup x (rq_plain_duration_tbl k)

rq_plain_to_duration_err :: Dots -> Rq -> Duration
rq_plain_to_duration_err k x = T.lookup_err x (rq_plain_duration_tbl k)

{- | Rational quarter note to duration value.
     Lookup composite plain (hence dots) and tuplet tables.
     It is a mistake to hope this could handle tuplets directly in a general sense.
     For instance, a @3:2@ dotted note is the same duration as a plain undotted note.
     However it does give durations for simple notations of simple tuplet values.

>>> rq_to_duration 2 (3/4) == Just (Duration 8 1 1) -- dotted_eighth_note
True

>>> rq_to_duration 2 (1/3) == Just (Duration 8 0 (2/3))
True
-}
rq_to_duration :: Dots -> Rq -> Maybe Duration
rq_to_duration k x = lookup x (rq_tuplet_duration_table ++ rq_plain_duration_tbl k)

-- | Variant of 'rq_to_duration' with error message.
rq_to_duration_err :: Show a => a -> Dots -> Rq -> Duration
rq_to_duration_err msg k n =
    let err = error ("rq_to_duration:" ++ show (msg,n))
    in fromMaybe err (rq_to_duration k n)

{- | Is 'Rq' a /cmn/ duration (ie. rq_plain_to_duration)

>>> map (rq_is_cmn 2) [1/4,1/5,1/8,3/32]
[True,False,True,True]
-}
rq_is_cmn :: Dots -> Rq -> Bool
rq_is_cmn k = isJust . rq_plain_to_duration k

{- | Convert a whole note division integer to an 'Rq' value.

>>> map whole_note_division_to_rq [1,2,4,8] == [4,2,1,1/2]
True
-}
whole_note_division_to_rq :: Division -> Rq
whole_note_division_to_rq x =
    let f = (* 4) . recip . (% 1)
    in case x of
         0 -> 8
         -1 -> 16
         _ -> f x

{- | Apply dots to an 'Rq' duration.

>>> map (rq_apply_dots 1) [1,2] == [1 + 1/2,1 + 1/2 + 1/4]
True
-}
rq_apply_dots :: Rq -> Dots -> Rq
rq_apply_dots n d =
    let m = iterate (/ 2) n
    in sum (genericTake (d + 1) m)

{- | Convert 'Duration' to 'Rq' value, see 'rq_to_duration' for partial inverse.

>>> let d = [Duration 2 0 1,Duration 4 1 1,Duration 1 1 1]
>>> map duration_to_rq d == [2,3/2,6] -- half_note,dotted_quarter_note,dotted_whole_note
True
-}
duration_to_rq :: Duration -> Rq
duration_to_rq (Duration n d m) =
    let x = whole_note_division_to_rq n
    in rq_apply_dots x d * m

{- | 'compare' function for 'Duration' via 'duration_to_rq'.

>>> Duration 2 0 1 `duration_compare_rq` Duration 4 0 1
GT
-}
duration_compare_rq :: Duration -> Duration -> Ordering
duration_compare_rq = compare `on` duration_to_rq

{- | 'Rq' modulo.

>>> map (rq_mod (5/2)) [3/2,3/4,5/2] == [1,1/4,0]
True
-}
rq_mod :: Rq -> Rq -> Rq
rq_mod i j
    | i == j = 0
    | i < 0 = rq_mod (i + j) j
    | i > j = rq_mod (i - j) j
    | otherwise = i

{- | Is /p/ divisible by /q/, ie. is the 'denominator' of @p/q@ '==' @1@.

>>map (rq_divisible_by (3/2)) [1/2,1/3]
[True,False]
-}
rq_divisible_by :: Rq -> Rq -> Bool
rq_divisible_by i j = denominator (i / j) == 1

{- | Is 'Rq' a whole number (ie. is 'denominator' '==' @1@.

>>> map rq_is_integral [1,3/2,2]
[True,False,True]
-}
rq_is_integral :: Rq -> Bool
rq_is_integral = (== 1) . denominator

{- | Return 'numerator' of 'Rq' if 'denominator' '==' @1@.

>>> map rq_integral [1,3/2,2]
[Just 1,Nothing,Just 2]
-}
rq_integral :: Rq -> Maybe Integer
rq_integral n = if rq_is_integral n then Just (numerator n) else Nothing

{- | Derive the tuplet structure of a set of 'Rq' values.

>>> rq_derive_tuplet_plain [1/2]
Nothing

>>> rq_derive_tuplet_plain [1/2,1/2]
Nothing

>>> rq_derive_tuplet_plain [1/4,1/4]
Nothing

>>> rq_derive_tuplet_plain [1/3,2/3]
Just (3,2)

>>> rq_derive_tuplet_plain [1/2,1/3,1/6]
Just (6,4)

>>> rq_derive_tuplet_plain [1/3,1/6]
Just (6,4)

>>> rq_derive_tuplet_plain [2/5,3/5]
Just (5,4)

>>> rq_derive_tuplet_plain [1/3,1/6,2/5,1/10]
Just (30,16)

>>> map rq_derive_tuplet_plain [[1/3,1/6],[2/5,1/10]]
[Just (6,4),Just (10,8)]
-}
rq_derive_tuplet_plain :: [Rq] -> Maybe (Integer,Integer)
rq_derive_tuplet_plain x =
    let i = foldl lcm 1 (map denominator x)
        j = let z = iterate (* 2) 2
            in fromJust (find (>= i) z) `div` 2
    in if i `rem` j == 0 then Nothing else Just (i,j)

{- | Derive the tuplet structure of a set of 'Rq' values.

>>> rq_derive_tuplet [1/4,1/8,1/8]
Nothing

>>> rq_derive_tuplet [1/3,2/3]
Just (3,2)

>>> rq_derive_tuplet [1/2,1/3,1/6]
Just (3,2)

>>> rq_derive_tuplet [2/5,3/5]
Just (5,4)

>>> rq_derive_tuplet [1/3,1/6,2/5,1/10]
Just (15,8)
-}
rq_derive_tuplet :: [Rq] -> Maybe (Integer,Integer)
rq_derive_tuplet =
    let f (i,j) = let k = i % j
                  in (numerator k,denominator k)
    in fmap f . rq_derive_tuplet_plain

{- | Remove tuplet multiplier from value, ie. to give notated
-- duration.  This seems odd but is neccessary to avoid ambiguity.
-- Ie. is @1@ a quarter note or a @3:2@ tuplet dotted-quarter-note etc.

>>> map (rq_un_tuplet (3,2)) [1,2/3,1/2,1/3] == [3/2,1,3/4,1/2]
True
-}
rq_un_tuplet :: (Integer,Integer) -> Rq -> Rq
rq_un_tuplet (i,j) x = x * (i % j)

{- | If an 'Rq' duration is un-representable by a single /cmn/
-- duration, give tied notation.

>>> catMaybes (map rq_to_cmn [1..9]) == [(4,1),(4,3),(8,1)]
True

>>> map rq_to_cmn [5/4,5/8] == [Just (1,1/4),Just (1/2,1/8)]
True
-}
rq_to_cmn :: Rq -> Maybe (Rq,Rq)
rq_to_cmn x =
    let (i,j) = (numerator x,denominator x)
        k = case i of
              5 -> Just (4,1)
              7 -> Just (4,3)
              9 -> Just (8,1)
              _ -> Nothing
        f (n,m) = (n%j,m%j)
    in fmap f k

{- | Predicate to determine if a segment can be notated
     either without a tuplet or with a single tuplet.

>>> rq_can_notate 2 [1/2,1/4,1/4]
True

>>> rq_can_notate 2 [1/3,1/6]
True

>>> rq_can_notate 2 [2/5,1/10]
True

>>> rq_can_notate 2 [1/3,1/6,2/5,1/10]
False

>>> rq_can_notate 2 [4/7,1/7,6/7,3/7]
True

>>> rq_can_notate 2 [4/7,1/7,2/7]
True
-}
rq_can_notate :: Dots -> [Rq] -> Bool
rq_can_notate k x =
    let x' = case rq_derive_tuplet x of
               Nothing -> x
               Just t -> map (rq_un_tuplet t) x
    in all (rq_is_cmn k) x'

-- * Time

{- | Duration in seconds of Rq given qpm

--   qpm = pulses-per-minute, rq = rational-quarter-note

>>> map (\sd -> rq_to_seconds_qpm (90 * sd) 1) [1,2,4,8,16] == [2/3,1/3,1/6,1/12,1/24]
True

>>> map (rq_to_seconds_qpm 90) [1,2,3,4] == [2/3,1 + 1/3,2,2 + 2/3]
True

>>> map (rq_to_seconds_qpm 90) [0::Rq,1,1 + 1/2,1 + 3/4,1 + 7/8,2] == [0,2/3,1,7/6,5/4,4/3]
True
-}
rq_to_seconds_qpm :: Fractional a => a -> a -> a
rq_to_seconds_qpm qpm rq = rq * (60 / qpm)

{- | Qpm given that /rq/ has duration /x/, ie. inverse of 'rq_to_seconds_qpm'

>>> map (rq_to_qpm 1) [0.4,0.5,0.8,1,1.5,2]
[150.0,120.0,75.0,60.0,40.0,30.0]

>>> map (\qpm -> rq_to_seconds_qpm qpm 1) [150,120,75,60,40,30]
[0.4,0.5,0.8,1.0,1.5,2.0]
-}
rq_to_qpm :: Fractional a => a -> a -> a
rq_to_qpm rq x = (rq / x) * 60
