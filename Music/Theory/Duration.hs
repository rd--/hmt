-- | Common music notation duration model.
module Music.Theory.Duration where

import Data.Maybe

-- | Standard music notation durational model
data Duration = Duration {division :: Integer -- ^ division of whole note
                         ,dots :: Integer -- ^ number of dots
                         ,multiplier :: Rational -- ^ tuplet modifier
                         }
                deriving (Eq,Show)

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

-- | 'Ord' instance in terms of 'duration_compare_meq'.
instance Ord Duration where
    compare = duration_compare_meq

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
    in fromMaybe err y2


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
    let err = error "duration_beam_count"
        bc = whole_note_division_to_beam_count x
    in fromMaybe err bc
