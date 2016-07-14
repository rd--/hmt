-- | Common music notation duration model.
module Music.Theory.Duration where

import Control.Monad {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}

import qualified Music.Theory.Ord as T {- hmt -}

-- | Common music notation durational model
data Duration = Duration {division :: Integer -- ^ division of whole note
                         ,dots :: Integer -- ^ number of dots
                         ,multiplier :: Rational -- ^ tuplet modifier
                         }
                deriving (Eq,Show)

-- | Are multipliers equal?
duration_meq :: Duration -> Duration -> Bool
duration_meq p q = multiplier p == multiplier q

-- | Compare durations with equal multipliers.
duration_compare_meq :: Duration -> Duration -> Maybe Ordering
duration_compare_meq y0 y1 =
    let (Duration x0 n0 m0) = y0
        (Duration x1 n1 m1) = y1
    in if y0 == y1
       then Just EQ
       else if m0 /= m1
            then Nothing
            else Just (if x0 == x1
                       then compare n0 n1
                       else compare x1 x0)

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
no_dots (x0,x1) = dots x0 == 0 && dots x1 == 0

-- | Sum undotted divisions, input is required to be sorted.
sum_dur_undotted :: (Integer, Integer) -> Maybe Duration
sum_dur_undotted (x0, x1)
    | x0 == x1 = Just (Duration (x0 `div` 2) 0 1)
    | x0 == x1 * 2 = Just (Duration x1 1 1)
    | otherwise = Nothing

-- | Sum dotted divisions, input is required to be sorted.
--
-- > sum_dur_dotted (4,1,4,1) == Just (Duration 2 1 1)
-- > sum_dur_dotted (4,0,2,1) == Just (Duration 1 0 1)
-- > sum_dur_dotted (8,1,4,0) == Just (Duration 4 2 1)
-- > sum_dur_dotted (16,0,4,2) == Just (Duration 2 0 1)
sum_dur_dotted :: (Integer,Integer,Integer,Integer) -> Maybe Duration
sum_dur_dotted (x0, n0, x1, n1)
    | x0 == x1 &&
      n0 == 1 &&
      n1 == 1 = Just (Duration (x1 `div` 2) 1 1)
    | x0 == x1 * 2 &&
      n0 == 0 &&
      n1 == 1 = Just (Duration (x1 `div` 2) 0 1)
    | x0 == x1 * 4 &&
      n0 == 0 &&
      n1 == 2 = Just (Duration (x1 `div` 2) 0 1)
    | x0 == x1 * 2 &&
      n0 == 1 &&
      n1 == 0 = Just (Duration x1 2 1)
    | otherwise = Nothing

-- | Sum durations.  Not all durations can be summed, and the present
--   algorithm is not exhaustive.
--
-- > import Music.Theory.Duration.Name
-- > sum_dur quarter_note eighth_note == Just dotted_quarter_note
-- > sum_dur dotted_quarter_note eighth_note == Just half_note
-- > sum_dur quarter_note dotted_eighth_note == Just double_dotted_quarter_note
sum_dur :: Duration -> Duration -> Maybe Duration
sum_dur y0 y1 =
    let f (x0,x1) = if no_dots (x0,x1)
                    then sum_dur_undotted (division x0, division x1)
                    else sum_dur_dotted (division x0, dots x0
                                        ,division x1, dots x1)
    in join (fmap f (T.sort_pair_m duration_compare_meq (y0,y1)))

-- | Erroring variant of 'sum_dur'.
sum_dur_err :: Duration -> Duration -> Duration
sum_dur_err y0 y1 =
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
-- > import Music.Theory.Duration.Name
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

whole_note_division_pp :: Integer -> Maybe Char
whole_note_division_pp x =
    let t = [(16,'s'),(8,'e'),(4,'q'),(2,'h'),(1,'w')]
    in lookup x t

-- > import Music.Theory.Duration.Name.Abbreviation
-- > map duration_pp [q,h',e''] == [Just "q",Just "h'",Just "e''"]
duration_pp :: Duration -> Maybe String
duration_pp (Duration x d m) =
    let d' = genericReplicate d '\''
        m' = case (numerator m,denominator m) of
               (1,1) -> ""
               (1,i) -> '/' : show i
               (i,j) -> '*' : show i ++ "/" ++ show j
    in case whole_note_division_pp x of
         Just x' -> Just (x' : d' ++ m')
         _ -> Nothing

-- | Duration to @**recip@ notation.
--
-- http://humdrum.org/Humdrum/representations/recip.rep.html
--
-- > let d = map (\z -> Duration z 0 1) [0,1,2,4,8,16,32]
-- > in map duration_recip_pp d == ["0","1","2","4","8","16","32"]
--
-- > let d = [Duration 1 1 (1/3),Duration 4 1 1,Duration 4 1 (2/3)]
-- > in map duration_recip_pp d == ["3.","4.","6."]
duration_recip_pp :: Duration -> String
duration_recip_pp (Duration x d m) =
    let (mn,md) = (numerator m,denominator m)
        r = (x % mn) * (md % 1)
    in if denominator r == 1
       then show (numerator r) ++ genericReplicate d '.'
       else error (show ("duration_recip_pp",x,d,m,r))
