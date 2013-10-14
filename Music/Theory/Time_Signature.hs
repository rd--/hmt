-- | Time Signatures.
module Music.Theory.Time_Signature where

import Data.Ratio
import Music.Theory.Duration
import Music.Theory.Duration.Name
import Music.Theory.Duration.RQ

-- | A Time Signature is a /(numerator,denominator)/ pair.
type Time_Signature = (Integer,Integer)

-- | Tied, non-multiplied durations to fill a whole measure.
--
-- > ts_whole_note (3,8) == [dotted_quarter_note]
-- > ts_whole_note (2,2) == [whole_note]
ts_whole_note :: Time_Signature -> [Duration]
ts_whole_note t =
    case t of
      (1,8) -> [eighth_note]
      (2,16) -> [eighth_note]
      (3,16) -> [dotted_eighth_note]
      (1,4) -> [quarter_note]
      (2,8) -> [quarter_note]
      (4,16) -> [quarter_note]
      (5,16) -> [quarter_note,sixteenth_note]
      (3,8) -> [dotted_quarter_note]
      (6,16) -> [dotted_quarter_note]
      (7,16) -> [quarter_note,dotted_eighth_note]
      (1,2) -> [half_note]
      (2,4) -> [half_note]
      (4,8) -> [half_note]
      (5,8) -> [half_note,eighth_note]
      (3,4) -> [dotted_half_note]
      (6,8) -> [dotted_half_note]
      (1,1) -> [whole_note]
      (2,2) -> [whole_note]
      (4,4) -> [whole_note]
      (5,4) -> [whole_note,quarter_note]
      (3,2) -> [dotted_whole_note]
      (6,4) -> [dotted_whole_note]
      (7,4) -> [whole_note,dotted_half_note]
      (2,1) -> [breve]
      (4,2) -> [breve]
      (3,1) -> [dotted_breve]
      (6,2) -> [dotted_breve]
      _ -> error ("ts_whole_note: " ++ show t)

-- | Duration of measure in 'RQ'.
--
-- > map ts_whole_note_rq [(3,8),(2,2)] == [3/2,4]
ts_whole_note_rq :: Time_Signature -> RQ
ts_whole_note_rq = sum . map duration_to_rq . ts_whole_note

-- | Duration, in 'RQ', of a measure of indicated 'Time_Signature'.
--
-- > map ts_rq [(3,4),(5,8)] == [3,5/2]
ts_rq :: Time_Signature -> RQ
ts_rq (n,d) = (4 * n) % d

-- | 'Time_Signature' derived from whole note duration in 'RQ' form.
--
-- > map rq_to_ts [4,3/2,7/4,6] == [(4,4),(3,8),(7,16),(6,4)]
rq_to_ts :: Rational -> Time_Signature
rq_to_ts rq =
    let n = numerator rq
        d = denominator rq * 4
    in (n,d)

-- | Uniform division of time signature.
--
-- > ts_divisions (3,4) == [1,1,1]
-- > ts_divisions (3,8) == [1/2,1/2,1/2]
-- > ts_divisions (2,2) == [2,2]
-- > ts_divisions (1,1) == [4]
ts_divisions :: Time_Signature -> [RQ]
ts_divisions (i,j) =
    let k = fromIntegral i
    in replicate k (recip (j % 4))

-- | Convert a duration to a pulse count in relation to the indicated
--   time signature.
--
-- > ts_duration_pulses (3,8) quarter_note == 2
ts_duration_pulses :: Time_Signature -> Duration -> Rational
ts_duration_pulses (_, b) (Duration dv dt ml) =
    let n = b % dv
    in rq_apply_dots n dt * ml

-- | Rewrite time signature to indicated denominator.
--
-- > ts_rewrite 8 (3,4) == (6,8)
ts_rewrite :: Integer -> Time_Signature -> Time_Signature
ts_rewrite d' =
    let dv i j = let (x,y) = i `divMod` j
                 in if y == 0 then x else error "ts_rewrite"
        go (n,d) = case compare d d' of
                     EQ -> (n,d)
                     GT -> go (n `dv` 2, d `dv` 2)
                     LT -> go (n * 2, d * 2)
    in go

-- | Sum time signatures.
--
-- > ts_sum [(3,16),(1,2)] == (11,16)
ts_sum :: [Time_Signature] -> Time_Signature
ts_sum t =
    let i = maximum (map snd t)
        t' = map (ts_rewrite i) t
        j = sum (map fst t')
    in (j,i)

