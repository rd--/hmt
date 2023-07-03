{- | Time Signatures. -}
module Music.Theory.Time_Signature where

import Data.Function {- base -}
import Data.Ratio {- base -}

import qualified Music.Theory.Math as Math {- hmt-base -}

import Music.Theory.Duration
import Music.Theory.Duration.Name
import Music.Theory.Duration.Rq

{- | A Time Signature is a /(numerator,denominator)/ pair. -}
type Time_Signature = (Integer,Integer)

{- | Tied, non-multiplied durations to fill a whole measure.

>>> ts_whole_note (3,8) == [dotted_quarter_note]
True

>>> ts_whole_note (2,2) == [whole_note]
True
-}
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
      (8,8) -> [whole_note]
      (5,4) -> [whole_note,quarter_note]
      (3,2) -> [dotted_whole_note]
      (6,4) -> [dotted_whole_note]
      (7,4) -> [whole_note,dotted_half_note]
      (2,1) -> [breve]
      (4,2) -> [breve]
      (3,1) -> [dotted_breve]
      (6,2) -> [dotted_breve]
      _ -> error ("ts_whole_note: " ++ show t)

{- | Duration of measure in 'Rq'.

>>> map ts_whole_note_rq [(3,8),(2,2)] == [3/2,4]
True
-}
ts_whole_note_rq :: Time_Signature -> Rq
ts_whole_note_rq = sum . map duration_to_rq . ts_whole_note

{- | Duration, in 'Rq', of a measure of indicated 'Time_Signature'.

>>> map ts_rq [(3,4),(5,8)] == [3,5/2]
True
-}
ts_rq :: Time_Signature -> Rq
ts_rq (n,d) = (4 * n) % d

{- | 'compare' 'on' 'ts_rq'. -}
ts_compare :: Time_Signature -> Time_Signature -> Ordering
ts_compare = compare `on` ts_rq

{- | 'Time_Signature' derived from whole note duration in 'Rq' form.

>>> map rq_to_ts [4,3/2,7/4,6]
[(4,4),(3,8),(7,16),(6,4)]
-}
rq_to_ts :: Rq -> Time_Signature
rq_to_ts rq =
    let n = numerator rq
        d = denominator rq * 4
    in (n,d)

{- | Uniform division of time signature.

>>> ts_divisions (3,4) == [1,1,1]
True

>>> ts_divisions (3,8) == [1/2,1/2,1/2]
True

>>> ts_divisions (2,2) == [2,2]
True

>>> ts_divisions (1,1) == [4]
True

>>> ts_divisions (7,4) == [1,1,1,1,1,1,1]
True

-}
ts_divisions :: Time_Signature -> [Rq]
ts_divisions (i,j) =
    let k = fromIntegral i
    in replicate k (recip (j % 4))

{- | Convert a duration to a pulse count in relation to the indicated   time signature.

>>> ts_duration_pulses (3,8) quarter_note == 2
True
-}
ts_duration_pulses :: Time_Signature -> Duration -> Rational
ts_duration_pulses (_, b) (Duration dv dt ml) =
    let n = b % dv
    in rq_apply_dots n dt * ml

{- | Rewrite time signature to indicated denominator.

>>> ts_rewrite 8 (3,4)
(6,8)
-}
ts_rewrite :: Integer -> Time_Signature -> Time_Signature
ts_rewrite d' =
    let dv i j = let (x,y) = i `divMod` j
                 in if y == 0 then x else error "ts_rewrite"
        go (n,d) = case compare d d' of
                     EQ -> (n,d)
                     GT -> go (n `dv` 2, d `dv` 2)
                     LT -> go (n * 2, d * 2)
    in go

{- | Sum time signatures.

>>> ts_sum [(3,16),(1,2)]
(11,16)
-}
ts_sum :: [Time_Signature] -> Time_Signature
ts_sum t =
    let i = maximum (map snd t)
        t' = map (ts_rewrite i) t
        j = sum (map fst t')
    in (j,i)

-- * Composite Time Signatures

{- | A composite time signature is a sequence of 'Time_Signature's. -}
type Composite_Time_Signature = [Time_Signature]

{- | The 'Rq' is the 'sum' of 'ts_rq' of the elements.

>>> cts_rq [(3,4),(1,8)] == 3 + 1/2
True
-}
cts_rq :: Composite_Time_Signature -> Rq
cts_rq = sum . map ts_rq

{- | The divisions are the 'concat' of the 'ts_divisions' of the elements.

>>> cts_divisions [(3,4),(1,8)] == [1,1,1,1/2]
True
-}
cts_divisions :: Composite_Time_Signature -> [Rq]
cts_divisions = concatMap ts_divisions

{- | Pulses are 1-indexed, Rq locations are 0-indexed.

>>> map (cts_pulse_to_rq [(2,4),(1,8),(1,4)]) [1 .. 4] == [0,1,2,2 + 1/2]
True
-}
cts_pulse_to_rq :: Composite_Time_Signature -> Int -> Rq
cts_pulse_to_rq cts p =
    let dv = cts_divisions cts
    in sum (take (p - 1) dv)

{- | Variant that gives the /window/ of the pulse (ie. the start location and the duration).

>>> let r = [(0,1),(1,1),(2,1/2),(2 + 1/2,1)]
>>> map (cts_pulse_to_rqw [(2,4),(1,8),(1,4)]) [1 .. 4] == r
True
-}
cts_pulse_to_rqw :: Composite_Time_Signature -> Int -> (Rq,Rq)
cts_pulse_to_rqw cts p = (cts_pulse_to_rq cts p,cts_divisions cts !! (p - 1))

-- * Rational Time Signatures

{- | A rational time signature is a 'Composite_Time_Signature' where the parts are 'Rational'. -}
type Rational_Time_Signature = [(Rational,Rational)]

{- | The 'sum' of the Rq of the elements.

>>> rts_rq [(3,4),(1,8)] == 3 + 1/2
True

>>> rts_rq [(3/2,4),(1/2,8)] == 3/2 + 1/4
True
-}
rts_rq :: Rational_Time_Signature -> Rq
rts_rq =
    let f (n,d) = (4 * n) / d
    in sum . map f

{- | The /divisions/ of the elements.

>>> rts_divisions [(3,4),(1,8)] == [[1,1,1],[1/2]]
True

>>> rts_divisions [(3/2,4),(1/2,8)] == [[1,1/2],[1/4]]
True
-}
rts_divisions :: Rational_Time_Signature -> [[Rq]]
rts_divisions =
    let f (n,d) = let (ni,nf) = Math.integral_and_fractional_parts n
                      rq = recip (d / 4)
                      ip = replicate ni rq
                  in if nf == 0 then ip else ip ++ [nf * rq]
    in map f

{- | Derive Rational_Time_Signature from list of Rq.

>>> rts_derive [1,1,1,1/2] == [(1,4),(1,4),(1,4),(1/2,4)]
True

>>> rts_derive [1,1/2,1/4] == [(1,4),(1/2,4),(1/4,4)]
True
-}
rts_derive :: [Rq] -> Rational_Time_Signature
rts_derive = let f rq = (rq,4) in map f

{- | Pulses are 1-indexed, Rq locations are 0-indexed.

>>> map (rts_pulse_to_rq [(2,4),(1,8),(1,4)]) [1 .. 4] == [0,1,2,2 + 1/2]
True

>>> map (rts_pulse_to_rq [(3/2,4),(1/2,8),(1/4,4)]) [1 .. 4] == [0,1,3/2,7/4]
True
-}
rts_pulse_to_rq :: Rational_Time_Signature -> Int -> Rq
rts_pulse_to_rq rts p =
    let dv = concat (rts_divisions rts)
    in sum (take (p - 1) dv)

{- | Variant that gives the /window/ of the pulse (ie. the start location and the duration).

>>> let r = [(0,1),(1,1),(2,1/2),(2 + 1/2,1)]
>>> map (rts_pulse_to_rqw [(2,4),(1,8),(1,4)]) [1 .. 4] == r
True
-}
rts_pulse_to_rqw :: Rational_Time_Signature -> Int -> (Rq,Rq)
rts_pulse_to_rqw ts p = (rts_pulse_to_rq ts p,concat (rts_divisions ts) !! (p - 1))
