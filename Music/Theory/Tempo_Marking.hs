-- | Common music notation tempo indications.
module Music.Theory.Tempo_Marking where

import Music.Theory.Duration
import Music.Theory.Duration.RQ
import Music.Theory.Time_Signature

-- | A tempo marking is in terms of a common music notation 'Duration'.
type Tempo_Marking = (Duration,Rational)

-- | Duration of a RQ value, in seconds, given indicated tempo.
--
-- > rq_to_seconds (quarter_note,90) 1 == 60/90
rq_to_seconds :: Tempo_Marking -> RQ -> Rational
rq_to_seconds (d,n) x =
    let d' = duration_to_rq d
        s = 60 / n
    in (x * s) / d'

-- | The duration, in seconds, of a pulse at the indicated time
--   signaure and tempo marking.
--
-- > import Music.Theory.Duration.Name
-- > pulse_duration (6,8) (quarter_note,60) == 1/2
pulse_duration :: Time_Signature -> Tempo_Marking -> Rational
pulse_duration t (x,i) =
    let j = recip (ts_duration_pulses t x)
        s = 60 / i
    in j * s

-- | The duration, in seconds, of a measure at the indicated time
--   signaure and tempo marking.
--
-- > measure_duration (3,4) (quarter_note,90) == 2
-- > measure_duration (6,8) (quarter_note,120) == 3/2
measure_duration :: Time_Signature -> Tempo_Marking -> Rational
measure_duration (n,d) t = pulse_duration (n,d) t * fromIntegral n

-- | 'Fractional' variant of 'measure_duration'.
measure_duration_f :: Fractional c => Time_Signature -> Tempo_Marking -> c
measure_duration_f ts = fromRational . measure_duration ts
