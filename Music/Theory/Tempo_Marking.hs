-- | Common music notation tempo indications.
module Music.Theory.Tempo_Marking where

import Data.List {- base -}

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
--   signature and tempo marking.
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

-- | Italian terms and markings from Wittner metronome (W.-Germany).
-- <http://wittner-gmbh.de/>
metronome_table_wittner :: Num n => [(String,(n,n))]
metronome_table_wittner =
    [("Largo",(40,60))
    ,("Larghetto",(60,66))
    ,("Adagio",(66,76))
    ,("Andante",(76,108))
    ,("Moderato",(108,120))
    ,("Allegro",(120,168))
    ,("Presto",(168,208))]

-- | Italian terms and markings from Nikko Seiki metronome (Japan).
-- <http://nikkoseiki.com/>
metronome_table_nikko :: Num n => [(String,(n,n))]
metronome_table_nikko =
    [("Grave",(40,46))
    ,("Largo",(46,52))
    ,("Lento",(52,56))
    ,("Adagio",(56,60))
    ,("Larghetto",(60,66))
    ,("Adagietto",(66,72))
    ,("Andante",(72,80))
    ,("Andantino",(80,88))
    ,("Maestoso",(88,96))
    ,("Moderato",(96,108))
    ,("Allegretto",(108,120))
    ,("Animato",(120,132))
    ,("Allegro",(132,144))
    ,("Assai",(144,160))
    ,("Vivace",(160,184))
    ,("Presto",(184,208))
    ,("Prestissimo",(208,240))]

-- | Lookup metronome mark in table.
--
-- > mm_name metronome_table_nikko 72 == Just "Andante"
mm_name :: (Num a, Ord a) => [(String,(a,a))] -> a -> Maybe String
mm_name tbl x =
    let f (_,(p,q)) = x >= p && x < q
    in fmap fst (find f tbl)
