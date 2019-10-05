-- | Harmonic series
module Music.Theory.Tuning.HS where

import Data.List {- base -}
import Data.Ratio {- base -}
import qualified Safe {- safe -}

import qualified Music.Theory.Pitch as T {- hmt -}
import Music.Theory.Tuning {- hmt -}
import Music.Theory.Tuning.Type {- hmt -}


-- | Harmonic series to /n/th partial, with indicated octave.
--
-- > harmonic_series 17 2
harmonic_series :: Integer -> Rational -> Tuning
harmonic_series n o = Tuning (Left [1 .. n%1]) o

-- | Harmonic series on /n/.
harmonic_series_cps :: (Num t, Enum t) => t -> [t]
harmonic_series_cps n = [n,n * 2 ..]

-- | /n/ elements of 'harmonic_series_cps'.
--
-- > let r = [55,110,165,220,275,330,385,440,495,550,605,660,715,770,825,880,935]
-- > harmonic_series_cps_n 17 55 == r
harmonic_series_cps_n :: (Num a, Enum a) => Int -> a -> [a]
harmonic_series_cps_n n = take n . harmonic_series_cps

-- | Sub-harmonic series on /n/.
subharmonic_series_cps :: (Fractional t,Enum t) => t -> [t]
subharmonic_series_cps n = map (* n) (map recip [1..])

-- | /n/ elements of 'harmonic_series_cps'.
--
-- > let r = [1760,880,587,440,352,293,251,220,196,176,160,147,135,126,117,110,104]
-- > map round (subharmonic_series_cps_n 17 1760) == r
subharmonic_series_cps_n :: (Fractional t,Enum t) => Int -> t -> [t]
subharmonic_series_cps_n n = take n . subharmonic_series_cps

-- | /n/th partial of /f1/, ie. one indexed.
--
-- > map (partial 55) [1,5,3] == [55,275,165]
partial :: (Num a, Enum a) => a -> Int -> a
partial f1 k = harmonic_series_cps f1 `Safe.at` (k - 1)

-- | Derivative harmonic series, based on /k/th partial of /f1/.
--
-- > import Music.Theory.Pitch
--
-- > let r = [52,103,155,206,258,309,361,412,464,515,567,618,670,721,773]
-- > let d = harmonic_series_cps_derived 5 (T.octpc_to_cps (1,4))
-- > map round (take 15 d) == r
harmonic_series_cps_derived :: (Ord a, RealFrac a, Floating a, Enum a) => Int -> a -> [a]
harmonic_series_cps_derived k f1 =
    let f0 = T.cps_in_octave_above f1 (partial f1 k)
    in harmonic_series_cps f0

-- | Harmonic series to /n/th harmonic (folded, duplicated removed).
--
-- > harmonic_series_folded_r 17 == [1,17/16,9/8,5/4,11/8,3/2,13/8,7/4,15/8]
--
-- > let r = [0,105,204,386,551,702,841,969,1088]
-- > map (round . ratio_to_cents) (harmonic_series_folded_r 17) == r
harmonic_series_folded_r :: Integer -> [Rational]
harmonic_series_folded_r n = nub (sort (map fold_ratio_to_octave_err [1 .. n%1]))

-- | 'ratio_to_cents' variant of 'harmonic_series_folded'.
harmonic_series_folded_c :: Integer -> [Cents]
harmonic_series_folded_c = map ratio_to_cents . harmonic_series_folded_r

harmonic_series_folded :: Integer -> Rational -> Tuning
harmonic_series_folded n o = Tuning (Left (harmonic_series_folded_r n)) o

-- | @12@-tone tuning of first @21@ elements of the harmonic series.
--
-- > tn_cents_i harmonic_series_folded_21 == [0,105,204,298,386,471,551,702,841,969,1088]
-- > tn_divisions harmonic_series_folded_21 == 11
harmonic_series_folded_21 :: Tuning
harmonic_series_folded_21 = harmonic_series_folded 21 2

