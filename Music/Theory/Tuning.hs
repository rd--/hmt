-- | Tuning theory
module Music.Theory.Tuning where

import Data.Fixed {- base -}
import Data.List {- base -}
import Data.Ratio {- base -}

import Music.Theory.Either {- hmt -}
import Music.Theory.List {- hmt -}

-- * Types

-- | An approximation of a ratio.
type Approximate_Ratio = Double

-- | A real valued division of a semi-tone into one hundred parts, and
-- hence of the octave into @1200@ parts.
type Cents = Double

-- | A tuning specified 'Either' as a sequence of exact ratios, or as
-- a sequence of possibly inexact 'Cents'.
data Tuning = Tuning {ratios_or_cents :: Either [Rational] [Cents]
                     ,octave_ratio :: Rational}
              deriving (Eq,Show)

-- | Divisions of octave.
--
-- > divisions ditone == 12
divisions :: Tuning -> Int
divisions = either length length . ratios_or_cents

-- | 'Maybe' exact ratios of 'Tuning'.
ratios :: Tuning -> Maybe [Rational]
ratios = fromLeft . ratios_or_cents

-- | Possibly inexact 'Cents' of tuning.
cents :: Tuning -> [Cents]
cents = either (map ratio_to_cents) id . ratios_or_cents

-- | 'map' 'round' '.' 'cents'.
cents_i :: Integral i => Tuning -> [i]
cents_i = map round . cents

-- | Convert from interval in cents to frequency ratio.
--
-- > map cents_to_ratio [0,701.9550008653874,1200] == [1,3/2,2]
cents_to_ratio :: Floating a => a -> a
cents_to_ratio n = 2 ** (n / 1200)

-- | Possibly inexact 'Approximate_Ratio's of tuning.
approximate_ratios :: Tuning -> [Approximate_Ratio]
approximate_ratios =
    either (map approximate_ratio) (map cents_to_ratio) .
    ratios_or_cents

-- | Cyclic form, taking into consideration 'octave_ratio'.
approximate_ratios_cyclic :: Tuning -> [Approximate_Ratio]
approximate_ratios_cyclic t =
    let r = approximate_ratios t
        m = realToFrac (octave_ratio t)
        g = iterate (* m) 1
        f n = map (* n) r
    in concatMap f g

-- | 'Maybe' exact ratios reconstructed from possibly inexact 'Cents'
-- of 'Tuning'.
--
-- > let r = [1,17/16,9/8,13/11,5/4,4/3,7/5,3/2,11/7,5/3,16/9,15/8]
-- > in reconstructed_ratios 1e-2 werckmeister_iii == Just r
reconstructed_ratios :: Double -> Tuning -> Maybe [Rational]
reconstructed_ratios epsilon =
    fmap (map (reconstructed_ratio epsilon)) .
    fromRight .
    ratios_or_cents

-- | Convert from a 'Floating' ratio to /cents/.
--
-- > let r = [0,498,702,1200]
-- > in map (round . fratio_to_cents) [1,4/3,3/2,2] == r
fratio_to_cents :: (Real r,Floating n) => r -> n
fratio_to_cents = (1200 *) . logBase 2 . realToFrac

-- | Type specialised 'fratio_to_cents'.
approximate_ratio_to_cents :: Approximate_Ratio -> Cents
approximate_ratio_to_cents = fratio_to_cents

-- | Type specialised 'fromRational'.
approximate_ratio :: Rational -> Approximate_Ratio
approximate_ratio = fromRational

-- | 'approximate_ratio_to_cents' '.' 'approximate_ratio'.
ratio_to_cents :: Rational -> Cents
ratio_to_cents = approximate_ratio_to_cents . approximate_ratio

-- | Construct an exact 'Rational' that approximates 'Cents' to within
-- /epsilon/.
--
-- > map (reconstructed_ratio 1e-5) [0,700,1200] == [1,442/295,2]
--
-- > ratio_to_cents (442/295) == 699.9976981706734
reconstructed_ratio :: Double -> Cents -> Rational
reconstructed_ratio epsilon c = approxRational (cents_to_ratio c) epsilon

-- | Frequency /n/ cents from /f/.
--
-- > import Music.Theory.Pitch
-- > map (cps_shift_cents 440) [-100,100] == map octpc_to_cps [(4,8),(4,10)]
cps_shift_cents :: Floating a => a -> a -> a
cps_shift_cents f = (* f) . cents_to_ratio

-- | Interval in /cents/ from /p/ to /q/, ie. 'ratio_to_cents' of /p/
-- '/' /q/.
--
-- > cps_difference_cents 440 (octpc_to_cps (5,2)) == 500
--
-- > let abs_dif i j = abs (i - j)
-- > in cps_difference_cents 440 (fmidi_to_cps 69.1) `abs_dif` 10 < 1e9
cps_difference_cents :: (Real r,Fractional r,Floating n) => r -> r -> n
cps_difference_cents p q = fratio_to_cents (q / p)

-- * Commas

-- | The Syntonic comma.
--
-- > syntonic_comma == 81/80
syntonic_comma :: Rational
syntonic_comma = 81 % 80

-- | The Pythagorean comma.
--
-- > pythagorean_comma == 3^12 / 2^19
pythagorean_comma :: Rational
pythagorean_comma = 531441 / 524288

-- | Mercators comma.
--
-- > mercators_comma == 3^53 / 2^84
mercators_comma :: Rational
mercators_comma = 19383245667680019896796723 / 19342813113834066795298816

-- | Calculate /n/th root of /x/.
--
-- > 12 `nth_root` 2 == twelve_tone_equal_temperament_comma
nth_root :: (Floating a,Eq a) => a -> a -> a
nth_root n x =
    let f (_,x0) = (x0, ((n-1)*x0+x/x0**(n-1))/n)
        e = uncurry (==)
    in fst (until e f (x, x/n))

-- | 12-tone equal temperament comma (ie. 12th root of 2).
--
-- > twelve_tone_equal_temperament_comma == 1.0594630943592953
twelve_tone_equal_temperament_comma :: (Floating a,Eq a) => a
twelve_tone_equal_temperament_comma = 12 `nth_root` 2

-- * Equal temperaments

-- | Make /n/ division equal temperament.
equal_temperament :: Integral n => n -> Tuning
equal_temperament n =
    let c = genericTake n [0,1200 / fromIntegral n ..]
    in Tuning (Right c) 2

-- | 12-tone equal temperament.
--
-- > cents equal_temperament_12 == [0,100..1100]
equal_temperament_12 :: Tuning
equal_temperament_12 = equal_temperament (12::Int)

-- | 19-tone equal temperament.
equal_temperament_19 :: Tuning
equal_temperament_19 = equal_temperament (19::Int)

-- | 31-tone equal temperament.
equal_temperament_31 :: Tuning
equal_temperament_31 = equal_temperament (31::Int)

-- | 53-tone equal temperament.
equal_temperament_53 :: Tuning
equal_temperament_53 = equal_temperament (53::Int)

-- | 72-tone equal temperament.
--
-- > let r = [0,17,33,50,67,83,100]
-- > in take 7 (map round (cents equal_temperament_72)) == r
equal_temperament_72 :: Tuning
equal_temperament_72 = equal_temperament (72::Int)

-- * Harmonic series

-- | Raise or lower the frequency /q/ by octaves until it is in the
-- octave starting at /p/.
--
-- > fold_cps_to_octave_of 55 392 == 98
fold_cps_to_octave_of :: (Ord a, Fractional a) => a -> a -> a
fold_cps_to_octave_of p =
    let f q = if q > p * 2 then f (q / 2) else if q < p then f (q * 2) else q
    in f

-- | Harmonic series on /n/.
harmonic_series_cps :: (Num t, Enum t) => t -> [t]
harmonic_series_cps n = [n,n * 2 ..]

-- | /n/ elements of 'harmonic_series_cps'.
--
-- > let r = [55,110,165,220,275,330,385,440,495,550,605,660,715,770,825,880,935]
-- > in harmonic_series_cps_n 17 55 == r
harmonic_series_cps_n :: (Num a, Enum a) => Int -> a -> [a]
harmonic_series_cps_n n = take n . harmonic_series_cps

-- | Sub-harmonic series on /n/.
subharmonic_series_cps :: (Fractional t,Enum t) => t -> [t]
subharmonic_series_cps n = map (* n) (map recip [1..])

-- | /n/ elements of 'harmonic_series_cps'.
--
-- > let r = [1760,880,587,440,352,293,251,220,196,176,160,147,135,126,117,110,104]
-- > in map round (subharmonic_series_cps_n 17 1760) == r
subharmonic_series_cps_n :: (Fractional t,Enum t) => Int -> t -> [t]
subharmonic_series_cps_n n = take n . subharmonic_series_cps

-- | /n/th partial of /f1/, ie. one indexed.
--
-- > map (partial 55) [1,5,3] == [55,275,165]
partial :: (Num a, Enum a) => a -> Int -> a
partial f1 k = harmonic_series_cps f1 !! (k - 1)

-- | Fold ratio until within an octave, ie. @1@ '<' /n/ '<=' @2@.
--
-- > map fold_ratio_to_octave [2/3,3/4] == [4/3,3/2]
fold_ratio_to_octave :: Integral i => Ratio i -> Ratio i
fold_ratio_to_octave n =
    if n >= 2
    then fold_ratio_to_octave (n / 2)
    else if n < 1
         then fold_ratio_to_octave (n * 2)
         else n

-- | The interval between two pitches /p/ and /q/ given as ratio
-- multipliers of a fundamental is /q/ '/' /p/.  The classes over such
-- intervals consider the 'fold_ratio_to_octave' of both /p/ to /q/
-- and /q/ to /p/.
--
-- > map ratio_interval_class [2/3,3/2,3/4,4/3] == [3/2,3/2,3/2,3/2]
ratio_interval_class :: Integral i => Ratio i -> Ratio i
ratio_interval_class i =
    let f = fold_ratio_to_octave
    in max (f i) (f (recip i))

-- | Derivative harmonic series, based on /k/th partial of /f1/.
--
-- > import Music.Theory.Pitch
--
-- > let {r = [52,103,155,206,258,309,361,412,464,515,567,618,670,721,773]
-- >     ;d = harmonic_series_cps_derived 5 (octpc_to_cps (1,4))}
-- > in map round (take 15 d) == r
harmonic_series_cps_derived :: (Ord a, Fractional a, Enum a) => Int -> a -> [a]
harmonic_series_cps_derived k f1 =
    let f0 = fold_cps_to_octave_of f1 (partial f1 k)
    in harmonic_series_cps f0

-- | Harmonic series to /n/th harmonic (folded).
--
-- > harmonic_series_folded 17 == [1,17/16,9/8,5/4,11/8,3/2,13/8,7/4,15/8]
--
-- > let r = [0,105,204,386,551,702,841,969,1088]
-- > in map (round . ratio_to_cents) (harmonic_series_folded 17) == r
harmonic_series_folded :: Integer -> [Rational]
harmonic_series_folded n =
    nub (sort (map fold_ratio_to_octave [1 .. n%1]))

-- | 'ratio_to_cents' variant of 'harmonic_series_folded'.
--
-- > map round (harmonic_series_folded_c 21) == [0,105,204,298,386,471,551,702,841,969,1088]
harmonic_series_folded_c :: Integer -> [Cents]
harmonic_series_folded_c = map ratio_to_cents . harmonic_series_folded

-- | @12@-tone tuning of first @21@ elements of the harmonic series.
--
-- > cents_i harmonic_series_folded_21 == [0,105,204,298,386,471,551,702,841,969,1088]
-- > divisions harmonic_series_folded_21 == 11
harmonic_series_folded_21 :: Tuning
harmonic_series_folded_21 = Tuning (Left (harmonic_series_folded 21)) 2

-- * Cents

-- | Give cents difference from nearest 12ET tone.
--
-- > let r = [50,-49,-2,0,2,49,50]
-- > in map cents_et12_diff [650,651,698,700,702,749,750] == r
cents_et12_diff :: Integral n => n -> n
cents_et12_diff n =
    let m = n `mod` 100
    in if m > 50 then m - 100 else m

-- | Fractional form of 'cents_et12_diff'.
fcents_et12_diff :: Real n => n -> n
fcents_et12_diff n =
    let m = n `mod'` 100
    in if m > 50 then m - 100 else m

-- | The class of cents intervals has range @(0,600)@.
--
-- > map cents_interval_class [50,1150,1250] == [50,50,50]
--
-- > let r = concat [[0,50 .. 550],[600],[550,500 .. 0]]
-- > in map cents_interval_class [1200,1250 .. 2400] == r
cents_interval_class :: Integral a => a -> a
cents_interval_class n =
    let n' = n `mod` 1200
    in if n' > 600 then 1200 - n' else n'

-- | Fractional form of 'cents_interval_class'.
fcents_interval_class :: Real a => a -> a
fcents_interval_class n =
    let n' = n `mod'` 1200
    in if n' > 600 then 1200 - n' else n'

-- | Always include the sign, elide @0@.
cents_diff_pp :: (Num a, Ord a, Show a) => a -> String
cents_diff_pp n =
    case compare n 0 of
      LT -> show n
      EQ -> ""
      GT -> '+' : show n

-- | Given brackets, print cents difference.
cents_diff_br :: (Num a, Ord a, Show a) => (String,String) -> a -> String
cents_diff_br br =
    let f s = if null s then s else bracket_l br s
    in f . cents_diff_pp

-- | 'cents_diff_br' with parentheses.
--
-- > map cents_diff_text [-1,0,1] == ["(-1)","","(+1)"]
cents_diff_text :: (Num a, Ord a, Show a) => a -> String
cents_diff_text = cents_diff_br ("(",")")

-- | 'cents_diff_br' with markdown superscript (@^@).
cents_diff_md :: (Num a, Ord a, Show a) => a -> String
cents_diff_md = cents_diff_br ("^","^")

-- | 'cents_diff_br' with HTML superscript (@<sup>@).
cents_diff_html :: (Num a, Ord a, Show a) => a -> String
cents_diff_html = cents_diff_br ("<SUP>","</SUP>")

-- Local Variables:
-- truncate-lines:t
-- End:
