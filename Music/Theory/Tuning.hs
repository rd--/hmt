-- | Tuning theory
module Music.Theory.Tuning where

import qualified Data.Fixed as Fixed {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}

import qualified Music.Theory.Either as T {- hmt -}
import qualified Music.Theory.Function as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Math as T {- hmt -}
import qualified Music.Theory.Ord as T {- hmt -}
import qualified Music.Theory.Z as T {- hmt -}

-- * Math/Floating

-- | Convert from interval in cents to frequency ratio.
--
-- > map cents_to_fratio [0,701.9550008653874,1200] == [1,3/2,2]
-- > map cents_to_fratio [-1800,1800] -- three octaves about zero
cents_to_fratio :: Floating a => a -> a
cents_to_fratio n = 2 ** (n / 1200)

-- | Convert from a 'Floating' ratio to /cents/.
--
-- > let r = [0,498,702,1200]
-- > map (round . fratio_to_cents) [1,4/3,3/2,2] == r
fratio_to_cents :: (Real r,Floating n) => r -> n
fratio_to_cents = (1200 *) . logBase 2 . realToFrac

-- | Frequency /n/ cents from /f/.
--
-- > import Music.Theory.Pitch
-- > map (cps_shift_cents 440) [-100,100] == map octpc_to_cps [(4,8),(4,10)]
cps_shift_cents :: Floating a => a -> a -> a
cps_shift_cents f = (* f) . cents_to_fratio

-- | Interval in /cents/ from /p/ to /q/, ie. 'ratio_to_cents' of /p/ '/' /q/.
--
-- > cps_difference_cents 440 (octpc_to_cps (5,2)) == 500
--
-- > let abs_dif i j = abs (i - j)
-- > cps_difference_cents 440 (fmidi_to_cps 69.1) `abs_dif` 10 < 1e9
cps_difference_cents :: (Real r,Fractional r,Floating n) => r -> r -> n
cps_difference_cents p q = fratio_to_cents (q / p)

-- * Math/Ratio

-- | Convert a (signed) number of octaves difference of given ratio to a ratio.
--
-- > map (oct_diff_to_ratio 2) [-3 .. 3] == [1/8,1/4,1/2,1,2,4,8]
-- > map (oct_diff_to_ratio (9/8)) [-3 .. 3] == [512/729,64/81,8/9,1/1,9/8,81/64,729/512]
oct_diff_to_ratio :: Integral a => Ratio a -> Int -> Ratio a
oct_diff_to_ratio r n = if n >= 0 then T.recur_n n (* r) 1 else T.recur_n (negate n) (/ r) 1

-- | 'ratio_to_cents' rounded to nearest multiple of 100, modulo 12.
--
-- > map (ratio_to_pc 0) [1,4/3,3/2,2] == [0,5,7,0]
ratio_to_pc :: Int -> Rational -> Int
ratio_to_pc n = T.mod12 . (+ n) . round . (/ 100) . ratio_to_cents

-- | Fold ratio to lie within an octave, ie. @1@ '<' /n/ '<=' @2@.
--   It is an error for /n/ to be more than one octave outside of this range.
--
-- > map fold_ratio_to_octave_nonrec [2/3,3/4,4/5,4/7] == [4/3,3/2,8/5,8/7]
fold_ratio_to_octave_nonrec :: Integral i => Ratio i -> Ratio i
fold_ratio_to_octave_nonrec n =
  if n >= 1 && n < 2
  then n
  else if n >= 2 && n < 4
       then n / 2
       else if n < 1 && n >= (1/2)
            then n * 2
            else error "fold_ratio_to_octave_nonrec"

-- | Fold ratio until within an octave, ie. @1@ '<' /n/ '<=' @2@.
--   Diverges if /n/ is negative.
fold_ratio_to_octave_unsafe :: Integral i => Ratio i -> Ratio i
fold_ratio_to_octave_unsafe =
    let rec_f n = if n >= 2 then rec_f (n / 2) else if n < 1 then rec_f (n * 2) else n
    in rec_f

-- | Fold ratio until within an octave, ie. @1@ '<' /n/ '<=' @2@.
--
-- > map fold_ratio_to_octave [0,1] == [Nothing,Just 1]
fold_ratio_to_octave :: Integral i => Ratio i -> Maybe (Ratio i)
fold_ratio_to_octave n = if n <= 0 then Nothing else Just (fold_ratio_to_octave_unsafe n)

-- | Error if input is less than or equal to zero.
--
-- > map fold_ratio_to_octave_err [2/3,3/4,4/5,4/7] == [4/3,3/2,8/5,8/7]
fold_ratio_to_octave_err :: Integral i => Ratio i -> Ratio i
fold_ratio_to_octave_err = fromMaybe (error "fold_ratio_to_octave") . fold_ratio_to_octave

-- | The interval between two pitches /p/ and /q/ given as ratio
-- multipliers of a fundamental is /q/ '/' /p/.  The classes over such
-- intervals consider the 'fold_ratio_to_octave' of both /p/ to /q/
-- and /q/ to /p/ and select the minima at the /cmp_f/.
--
-- > map ratio_interval_class [2/3,3/2,3/4,4/3] == [3/2,3/2,3/2,3/2]
-- > map ratio_interval_class [7/6,12/7] == [7/6,7/6]
ratio_interval_class_by :: (Ord t, Integral i) => (Ratio i -> t) -> Ratio i -> Ratio i
ratio_interval_class_by cmp_f i =
    let f = fold_ratio_to_octave_err
    in T.min_by cmp_f (f i) (f (recip i))

-- | 'ratio_interval_class_by' 'ratio_nd_sum'
ratio_interval_class :: Integral i => Ratio i -> Ratio i
ratio_interval_class = ratio_interval_class_by T.ratio_nd_sum

-- * Types

-- | An approximation of a ratio.
type Approximate_Ratio = Double

-- | Type specialised 'fromRational'.
approximate_ratio :: Rational -> Approximate_Ratio
approximate_ratio = fromRational

-- | A real valued division of a semi-tone into one hundred parts, and
-- hence of the octave into @1200@ parts.
type Cents = Double

-- | Type specialised 'fratio_to_cents'.
approximate_ratio_to_cents :: Approximate_Ratio -> Cents
approximate_ratio_to_cents = fratio_to_cents

-- | 'approximate_ratio_to_cents' '.' 'approximate_ratio'.
--
-- > import Data.Ratio {- base -}
-- > map (\n -> (n,round (ratio_to_cents (fold_ratio_to_octave_err (n % 1))))) [1..21]
ratio_to_cents :: Integral i => Ratio i -> Cents
ratio_to_cents = approximate_ratio_to_cents . realToFrac

-- | Construct an exact 'Rational' that approximates 'Cents' to within /epsilon/.
--
-- > map (reconstructed_ratio 1e-5) [0,700,1200,1800] == [1,442/295,2,577/204]
--
-- > ratio_to_cents (442/295) == 699.9976981706735
reconstructed_ratio :: Double -> Cents -> Rational
reconstructed_ratio epsilon c = approxRational (cents_to_fratio c) epsilon

-- * Tuning

-- | A tuning specified 'Either' as a sequence of exact ratios, or as
-- a sequence of possibly inexact 'Cents'.
--
-- In both cases, the values are given in relation to the first degree
-- of the scale, which for ratios is 1 and for cents 0.
data Tuning = Tuning {tn_ratios_or_cents :: Either [Rational] [Cents]
                     ,tn_octave_ratio :: Rational}
              deriving (Eq,Show)

-- | Divisions of octave.
--
-- > tn_divisions (equal_temperament 12) == 12
tn_divisions :: Tuning -> Int
tn_divisions = either length length . tn_ratios_or_cents

-- | 'Maybe' exact ratios of 'Tuning'.
tn_ratios :: Tuning -> Maybe [Rational]
tn_ratios = T.from_left . tn_ratios_or_cents

-- | 'error'ing variant.
tn_ratios_err :: Tuning -> [Rational]
tn_ratios_err = fromMaybe (error "ratios") . tn_ratios

-- | Possibly inexact 'Cents' of tuning.
tn_cents :: Tuning -> [Cents]
tn_cents = either (map ratio_to_cents) id . tn_ratios_or_cents

-- | 'map' 'round' '.' 'cents'.
tn_cents_i :: Integral i => Tuning -> [i]
tn_cents_i = map round . tn_cents

-- | Variant of 'cents' that includes octave at right.
tn_cents_octave :: Tuning -> [Cents]
tn_cents_octave t = tn_cents t ++ [ratio_to_cents (tn_octave_ratio t)]

-- | Possibly inexact 'Approximate_Ratio's of tuning.
tn_approximate_ratios :: Tuning -> [Approximate_Ratio]
tn_approximate_ratios =
    either (map approximate_ratio) (map cents_to_fratio) .
    tn_ratios_or_cents

-- | Cyclic form, taking into consideration 'octave_ratio'.
tn_approximate_ratios_cyclic :: Tuning -> [Approximate_Ratio]
tn_approximate_ratios_cyclic t =
    let r = tn_approximate_ratios t
        m = realToFrac (tn_octave_ratio t)
        g = iterate (* m) 1
        f n = map (* n) r
    in concatMap f g

-- | Lookup function that allows both negative & multiple octave indices.
--
-- > :l Music.Theory.Tuning.DB.Werckmeister
-- > let map_zip f l = zip l (map f l)
-- > map_zip (tn_ratios_lookup werckmeister_vi) [-24 .. 24]
tn_ratios_lookup :: Tuning -> Int -> Maybe Rational
tn_ratios_lookup t n =
    let (o,pc) = n `divMod` tn_divisions t
        o_ratio = oct_diff_to_ratio (tn_octave_ratio t) o
    in fmap (\r -> o_ratio * (r !! pc)) (tn_ratios t)

-- | Lookup function that allows both negative & multiple octave indices.
--
-- > map_zip (tn_approximate_ratios_lookup werckmeister_v) [-24 .. 24]
tn_approximate_ratios_lookup :: Tuning -> Int -> Approximate_Ratio
tn_approximate_ratios_lookup t n =
    let (o,pc) = n `divMod` tn_divisions t
        o_ratio = fromRational (oct_diff_to_ratio (tn_octave_ratio t) o)
    in o_ratio * ((tn_approximate_ratios t) !! pc)

-- | 'Maybe' exact ratios reconstructed from possibly inexact 'Cents'
-- of 'Tuning'.
--
-- > :l Music.Theory.Tuning.DB.Werckmeister
-- > let r = [1,17/16,9/8,13/11,5/4,4/3,7/5,3/2,11/7,5/3,16/9,15/8]
-- > tn_reconstructed_ratios 1e-2 werckmeister_iii == Just r
tn_reconstructed_ratios :: Double -> Tuning -> Maybe [Rational]
tn_reconstructed_ratios epsilon =
    fmap (map (reconstructed_ratio epsilon)) .
    T.from_right .
    tn_ratios_or_cents

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

-- | 12-tone equal temperament comma (ie. 12th root of 2).
--
-- > twelve_tone_equal_temperament_comma == 1.0594630943592953
twelve_tone_equal_temperament_comma :: (Floating a,Eq a) => a
twelve_tone_equal_temperament_comma = 12 `T.nth_root` 2

-- * Equal temperaments

-- | Make /n/ division equal temperament.
equal_temperament :: Integral n => n -> Tuning
equal_temperament n =
    let c = genericTake n [0,1200 / fromIntegral n ..]
    in Tuning (Right c) 2

-- | 12-tone equal temperament.
--
-- > tn_cents equal_temperament_12 == [0,100..1100]
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
-- > take 7 (map round (tn_cents equal_temperament_72)) == r
equal_temperament_72 :: Tuning
equal_temperament_72 = equal_temperament (72::Int)

-- | 96-tone equal temperament.
equal_temperament_96 :: Tuning
equal_temperament_96 = equal_temperament (96::Int)

-- * Cents

-- | Give cents difference from nearest 12ET tone.
--
-- > let r = [50,-49,-2,0,2,49,50]
-- > map cents_et12_diff [650,651,698,700,702,749,750] == r
cents_et12_diff :: Integral n => n -> n
cents_et12_diff n =
    let m = n `mod` 100
    in if m > 50 then m - 100 else m

-- | Fractional form of 'cents_et12_diff'.
fcents_et12_diff :: Real n => n -> n
fcents_et12_diff n =
    let m = n `Fixed.mod'` 100
    in if m > 50 then m - 100 else m

-- | The class of cents intervals has range @(0,600)@.
--
-- > map cents_interval_class [50,1150,1250] == [50,50,50]
--
-- > let r = concat [[0,50 .. 550],[600],[550,500 .. 0]]
-- > map cents_interval_class [1200,1250 .. 2400] == r
cents_interval_class :: Integral a => a -> a
cents_interval_class n =
    let n' = n `mod` 1200
    in if n' > 600 then 1200 - n' else n'

-- | Fractional form of 'cents_interval_class'.
fcents_interval_class :: Real a => a -> a
fcents_interval_class n =
    let n' = n `Fixed.mod'` 1200
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
    let f s = if null s then s else T.bracket_l br s
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

