-- | Tuning theory
module Music.Theory.Tuning where

import Data.Fixed (mod') {- base -}
import Data.List {- base -}
import qualified Data.Map as M {- containers -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}
import Safe {- safe -}

import qualified Music.Theory.Either as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Map as T {- hmt -}
import qualified Music.Theory.Pitch as T {- hmt -}
import qualified Music.Theory.Set.List as T {- hmt -}
import qualified Music.Theory.Tuple as T {- hmt -}

-- * Types

-- | An approximation of a ratio.
type Approximate_Ratio = Double

-- | A real valued division of a semi-tone into one hundred parts, and
-- hence of the octave into @1200@ parts.
type Cents = Double

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
tn_ratios = T.fromLeft . tn_ratios_or_cents

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

-- | Convert from interval in cents to frequency ratio.
--
-- > map cents_to_ratio [0,701.9550008653874,1200] == [1,3/2,2]
cents_to_ratio :: Floating a => a -> a
cents_to_ratio n = 2 ** (n / 1200)

-- | Possibly inexact 'Approximate_Ratio's of tuning.
tn_approximate_ratios :: Tuning -> [Approximate_Ratio]
tn_approximate_ratios =
    either (map approximate_ratio) (map cents_to_ratio) .
    tn_ratios_or_cents

-- | Cyclic form, taking into consideration 'octave_ratio'.
tn_approximate_ratios_cyclic :: Tuning -> [Approximate_Ratio]
tn_approximate_ratios_cyclic t =
    let r = tn_approximate_ratios t
        m = realToFrac (tn_octave_ratio t)
        g = iterate (* m) 1
        f n = map (* n) r
    in concatMap f g

-- | Iterate the function /f/ /n/ times, the inital value is /x/.
--
-- > recur_n 5 (* 2) 1 == 32
-- > take (5 + 1) (iterate (* 2) 1) == [1,2,4,8,16,32]
recur_n :: Integral n => n -> (t -> t) -> t -> t
recur_n n f x = if n < 1 then x else recur_n (n - 1) f (f x)

-- | Convert a (signed) number of octaves difference of given ratio to a ratio.
--
-- > map (oct_diff_to_ratio 2) [-3 .. 3] == [1/8,1/4,1/2,1,2,4,8]
-- > map (oct_diff_to_ratio (9/8)) [-3 .. 3] == [512/729,64/81,8/9,1/1,9/8,81/64,729/512]
oct_diff_to_ratio :: Integral a => Ratio a -> Int -> Ratio a
oct_diff_to_ratio r n = if n >= 0 then recur_n n (* r) 1 else recur_n (negate n) (/ r) 1

-- | Lookup function that allows both negative & multiple octave indices.
--
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
-- > :l Music.Theory.Tuning.Werckmeister
-- > let r = [1,17/16,9/8,13/11,5/4,4/3,7/5,3/2,11/7,5/3,16/9,15/8]
-- > tn_reconstructed_ratios 1e-2 werckmeister_iii == Just r
tn_reconstructed_ratios :: Double -> Tuning -> Maybe [Rational]
tn_reconstructed_ratios epsilon =
    fmap (map (reconstructed_ratio epsilon)) .
    T.fromRight .
    tn_ratios_or_cents

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
--
-- > map (\n -> (n,round (ratio_to_cents (fold_ratio_to_octave_err (n % 1))))) [1..21]
ratio_to_cents :: Integral i => Ratio i -> Cents
ratio_to_cents = approximate_ratio_to_cents . realToFrac

-- | Construct an exact 'Rational' that approximates 'Cents' to within
-- /epsilon/.
--
-- > map (reconstructed_ratio 1e-5) [0,700,1200] == [1,442/295,2]
--
-- > ratio_to_cents (442/295) == 699.9976981706735
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

-- | 96-tone equal temperament.
equal_temperament_96 :: Tuning
equal_temperament_96 = equal_temperament (96::Int)

-- * Harmonic series

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
partial f1 k = harmonic_series_cps f1 `at` (k - 1)

fold_ratio_to_octave' :: Integral i => Ratio i -> Ratio i
fold_ratio_to_octave' =
    let rec_f n = if n >= 2 then rec_f (n / 2) else if n < 1 then rec_f (n * 2) else n
    in rec_f

-- | Error if input is less than or equal to zero.
--
-- > map fold_ratio_to_octave_err [2/3,3/4] == [4/3,3/2]
fold_ratio_to_octave_err :: Integral i => Ratio i -> Ratio i
fold_ratio_to_octave_err n =
    if n <= 0
    then error "fold_ratio_to_octave"
    else fold_ratio_to_octave' n

-- | Fold ratio until within an octave, ie. @1@ '<' /n/ '<=' @2@.
--
-- > map fold_ratio_to_octave [0,1] == [Nothing,Just 1]
fold_ratio_to_octave :: Integral i => Ratio i -> Maybe (Ratio i)
fold_ratio_to_octave n = if n <= 0 then Nothing else Just (fold_ratio_to_octave' n)

-- | Sun of numerator & denominator.
ratio_nd_sum :: Num a => Ratio a -> a
ratio_nd_sum r = numerator r + denominator r

min_by :: Ord a => (t -> a) -> t -> t -> t
min_by f p q = if f p <= f q then p else q

-- | The interval between two pitches /p/ and /q/ given as ratio
-- multipliers of a fundamental is /q/ '/' /p/.  The classes over such
-- intervals consider the 'fold_ratio_to_octave' of both /p/ to /q/
-- and /q/ to /p/.
--
-- > map ratio_interval_class [2/3,3/2,3/4,4/3] == [3/2,3/2,3/2,3/2]
-- > map ratio_interval_class [7/6,12/7] == [7/6,7/6]
ratio_interval_class :: Integral i => Ratio i -> Ratio i
ratio_interval_class i =
    let f = fold_ratio_to_octave_err
    in min_by ratio_nd_sum (f i) (f (recip i))

-- | Derivative harmonic series, based on /k/th partial of /f1/.
--
-- > import Music.Theory.Pitch
--
-- > let {r = [52,103,155,206,258,309,361,412,464,515,567,618,670,721,773]
-- >     ;d = harmonic_series_cps_derived 5 (octpc_to_cps (1,4))}
-- > in map round (take 15 d) == r
harmonic_series_cps_derived :: (Ord a, Fractional a, Enum a) => Int -> a -> [a]
harmonic_series_cps_derived k f1 =
    let f0 = T.cps_in_octave_above f1 (partial f1 k)
    in harmonic_series_cps f0

-- | Harmonic series to /n/th harmonic (folded, duplicated removed).
--
-- > harmonic_series_folded_r 17 == [1,17/16,9/8,5/4,11/8,3/2,13/8,7/4,15/8]
--
-- > let r = [0,105,204,386,551,702,841,969,1088]
-- > in map (round . ratio_to_cents) (harmonic_series_folded_r 17) == r
harmonic_series_folded_r :: Integer -> [Rational]
harmonic_series_folded_r n = nub (sort (map fold_ratio_to_octave_err [1 .. n%1]))

-- | 'ratio_to_cents' variant of 'harmonic_series_folded'.
harmonic_series_folded_c :: Integer -> [Cents]
harmonic_series_folded_c = map ratio_to_cents . harmonic_series_folded_r

harmonic_series_folded :: Integer -> Rational -> Tuning
harmonic_series_folded n o = Tuning (Left (harmonic_series_folded_r n)) o

-- | @12@-tone tuning of first @21@ elements of the harmonic series.
--
-- > cents_i harmonic_series_folded_21 == [0,105,204,298,386,471,551,702,841,969,1088]
-- > divisions harmonic_series_folded_21 == 11
harmonic_series_folded_21 :: Tuning
harmonic_series_folded_21 = harmonic_series_folded 21 2

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

-- * Midi

-- | (/n/ -> /dt/).  Function from midi note number /n/ to
-- 'Midi_Detune' /dt/.  The incoming note number is the key pressed,
-- which may be distant from the note sounded.
type Midi_Tuning_F = Int -> T.Midi_Detune

-- | Variant for tunings that are incomplete.
type Sparse_Midi_Tuning_F = Int -> Maybe T.Midi_Detune

-- | Variant for sparse tunings that require state.
type Sparse_Midi_Tuning_ST_F st = st -> Int -> (st,Maybe T.Midi_Detune)

-- | Lift 'Midi_Tuning_F' to 'Sparse_Midi_Tuning_F'.
lift_tuning_f :: Midi_Tuning_F -> Sparse_Midi_Tuning_F
lift_tuning_f tn_f = Just . tn_f

-- | Lift 'Sparse_Midi_Tuning_F' to 'Sparse_Midi_Tuning_ST_F'.
lift_sparse_tuning_f :: Sparse_Midi_Tuning_F -> Sparse_Midi_Tuning_ST_F st
lift_sparse_tuning_f tn_f st k = (st,tn_f k)

-- | (t,c,k) where t=tuning (must have 12 divisions of octave),
-- c=cents deviation (ie. constant detune offset), k=midi offset
-- (ie. value to be added to incoming midi note number).
type D12_Midi_Tuning = (Tuning,Cents,Int)

-- | 'Midi_Tuning_F' for 'D12_Midi_Tuning'.
--
-- > let f = d12_midi_tuning_f (equal_temperament 12,0,0)
-- > map f [0..127] == zip [0..127] (repeat 0)
d12_midi_tuning_f :: D12_Midi_Tuning -> Midi_Tuning_F
d12_midi_tuning_f (t,c_diff,k) n =
    let (_,pc) = T.midi_to_octpc (n + k)
        dt = zipWith (-) (tn_cents t) [0,100 .. 1200]
    in if tn_divisions t /= 12
       then error "d12_midi_tuning_f: not d12"
       else case dt `atMay` pc of
              Nothing -> error "d12_midi_tuning_f: pc?"
              Just c -> (n,c + c_diff)

-- | (t,f0,k,g) where t=tuning, f0=fundamental frequency, k=midi note
-- number for f0, g=gamut
type CPS_Midi_Tuning = (Tuning,Double,Int,Int)

-- | 'Midi_Tuning_F' for 'CPS_Midi_Tuning'.  The function is sparse, it is only
-- valid for /g/ values from /k/.
--
-- > let f = cps_midi_tuning_f (equal_temperament 72,T.midi_to_cps 59,59,72 * 4)
-- > map f [59 .. 59 + 72]
cps_midi_tuning_f :: CPS_Midi_Tuning -> Sparse_Midi_Tuning_F
cps_midi_tuning_f (t,f0,k,g) n =
    let r = tn_approximate_ratios_cyclic t
        m = take g (map (T.cps_to_midi_detune . (* f0)) r)
    in m `atMay` (n - k)

-- * Midi tuning tables.

-- | Midi-note-number -> CPS table, possibly sparse.
type MNN_CPS_Table = [(Int,Double)]

-- | Generates 'MNN_CPS_Table' given 'Midi_Tuning_F' with keys for all valid @MNN@.
--
-- > import Sound.SC3.Plot
-- > plot_p2_ln [map (fmap round) (gen_cps_tuning_tbl f)]
gen_cps_tuning_tbl :: Sparse_Midi_Tuning_F -> MNN_CPS_Table
gen_cps_tuning_tbl tn_f =
    let f n = case tn_f n of
                Just r -> Just (n,T.midi_detune_to_cps r)
                Nothing -> Nothing
    in mapMaybe f [0 .. 127]

-- * Derived (secondary) tuning table (DTT) lookup.

-- | Given an 'MNN_CPS_Table' /tbl/, a list of @CPS@ /c/, and a @MNN@ /m/
-- find the @CPS@ in /c/ that is nearest to the @CPS@ in /t/ for /m/.
dtt_lookup :: (Eq k, Num v, Ord v) => [(k,v)] -> [v] -> k -> (Maybe v,Maybe v)
dtt_lookup tbl cps n =
    let f = lookup n tbl
    in (f,fmap (T.find_nearest_err cps) f)

-- | Require table be non-sparse.
dtt_lookup_err :: (Eq k, Num v, Ord v) => [(k,v)] -> [v] -> k -> (k,v,v)
dtt_lookup_err tbl cps n =
    case dtt_lookup tbl cps n of
      (Just f,Just g) -> (n,f,g)
      _ -> error "dtt_lookup"

-- | Given two tuning tables generate the @dtt@ table.
gen_dtt_lookup_tbl :: MNN_CPS_Table -> MNN_CPS_Table -> MNN_CPS_Table
gen_dtt_lookup_tbl t0 t1 =
    let ix = [0..127]
        cps = sort (map (T.p3_third . dtt_lookup_err t0 (map snd t1)) ix)
    in zip ix cps

gen_dtt_lookup_f :: MNN_CPS_Table -> MNN_CPS_Table -> Midi_Tuning_F
gen_dtt_lookup_f t0 t1 =
    let m = M.fromList (gen_dtt_lookup_tbl t0 t1)
    in T.cps_to_midi_detune . T.map_ix_err m

-- * Euler-Fokker genus <http://www.huygens-fokker.org/microtonality/efg.html>

-- | Normal form, value with occurences count (ie. exponent in notation above).
type EFG i = [(i,Int)]

-- | Degree of EFG, ie. sum of exponents.
--
-- > efg_degree [(3,3),(7,2)] == 3 + 2
efg_degree :: EFG i -> Int
efg_degree = sum . map snd

-- | Number of tones of EFG, ie. product of increment of exponents.
--
-- > efg_tones [(3,3),(7,2)] == (3 + 1) * (2 + 1)
efg_tones :: EFG i -> Int
efg_tones = product . map ((+ 1) . snd)

-- | Collate a genus given as a multiset into standard form, ie. histogram.
--
-- > efg_collate [3,3,3,7,7] == [(3,3),(7,2)]
efg_collate :: Ord i => [i] -> EFG i
efg_collate = T.histogram . sort

{- | Factors of EFG given with co-ordinate of grid location.

> efg_factors [(3,3)]

> let r = [([0,0],[]),([0,1],[7]),([0,2],[7,7])
>         ,([1,0],[3]),([1,1],[3,7]),([1,2],[3,7,7])
>         ,([2,0],[3,3]),([2,1],[3,3,7]),([2,2],[3,3,7,7])
>         ,([3,0],[3,3,3]),([3,1],[3,3,3,7]),([3,2],[3,3,3,7,7])]
> in efg_factors [(3,3),(7,2)] == r

-}
efg_factors :: EFG i -> [([Int],[i])]
efg_factors efg =
    let k = map (\(_,n) -> [0 .. n]) efg
        k' = if length efg == 1
             then concatMap (map return) k
             else T.nfold_cartesian_product k
        z = map fst efg
        f ix = (ix,concat (zipWith (\n m -> replicate n (z !! m)) ix [0..]))
    in map f k'

{- | Ratios of EFG, taking /n/ as the 1:1 ratio, with indices, folded into one octave.

> let r = sort $ map snd $ efg_ratios 7 [(3,3),(7,2)]
> r == [1/1,9/8,8/7,9/7,21/16,189/128,3/2,27/16,12/7,7/4,27/14,63/32]
> map (round . ratio_to_cents) r == [0,204,231,435,471,675,702,906,933,969,1137,1173]

      0:         1/1          C          0.000 cents
      1:         9/8          D        203.910 cents
      2:         8/7          D+       231.174 cents
      3:         9/7          E+       435.084 cents
      4:        21/16         F-       470.781 cents
      5:       189/128        G-       674.691 cents
      6:         3/2          G        701.955 cents
      7:        27/16         A        905.865 cents
      8:        12/7          A+       933.129 cents
      9:         7/4          Bb-      968.826 cents
     10:        27/14         B+      1137.039 cents
     11:        63/32         C-      1172.736 cents
     12:         2/1          C       1200.000 cents

> let r' = sort $ map snd $ efg_ratios 5 [(5,2),(7,3)]
> r' == [1/1,343/320,35/32,49/40,5/4,343/256,7/5,49/32,8/5,1715/1024,7/4,245/128]
> map (round . ratio_to_cents) r' == [0,120,155,351,386,506,583,738,814,893,969,1124]

> let r'' = sort $ map snd $ efg_ratios 3 [(3,1),(5,1),(7,1)]
> r'' == [1/1,35/32,7/6,5/4,4/3,35/24,5/3,7/4]
> map (round . ratio_to_cents) r'' == [0,155,267,386,498,653,884,969]

> let c0 = [0,204,231,435,471,675,702,906,933,969,1137,1173,1200]
> let c1 = [0,120,155,351,386,506,583,738,814,893,969,1124,1200]
> let c2 = [0,155,267,386,498,653,884,969,1200]
> let f (c',y) = map (\x -> (x,y,x,y + 10)) c'
> map f (zip [c0,c1,c2] [0,20,40])

-}
efg_ratios :: Real r => Rational -> EFG r -> [([Int],Rational)]
efg_ratios n =
    let to_r = fold_ratio_to_octave_err . (/ n) . toRational . product
        f (ix,i) = (ix,to_r i)
    in map f . efg_factors

{- | Generate a line drawing, as a set of (x0,y0,x1,y1) 4-tuples.
     h=row height, m=distance of vertical mark from row edge, k=distance between rows

> let e = [[3,3,3],[3,3,5],[3,5,5],[3,5,7],[3,7,7],[5,5,5],[5,5,7],[3,3,7],[5,7,7],[7,7,7]]
> let e = [[3,3,3],[5,5,5],[7,7,7],[3,3,5],[3,5,5],[5,5,7],[5,7,7],[3,7,7],[3,3,7],[3,5,7]]
> let e' = map efg_collate e
> efg_diagram_set (round,25,4,75) e'

-}
efg_diagram_set :: (Enum n,Real n) => (Cents -> n,n,n,n) -> [EFG n] -> [(n,n,n,n)]
efg_diagram_set (to_f,h,m,k) e =
    let f = (++ [1200]) . sort . map (to_f . ratio_to_cents . snd) . efg_ratios 1
        g (c,y) = let y' = y + h
                      b = [(0,y,1200,y),(0,y',1200,y')]
                  in b ++ map (\x -> (x,y + m,x,y' - m)) c
    in concatMap g (zip (map f e) [0,k ..])
