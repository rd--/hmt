-- | Tuning type
module Music.Theory.Tuning.Type where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.Either as T {- hmt -}

import qualified Music.Theory.Tuning as T {- hmt -}

-- * Tuning

-- | A tuning specified 'Either' as a sequence of exact ratios, or as
-- a sequence of possibly inexact 'Cents', and an octave if not 2:1 or 1200.
--
-- In both cases, the values are given in relation to the first degree
-- of the scale, which for ratios is 1 and for cents 0.
data Tuning = Tuning {tn_ratios_or_cents :: Either [Rational] [T.Cents]
                     ,tn_octave :: Maybe (Either Rational T.Cents)}
              deriving (Eq,Show)

-- | Default epsilon for recovering ratios from cents.
tn_epsilon :: Double
tn_epsilon = 0.001

-- | Tuning value as rational, reconstructed if required.
tn_as_ratio :: Double -> Either Rational T.Cents -> Rational
tn_as_ratio epsilon = either id (T.reconstructed_ratio epsilon)

-- | Tuning value as cents.
tn_as_cents :: Either Rational T.Cents -> T.Cents
tn_as_cents = either T.ratio_to_cents id

-- | Tuning octave, defaulting to 2:1.
tn_octave_def :: Tuning -> Either Rational T.Cents
tn_octave_def = maybe (Left 2) id . tn_octave

-- | Tuning octave in cents.
tn_octave_cents :: Tuning -> T.Cents
tn_octave_cents = tn_as_cents . tn_octave_def

-- | Tuning octave as ratio cents.
tn_octave_ratio :: Double -> Tuning -> Rational
tn_octave_ratio epsilon = tn_as_ratio epsilon . tn_octave_def

-- | Divisions of octave.
--
-- > tn_divisions (tn_equal_temperament 12) == 12
tn_divisions :: Tuning -> Int
tn_divisions = either length length . tn_ratios_or_cents

-- | 'Maybe' exact ratios of 'Tuning', NOT including the octave.
tn_ratios :: Tuning -> Maybe [Rational]
tn_ratios = T.from_left . tn_ratios_or_cents

-- | 'error'ing variant.
tn_ratios_err :: Tuning -> [Rational]
tn_ratios_err = fromMaybe (error "ratios") . tn_ratios

-- | Possibly inexact 'Cents' of tuning, NOT including the octave.
tn_cents :: Tuning -> [T.Cents]
tn_cents = either (map T.ratio_to_cents) id . tn_ratios_or_cents

-- | 'map' 'round' '.' 'cents'.
tn_cents_i :: Integral i => Tuning -> [i]
tn_cents_i = map round . tn_cents

-- | Variant of 'tn_cents' that includes octave at right.
tn_cents_octave :: Tuning -> [T.Cents]
tn_cents_octave t = tn_cents t ++ [tn_octave_cents t]

-- | 'tn_cents' / 100
tn_fmidi :: Tuning -> [Double]
tn_fmidi = map (* 0.01) . tn_cents

-- | Possibly inexact 'Approximate_Ratio's of tuning.
tn_approximate_ratios :: Tuning -> [T.Approximate_Ratio]
tn_approximate_ratios =
    either (map T.approximate_ratio) (map T.cents_to_fratio) .
    tn_ratios_or_cents

-- | Cyclic form, taking into consideration 'octave_ratio'.
tn_approximate_ratios_cyclic :: Tuning -> [T.Approximate_Ratio]
tn_approximate_ratios_cyclic t =
    let r = tn_approximate_ratios t
        m = T.cents_to_fratio (tn_octave_cents t)
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
        o_ratio = T.oct_diff_to_ratio (tn_octave_ratio tn_epsilon t) o
    in fmap (\r -> o_ratio * (r !! pc)) (tn_ratios t)

-- | Lookup function that allows both negative & multiple octave indices.
--
-- > map_zip (tn_approximate_ratios_lookup werckmeister_v) [-24 .. 24]
tn_approximate_ratios_lookup :: Tuning -> Int -> T.Approximate_Ratio
tn_approximate_ratios_lookup t n =
    let (o,pc) = n `divMod` tn_divisions t
        o_ratio = fromRational (T.oct_diff_to_ratio (tn_octave_ratio tn_epsilon t) o)
    in o_ratio * ((tn_approximate_ratios t) !! pc)

-- | 'Maybe' exact ratios reconstructed from possibly inexact 'Cents'
-- of 'Tuning'.
--
-- > :l Music.Theory.Tuning.DB.Werckmeister
-- > let r = [1,17/16,9/8,13/11,5/4,4/3,7/5,3/2,11/7,5/3,16/9,15/8]
-- > tn_reconstructed_ratios 1e-2 werckmeister_iii == Just r
tn_reconstructed_ratios :: Double -> Tuning -> Maybe [Rational]
tn_reconstructed_ratios epsilon =
    fmap (map (T.reconstructed_ratio epsilon)) .
    T.from_right .
    tn_ratios_or_cents

-- * Equal temperaments

-- | Make /n/ division equal temperament.
tn_equal_temperament :: Integral n => n -> Tuning
tn_equal_temperament n =
    let c = genericTake n [0,1200 / fromIntegral n ..]
    in Tuning (Right c) Nothing

-- | 12-tone equal temperament.
--
-- > tn_cents tn_equal_temperament_12 == [0,100..1100]
tn_equal_temperament_12 :: Tuning
tn_equal_temperament_12 = tn_equal_temperament (12::Int)

-- | 19-tone equal temperament.
--
-- > let c = [0,63,126,189,253,316,379,442,505,568,632,695,758,821,884,947,1011,1074,1137]
-- > tn_cents_i tn_equal_temperament_19 == c
tn_equal_temperament_19 :: Tuning
tn_equal_temperament_19 = tn_equal_temperament (19::Int)

-- | 31-tone equal temperament.
tn_equal_temperament_31 :: Tuning
tn_equal_temperament_31 = tn_equal_temperament (31::Int)

-- | 53-tone equal temperament.
tn_equal_temperament_53 :: Tuning
tn_equal_temperament_53 = tn_equal_temperament (53::Int)

-- | 72-tone equal temperament.
--
-- > let r = [0,17,33,50,67,83,100]
-- > take 7 (map round (tn_cents tn_equal_temperament_72)) == r
tn_equal_temperament_72 :: Tuning
tn_equal_temperament_72 = tn_equal_temperament (72::Int)

-- | 96-tone equal temperament.
tn_equal_temperament_96 :: Tuning
tn_equal_temperament_96 = tn_equal_temperament (96::Int)

