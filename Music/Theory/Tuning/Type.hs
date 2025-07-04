-- | Tuning type
module Music.Theory.Tuning.Type where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.Either as Either {- hmt -}
import qualified Music.Theory.Math.Prime as Math.Prime {- hmt -}

import qualified Music.Theory.Tuning as Tuning {- hmt -}

-- * Tuning

{- | A tuning specified 'Either' as a sequence of exact ratios,
or as a sequence of possibly inexact 'Cents', and an octave if not 2:1 or 1200.

In both cases, the values are given in relation to the first degree of the scale,
which for ratios is 1 and for cents 0.
-}
data Tuning = Tuning
  { tn_ratios_or_cents :: Either [Rational] [Tuning.Cents]
  , tn_octave :: Maybe (Either Rational Tuning.Cents)
  }
  deriving (Eq, Show)

-- | Default epsilon for recovering ratios from cents.
tn_epsilon :: Double
tn_epsilon = 0.001

-- | Tuning value as rational, reconstructed if required.
tn_as_ratio :: Double -> Either Rational Tuning.Cents -> Rational
tn_as_ratio epsilon = either id (Tuning.reconstructed_ratio epsilon)

-- | Tuning value as cents.
tn_as_cents :: Either Rational Tuning.Cents -> Tuning.Cents
tn_as_cents = either Tuning.ratio_to_cents id

-- | Tuning octave, defaulting to 2:1.
tn_octave_def :: Tuning -> Either Rational Tuning.Cents
tn_octave_def = fromMaybe (Left 2) . tn_octave

-- | Tuning octave in cents.
tn_octave_cents :: Tuning -> Tuning.Cents
tn_octave_cents = tn_as_cents . tn_octave_def

-- | Tuning octave as ratio cents.
tn_octave_ratio :: Double -> Tuning -> Rational
tn_octave_ratio epsilon = tn_as_ratio epsilon . tn_octave_def

{- | Divisions of octave.

>>> tn_divisions (tn_equal_temperament 12)
12
-}
tn_divisions :: Tuning -> Int
tn_divisions = either length length . tn_ratios_or_cents

-- | 'Maybe' exact ratios of 'Tuning', NOT including the octave.
tn_ratios :: Tuning -> Maybe [Rational]
tn_ratios = Either.from_left . tn_ratios_or_cents

-- | Limit of JI tuning.
tn_limit :: Tuning -> Maybe Integer
tn_limit = fmap (maximum . map Math.Prime.rational_prime_limit) . tn_ratios

-- | 'error'ing variant.
tn_ratios_err :: Tuning -> [Rational]
tn_ratios_err = fromMaybe (error "ratios") . tn_ratios

-- | Possibly inexact 'Cents' of tuning, NOT including the octave.
tn_cents :: Tuning -> [Tuning.Cents]
tn_cents = either (map Tuning.ratio_to_cents) id . tn_ratios_or_cents

-- | 'map' 'round' '.' 'cents'.
tn_cents_i :: Integral i => Tuning -> [i]
tn_cents_i = map round . tn_cents

-- | Variant of 'tn_cents' that includes octave at right.
tn_cents_octave :: Tuning -> [Tuning.Cents]
tn_cents_octave t = tn_cents t ++ [tn_octave_cents t]

-- | 'tn_cents' / 100
tn_fmidi :: Tuning -> [Double]
tn_fmidi = map (* 0.01) . tn_cents

-- | Possibly inexact 'Approximate_Ratio's of tuning.
tn_approximate_ratios :: Tuning -> [Tuning.Approximate_Ratio]
tn_approximate_ratios =
  either (map Tuning.approximate_ratio) (map Tuning.cents_to_fratio)
    . tn_ratios_or_cents

{- | Cyclic form, taking into consideration 'octave_ratio'.

>>> let r = tn_approximate_ratios_cyclic (tn_equal_temperament 12)
>>> map (round . Tuning.fratio_to_cents) (take 15 r)
[0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400]
-}
tn_approximate_ratios_cyclic :: Tuning -> [Tuning.Approximate_Ratio]
tn_approximate_ratios_cyclic t =
  let r = tn_approximate_ratios t
      m = Tuning.cents_to_fratio (tn_octave_cents t)
      g = iterate (* m) 1
      f n = map (* n) r
  in concatMap f g

-- | Lookup function that allows both negative & multiple octave indices.
tn_ratios_lookup :: Tuning -> Int -> Maybe Rational
tn_ratios_lookup t n =
  let (o, pc) = n `divMod` tn_divisions t
      o_ratio = Tuning.oct_diff_to_ratio (tn_octave_ratio tn_epsilon t) o
  in fmap (\r -> o_ratio * (r !! pc)) (tn_ratios t)

-- | Lookup function that allows both negative & multiple octave indices.
tn_approximate_ratios_lookup :: Tuning -> Int -> Tuning.Approximate_Ratio
tn_approximate_ratios_lookup t n =
  let (o, pc) = n `divMod` tn_divisions t
      o_ratio = fromRational (Tuning.oct_diff_to_ratio (tn_octave_ratio tn_epsilon t) o)
  in o_ratio * (tn_approximate_ratios t !! pc)

-- | 'Maybe' exact ratios reconstructed from possibly inexact 'Cents' of 'Tuning'.
tn_reconstructed_ratios :: Double -> Tuning -> Maybe [Rational]
tn_reconstructed_ratios epsilon =
  fmap (map (Tuning.reconstructed_ratio epsilon))
    . Either.from_right
    . tn_ratios_or_cents

-- * Equal temperaments

-- | Make /n/ division equal temperament.
tn_equal_temperament :: Integral n => n -> Tuning
tn_equal_temperament n =
  let c = genericTake n [0, 1200 / fromIntegral n ..]
  in Tuning (Right c) Nothing

{- | 12-tone equal temperament.

>>> tn_cents tn_equal_temperament_12 == [0,100..1100]
True
-}
tn_equal_temperament_12 :: Tuning
tn_equal_temperament_12 = tn_equal_temperament (12 :: Int)

{- | 19-tone equal temperament.

>>> tn_cents_i tn_equal_temperament_19
[0,63,126,189,253,316,379,442,505,568,632,695,758,821,884,947,1011,1074,1137]
-}
tn_equal_temperament_19 :: Tuning
tn_equal_temperament_19 = tn_equal_temperament (19 :: Int)

-- | 31-tone equal temperament.
tn_equal_temperament_31 :: Tuning
tn_equal_temperament_31 = tn_equal_temperament (31 :: Int)

-- | 53-tone equal temperament.
tn_equal_temperament_53 :: Tuning
tn_equal_temperament_53 = tn_equal_temperament (53 :: Int)

{- | 72-tone equal temperament.

>>> take 7 (map round (tn_cents tn_equal_temperament_72))
[0,17,33,50,67,83,100]
-}
tn_equal_temperament_72 :: Tuning
tn_equal_temperament_72 = tn_equal_temperament (72 :: Int)

-- | 96-tone equal temperament.
tn_equal_temperament_96 :: Tuning
tn_equal_temperament_96 = tn_equal_temperament (96 :: Int)
