-- | Math functions.
module Music.Theory.Math where

import Data.Maybe {- base -}
import Data.Ratio {- base -}
import Numeric {- base -}

import qualified Music.Theory.Math.Convert as T

-- | Real (alias for 'Double').
type R = Double

-- | <http://reference.wolfram.com/mathematica/ref/FractionalPart.html>
--
-- > integral_and_fractional_parts 1.5 == (1,0.5)
integral_and_fractional_parts :: (Integral i, RealFrac t) => t -> (i,t)
integral_and_fractional_parts n =
    if n >= 0
    then let n' = floor n in (n',n - fromIntegral n')
    else let n' = ceiling n in (n',n - fromIntegral n')

-- | Type specialised.
integer_and_fractional_parts :: RealFrac t => t -> (Integer,t)
integer_and_fractional_parts = integral_and_fractional_parts

-- | <http://reference.wolfram.com/mathematica/ref/FractionalPart.html>
--
-- > import Sound.SC3.Plot {- hsc3-plot -}
-- > plotTable1 (map fractional_part [-2.0,-1.99 .. 2.0])
fractional_part :: RealFrac a => a -> a
fractional_part = snd . integer_and_fractional_parts

-- | 'floor' of 'T.real_to_double'.
real_floor :: (Real r,Integral i)  => r -> i
real_floor = floor . T.real_to_double

-- | Type specialised 'real_floor'.
real_floor_int :: Real r => r -> Int
real_floor_int = real_floor

-- | Is /r/ zero to /k/ decimal places.
--
-- > map (flip zero_to_precision 0.00009) [4,5] == [True,False]
-- > zero_to_precision 4 1.00009 == False
zero_to_precision :: Real r => Int -> r -> Bool
zero_to_precision k r = real_floor_int (r * (fromIntegral ((10::Int) ^ k))) == 0

-- | Is /r/ whole to /k/ decimal places.
--
-- > map (flip whole_to_precision 1.00009) [4,5] == [True,False]
whole_to_precision :: Real r => Int -> r -> Bool
whole_to_precision k = zero_to_precision k . fractional_part . T.real_to_double

-- | <http://reference.wolfram.com/mathematica/ref/SawtoothWave.html>
--
-- > plotTable1 (map sawtooth_wave [-2.0,-1.99 .. 2.0])
sawtooth_wave :: RealFrac a => a -> a
sawtooth_wave n = n - fromInteger (floor n)

-- | Pretty printer for 'Rational' that elides denominators of @1@.
--
-- > map rational_pp [1,3/2,2] == ["1","3/2","2"]
rational_pp :: (Show a,Integral a) => Ratio a -> String
rational_pp r =
    let n = numerator r
        d = denominator r
    in if d == 1
       then show n
       else concat [show n,"/",show d]

-- | Pretty print ratio as @:@ separated integers.
--
-- > map ratio_pp [1,3/2,2] == ["1:1","3:2","2:1"]
ratio_pp :: Rational -> String
ratio_pp r =
    let (n,d) = rational_nd r
    in concat [show n,":",show d]

-- | Predicate that is true if @n/d@ can be simplified, ie. where
-- 'gcd' of @n@ and @d@ is not @1@.
--
-- > let r = [False,True,False]
-- > in map rational_simplifies [(2,3),(4,6),(5,7)] == r
rational_simplifies :: Integral a => (a,a) -> Bool
rational_simplifies (n,d) = gcd n d /= 1

-- | 'numerator' and 'denominator' of rational.
rational_nd :: Ratio t -> (t,t)
rational_nd r = (numerator r,denominator r)

-- | Rational as a whole number, or 'Nothing'.
rational_whole :: Integral a => Ratio a -> Maybe a
rational_whole r = if denominator r == 1 then Just (numerator r) else Nothing

-- | Erroring variant.
rational_whole_err :: Integral a => Ratio a -> a
rational_whole_err = fromMaybe (error "rational_whole") . rational_whole

-- | Show rational to /n/ decimal places.
--
-- > let r = approxRational pi 1e-100
-- > r == 884279719003555 / 281474976710656
-- > show_rational_decimal 12 r == "3.141592653590"
show_rational_decimal :: Int -> Rational -> String
show_rational_decimal n r =
    let d = round (abs r * 10^n)
        s = show (d :: Integer)
        s' = replicate (n - length s + 1) '0' ++ s
        (h, f) = splitAt (length s' - n) s'
    in  (if r < 0 then "-" else "") ++ h ++ "." ++ f

-- | Variant of 'showFFloat'.  The 'Show' instance for floats resorts
-- to exponential notation very readily.
--
-- > [show 0.01,realfloat_pp 2 0.01] == ["1.0e-2","0.01"]
realfloat_pp :: RealFloat a => Int -> a -> String
realfloat_pp k n = showFFloat (Just k) n ""

-- | Show /r/ as float to /k/ places.
real_pp :: Real t => Int -> t -> String
real_pp k t = showFFloat (Just k) (T.real_to_double t) ""

-- | Type specialised 'realfloat_pp'.
float_pp :: Int -> Float -> String
float_pp = realfloat_pp

-- | Type specialised 'realfloat_pp'.
double_pp :: Int -> Double -> String
double_pp = realfloat_pp

-- | Show /only/ positive and negative values, always with sign.
--
-- > map num_diff_str [-2,-1,0,1,2] == ["-2","-1","","+1","+2"]
-- > map show [-2,-1,0,1,2] == ["-2","-1","0","1","2"]
num_diff_str :: (Num a, Ord a, Show a) => a -> String
num_diff_str n =
    case compare n 0 of
      LT -> '-' : show (abs n)
      EQ -> ""
      GT -> '+' : show n

-- | 'fromInteger' . 'floor'.
floor' :: (RealFrac a, Num b) => a -> b
floor' = fromInteger . floor

-- | Round /b/ to nearest multiple of /a/.
--
-- > map (round_to 0.25) [0,0.1 .. 1] == [0.0,0.0,0.25,0.25,0.5,0.5,0.5,0.75,0.75,1.0,1.0]
-- > map (round_to 25) [0,10 .. 100] == [0,0,25,25,50,50,50,75,75,100,100]
round_to :: RealFrac n => n -> n -> n
round_to a b = if a == 0 then b else floor' ((b / a) + 0.5) * a

-- * One-indexed

-- | One-indexed 'mod' function.
--
-- > map (`oi_mod` 5) [1..10] == [1,2,3,4,5,1,2,3,4,5]
oi_mod :: Integral a => a -> a -> a
oi_mod n m = ((n - 1) `mod` m) + 1

-- | One-indexed 'divMod' function.
--
-- > map (`oi_divMod` 5) [1,3 .. 9] == [(0,1),(0,3),(0,5),(1,2),(1,4)]
oi_divMod :: Integral t => t -> t -> (t, t)
oi_divMod n m = let (i,j) = (n - 1) `divMod` m in (i,j + 1)
