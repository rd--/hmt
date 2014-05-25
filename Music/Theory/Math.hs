-- | Math functions.
module Music.Theory.Math where

import Data.Maybe {- base -}
import Data.Ratio {- base -}

-- | Real (alias for 'Double').
type R = Double

-- | <http://reference.wolfram.com/mathematica/ref/FractionalPart.html>
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
rational_nd :: Integral t => Ratio t -> (t,t)
rational_nd r = (numerator r,denominator r)

-- | Rational as a whole number, or 'Nothing'.
rational_whole :: Integral a => Ratio a -> Maybe a
rational_whole r = if denominator r == 1 then Just (numerator r) else Nothing

-- | Erroring variant.
rational_whole_err :: Integral a => Ratio a -> a
rational_whole_err = fromMaybe (error "rational_whole") . rational_whole
