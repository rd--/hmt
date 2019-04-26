-- | Show functions.
module Music.Theory.Show where

import Data.Char {- base -}
import Data.List {- base -}
import Data.Ratio {- base -}
import Numeric {- base -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Math as T {- hmt -}
import qualified Music.Theory.Math.Convert as T {- hmt -}

-- * DIFF

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

-- * RATIONAL

-- | Pretty printer for 'Rational' using @/@ and eliding denominators of @1@.
--
-- > map rational_pp [1,3/2,5/4,2] == ["1","3/2","5/4","2"]
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
    let (n,d) = T.rational_nd r
    in concat [show n,":",show d]

-- | Show rational to /n/ decimal places.
--
-- > let r = approxRational pi 1e-100
-- > r == 884279719003555 / 281474976710656
-- > show_rational_decimal 12 r == "3.141592653590"
-- > show_rational_decimal 3 (-100) == "-100.000"
show_rational_decimal :: Int -> Rational -> String
show_rational_decimal n = double_pp n . fromRational

-- * REAL

-- | Show /r/ as float to /k/ places.
--
-- > map (real_pp 4) [1,1.1,1.12,1.123,1.1234]
real_pp :: Real t => Int -> t -> String
real_pp k = realfloat_pp k . T.real_to_double

-- | Prints /n/ as integral or to at most /k/ decimal places.
--
-- > map (real_pp_trunc 4) [1,1.1,1.12,1.123,1.1234] == ["1","1.1","1.12","1.123","1.1234"]
real_pp_trunc :: Real t => Int -> t -> String
real_pp_trunc k n =
  if T.real_is_whole n
  then show (numerator (toRational n))
  else dropWhileEnd (== '0') (real_pp k n)

-- | Type specialised 'realfloat_pp'.
float_pp :: Int -> Float -> String
float_pp = realfloat_pp

-- | Type specialised 'realfloat_pp'.
double_pp :: Int -> Double -> String
double_pp = realfloat_pp

-- | Variant of 'showFFloat'.  The 'Show' instance for floats resorts
-- to exponential notation very readily.
--
-- > [show 0.01,realfloat_pp 2 0.01] == ["1.0e-2","0.01"]
realfloat_pp :: RealFloat a => Int -> a -> String
realfloat_pp k n = showFFloat (Just k) n ""

-- * BIN

-- | Read binary integer.
--
-- > unwords (map (show_bin Nothing) [0 .. 7]) == "0 1 10 11 100 101 110 111"
-- > unwords (map (show_bin (Just 3)) [0 .. 7]) == "000 001 010 011 100 101 110 111"
show_bin :: (Integral i,Show i) => Maybe Int -> i -> String
show_bin k n = (maybe id (\x -> T.pad_left '0' x) k) (showIntAtBase 2 intToDigit n "")
