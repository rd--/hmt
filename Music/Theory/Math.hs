-- | Math functions.
module Music.Theory.Math where

import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}

import qualified Music.Theory.Math.Convert as T

-- | 'mod' 5.
mod5 :: Integral i => i -> i
mod5 n = n `mod` 5

-- | 'mod' 7.
mod7 :: Integral i => i -> i
mod7 n = n `mod` 7

-- | 'mod' 12.
mod12 :: Integral i => i -> i
mod12 n = n `mod` 12

-- | 'mod' 16.
mod16 :: Integral i => i -> i
mod16 n = n `mod` 16

-- | <http://reference.wolfram.com/mathematica/ref/FractionalPart.html>
--   i.e. 'properFraction'
--
-- > integral_and_fractional_parts 1.5 == (1,0.5)
integral_and_fractional_parts :: (Integral i, RealFrac t) => t -> (i,t)
integral_and_fractional_parts = properFraction

-- | Type specialised.
integer_and_fractional_parts :: RealFrac t => t -> (Integer,t)
integer_and_fractional_parts = integral_and_fractional_parts

-- | <http://reference.wolfram.com/mathematica/ref/FractionalPart.html>
--
-- > import Sound.SC3.Plot {- hsc3-plot -}
-- > plot_p1_ln [map fractional_part [-2.0,-1.99 .. 2.0]]
fractional_part :: RealFrac a => a -> a
fractional_part = snd . integer_and_fractional_parts

-- | 'floor' of 'T.real_to_double'.
real_floor :: (Real r,Integral i)  => r -> i
real_floor = floor . T.real_to_double

-- | Type specialised 'real_floor'.
real_floor_int :: Real r => r -> Int
real_floor_int = real_floor

-- | 'round' of 'T.real_to_double'.
real_round :: (Real r,Integral i)  => r -> i
real_round = round . T.real_to_double

-- | Type specialised 'real_round'.
real_round_int :: Real r => r -> Int
real_round_int = real_round

-- | Type specialised 'round'
round_int :: RealFrac t => t -> Int
round_int = round

-- | Type-specialised 'fromIntegral'
from_integral_to_int :: Integral i => i -> Int
from_integral_to_int = fromIntegral

-- | Type-specialised 'id'
int_id :: Int -> Int
int_id = id

-- | Is /r/ zero to /k/ decimal places.
--
-- > map (flip zero_to_precision 0.00009) [4,5] == [True,False]
-- > map (zero_to_precision 4) [0.00009,1.00009] == [True,False]
zero_to_precision :: Real r => Int -> r -> Bool
zero_to_precision k r = real_floor_int (r * fromIntegral ((10::Int) ^ k)) == 0

-- | Is /r/ whole to /k/ decimal places.
--
-- > map (flip whole_to_precision 1.00009) [4,5] == [True,False]
whole_to_precision :: Real r => Int -> r -> Bool
whole_to_precision k = zero_to_precision k . fractional_part . T.real_to_double

-- | <http://reference.wolfram.com/mathematica/ref/SawtoothWave.html>
--
-- > plot_p1_ln [map sawtooth_wave [-2.0,-1.99 .. 2.0]]
sawtooth_wave :: RealFrac a => a -> a
sawtooth_wave n = n - floor_f n

-- | Predicate that is true if @n/d@ can be simplified, ie. where
-- 'gcd' of @n@ and @d@ is not @1@.
--
-- > map rational_simplifies [(2,3),(4,6),(5,7)] == [False,True,False]
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

-- | Sum of numerator & denominator.
ratio_nd_sum :: Num a => Ratio a -> a
ratio_nd_sum r = numerator r + denominator r

-- | Is /n/ a whole (integral) value.
--
-- > map real_is_whole [-1.0,-0.5,0.0,0.5,1.0] == [True,False,True,False,True]
real_is_whole :: Real n => n -> Bool
real_is_whole = (== 1) . denominator . toRational

-- | 'fromInteger' . 'floor'.
floor_f :: (RealFrac a, Num b) => a -> b
floor_f = fromInteger . floor

-- | Round /b/ to nearest multiple of /a/.
--
-- > map (round_to 0.25) [0,0.1 .. 1] == [0.0,0.0,0.25,0.25,0.5,0.5,0.5,0.75,0.75,1.0,1.0]
-- > map (round_to 25) [0,10 .. 100] == [0,0,25,25,50,50,50,75,75,100,100]
round_to :: RealFrac n => n -> n -> n
round_to a b = if a == 0 then b else floor_f ((b / a) + 0.5) * a

-- | Variant of 'recip' that checks input for zero.
--
-- > map recip [1,1/2,0,-1]
-- > map recip_m [1,1/2,0,-1] == [Just 1,Just 2,Nothing,Just (-1)]
recip_m :: (Eq a, Fractional a) => a -> Maybe a
recip_m x = if x == 0 then Nothing else Just (recip x)

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

-- * I = integral

-- | Integral square root (sqrt) function.
--
-- > map i_square_root [0,1,4,9,16,25,36,49,64,81,100] == [0 .. 10]
-- > map i_square_root [4 .. 16] == [2,2,2,2,2,3,3,3,3,3,3,3,4]
i_square_root :: Integral t => t -> t
i_square_root n =
    let babylon a =
            let b  = quot (a + quot n a) 2
            in if a > b then babylon b else a
    in case compare n 0 of
         GT -> babylon n
         EQ -> 0
         _ -> error "i_square_root: negative?"

-- * Interval

-- | (0,1) = {x | 0 < x < 1}
in_open_interval :: Ord a => (a, a) -> a -> Bool
in_open_interval (p,q) n = p < n && n < q

-- | [0,1] = {x | 0 ≤ x ≤ 1}
in_closed_interval :: Ord a => (a, a) -> a -> Bool
in_closed_interval (p,q) n = p <= n && n <= q

-- | (p,q] (0,1] = {x | 0 < x ≤ 1}
in_left_half_open_interval :: Ord a => (a, a) -> a -> Bool
in_left_half_open_interval (p,q) n = p < n && n <= q

-- | [p,q) [0,1) = {x | 0 ≤ x < 1}
in_right_half_open_interval :: Ord a => (a, a) -> a -> Bool
in_right_half_open_interval (p,q) n = p <= n && n < q

-- | Calculate /n/th root of /x/.
--
-- > 12 `nth_root` 2 == 1.0594630943592953
nth_root :: (Floating a,Eq a) => a -> a -> a
nth_root n x =
    let f (_,x0) = (x0, ((n - 1) * x0 + x / x0 ** (n - 1)) / n)
        eq = uncurry (==)
    in fst (until eq f (x, x/n))

-- | Arithmetic mean (average) of a list.
--
-- > map arithmetic_mean [[-3..3],[0..5],[1..5],[3,5,7],[7,7],[3,9,10,11,12]] == [0,2.5,3,5,7,9]
arithmetic_mean :: Fractional a => [a] -> a
arithmetic_mean x = sum x / fromIntegral (length x)

-- | Numerically stable mean
--
-- > map ns_mean [[-3..3],[0..5],[1..5],[3,5,7],[7,7],[3,9,10,11,12]] == [0,2.5,3,5,7,9]
ns_mean :: Floating a => [a] -> a
ns_mean =
    let f (m,n) x = (m + (x - m) / (n + 1),n + 1)
    in fst . foldl' f (0,0)

-- | Square of /n/.
--
-- > square 5 == 25
square :: Num a => a -> a
square n = n * n

-- | The totient function phi(n), also called Euler's totient function.
--
-- > import Sound.SC3.Plot {- hsc3-plot -}
-- > plot_p1_stp [map totient [1::Int .. 100]]
totient :: Integral i => i -> i
totient n = genericLength (filter (==1) (map (gcd n) [1..n]))

{- | The /n/-th order Farey sequence.

> farey 1 == [0,                                                                                    1]
> farey 2 == [0,                                        1/2,                                        1]
> farey 3 == [0,                        1/3,            1/2,            2/3,                        1]
> farey 4 == [0,                1/4,    1/3,            1/2,            2/3,    3/4,                1]
> farey 5 == [0,            1/5,1/4,    1/3,    2/5,    1/2,    3/5,    2/3,    3/4,4/5,            1]
> farey 6 == [0,        1/6,1/5,1/4,    1/3,    2/5,    1/2,    3/5,    2/3,    3/4,4/5,5/6,        1]
> farey 7 == [0,    1/7,1/6,1/5,1/4,2/7,1/3,    2/5,3/7,1/2,4/7,3/5,    2/3,5/7,3/4,4/5,5/6,6/7,    1]
> farey 8 == [0,1/8,1/7,1/6,1/5,1/4,2/7,1/3,3/8,2/5,3/7,1/2,4/7,3/5,5/8,2/3,5/7,3/4,4/5,5/6,6/7,7/8,1]
-}
farey :: Integral i => i -> [Ratio i]
farey n =
  let step (a,b,c,d) =
        if c > n
        then Nothing
        else let k = (n + b) `quot` d in Just (c % d, (c,d,k * c - a,k * d - b))
  in 0 : unfoldr step (0,1,1,n)

-- | The length of the /n/-th order Farey sequence.
--
-- > map farey_length [1 .. 12] == [2,3,5,7,11,13,19,23,29,33,43,47]
-- > map (length . farey) [1 .. 12] == map farey_length [1 .. 12]
farey_length :: Integral i => i -> i
farey_length n = if n == 0 then 1 else farey_length (n - 1) + totient n

-- | Function to generate the Stern-Brocot tree from an initial row.
--   '%' normalises so 1/0 cannot be written as a 'Rational', hence (n,d).
stern_brocot_tree_f :: Num n => [(n,n)] -> [[(n,n)]]
stern_brocot_tree_f =
   let med_f (n1,d1) (n2,d2) = (n1 + n2,d1 + d2)
       f x = concat (transpose [x, zipWith med_f x (tail x)])
   in iterate f

{- | The Stern-Brocot tree from (0/1,1/0).

> let t = stern_brocot_tree
> t !! 0 == [(0,1),(1,0)]
> t !! 1 == [(0,1),(1,1),(1,0)]
> t !! 2 == [(0,1),(1,2),(1,1),(2,1),(1,0)]
> t !! 3 == [(0,1),(1,3),(1,2),(2,3),(1,1),(3,2),(2,1),(3,1),(1,0)]

> map length (take 12 stern_brocot_tree) == [2,3,5,9,17,33,65,129,257,513,1025,2049] -- A000051
-}
stern_brocot_tree :: Num n => [[(n,n)]]
stern_brocot_tree = stern_brocot_tree_f [(0,1),(1,0)]

-- | Left-hand (rational) side of the the Stern-Brocot tree, ie, from (0/1,1/1).
stern_brocot_tree_lhs :: Num n => [[(n,n)]]
stern_brocot_tree_lhs = stern_brocot_tree_f [(0,1),(1,1)]

{- | 'stern_brocot_tree_f' as 'Ratio's, for finite subsets.

> let t = stern_brocot_tree_f_r [0,1]
> t !! 1 == [0,1/2,1]
> t !! 2 == [0,1/3,1/2,2/3,1]
> t !! 3 == [0,1/4,1/3,2/5,1/2,3/5,2/3,3/4,1]
> t !! 4 == [0,1/5,1/4,2/7,1/3,3/8,2/5,3/7,1/2,4/7,3/5,5/8,2/3,5/7,3/4,4/5,1]
-}
stern_brocot_tree_f_r :: Integral n => [Ratio n] -> [[Ratio n]]
stern_brocot_tree_f_r = map (map (uncurry (%))) . stern_brocot_tree_f . map rational_nd
