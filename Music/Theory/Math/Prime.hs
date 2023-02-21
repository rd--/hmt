-- | Prime number related functions.
module Music.Theory.Math.Prime where

import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}

import qualified Data.Numbers.Primes as Primes {- primes -}

import qualified Music.Theory.Function as Function {- hmt -}
import qualified Music.Theory.List as List {- hmt -}
import qualified Music.Theory.Math as Math {- hmt -}
import qualified Music.Theory.Unicode as Unicode {- hmt -}

-- | Alias for 'Primes.primes'.
--
-- > take 12 primes_list == [2,3,5,7,11,13,17,19,23,29,31,37]
primes_list :: Integral i => [i]
primes_list = Primes.primes

-- | Give zero-index of prime, or Nothing if value is not prime.
--
-- > map prime_k [2,3,5,7,11,13,17,19,23,29,31,37] == map Just [0 .. 11]
-- > map prime_k [1,4,6,8,9,10,12,14,15,16,18,20,21,22] == replicate 14 Nothing
prime_k :: Integral a => a -> Maybe Int
prime_k i = if Primes.isPrime i then Just (List.findIndex_err (== i) primes_list) else Nothing

-- | 'maybe' 'error' of 'prime_k'
--
-- > prime_k_err 13 == 5
prime_k_err :: Integral a => a -> Int
prime_k_err = fromMaybe (error "prime_k: not prime?") . prime_k

{- | Generate list of factors of /n/ from /x/.

> factor primes_list 315 == [3,3,5,7]
> Primes.primeFactors 315 == [3,3,5,7]

As a special case 1 gives the empty list.

> factor primes_list 1 == []
> Primes.primeFactors 1 == []
-}
factor :: Integral i => [i] -> i -> [i]
factor x n =
    case x of
      [] -> error "factor: null primes_list input"
      i:x' -> if n < i
              then [] -- ie. prime factors of 1...
              else if i * i > n
                   then [n]
                   else if rem n i == 0
                        then i : factor x (quot n i)
                        else factor x' n

-- | 'factor' of 'primes_list'.
--
-- > map prime_factors [-1,0,1] == [[],[],[]]
-- > map prime_factors [1,4,231,315] == [[],[2,2],[3,7,11],[3,3,5,7]]
-- > map Primes.primeFactors [1,4,231,315] == [[],[2,2],[3,7,11],[3,3,5,7]]
prime_factors :: Integral i => i -> [i]
prime_factors = factor primes_list

-- | 'maximum' of 'prime_factors'
--
-- > map prime_limit [243,125] == [3,5]
-- > map prime_limit [0,1] == [1,1]
prime_limit :: Integral i => i -> i
prime_limit x = if x < 2 then 1 else maximum (prime_factors x)

-- | Collect number of occurences of each element of a sorted list.
--
-- > multiplicities [1,1,1,2,2,3] == [(1,3),(2,2),(3,1)]
multiplicities :: Eq t => [t] -> [(t,Int)]
multiplicities = List.generic_histogram_by (==) Nothing

-- | Pretty printer for histogram (multiplicites).
--
-- > multiplicities_pp [(3,2),(5,1),(7,1)] == "3×2 5×1 7×1"
multiplicities_pp :: Show t => [(t,Int)] -> String
multiplicities_pp =
  let f (x,y) = show x ++ "×" ++ show y
  in unwords . map f

-- | 'multiplicities' of 'Primes.primeFactors'.
--
-- > prime_factors_m 1 == []
-- > prime_factors_m 315 == [(3,2),(5,1),(7,1)]
prime_factors_m :: Integral i => i -> [(i,Int)]
prime_factors_m = multiplicities . Primes.primeFactors

-- | 'multiplicities_pp' of 'prime_factors_m'.
prime_factors_m_pp :: (Show i,Integral i) => i -> String
prime_factors_m_pp = multiplicities_pp . prime_factors_m

-- | Prime factors of /n/ and /d/.
rat_prime_factors :: Integral i => (i,i) -> ([i],[i])
rat_prime_factors = Function.bimap1 Primes.primeFactors

-- | 'Ratio' variant of 'rat_prime_factors'
rational_prime_factors :: Integral i => Ratio i -> ([i],[i])
rational_prime_factors = rat_prime_factors . Math.rational_nd

{- | Variant that writes factors of numerator as positive and factors for denominator as negative.
     Sorted by absolute value.

> rat_prime_factors_sgn (3 * 5 * 7 * 11,1) == [3,5,7,11]
> rat_prime_factors_sgn (3 * 5,7 * 11) == [3,5,-7,-11]
> rat_prime_factors_sgn (3 * 7,5) == [3,-5,7]
-}
rat_prime_factors_sgn :: Integral i => (i,i) -> [i]
rat_prime_factors_sgn r = let (n,d) = rat_prime_factors r in sortOn abs (n ++ map negate d)

-- | Rational variant.
--
-- > rational_prime_factors_sgn (2 * 2 * 2 * 1/3 * 1/3 * 1/3 * 1/3 * 5) == [2,2,2,-3,-3,-3,-3,5]
rational_prime_factors_sgn :: Integral i => Ratio i -> [i]
rational_prime_factors_sgn = rat_prime_factors_sgn . Math.rational_nd

-- | The largest prime factor of n/d.
rat_prime_limit :: Integral i => (i,i) -> i
rat_prime_limit = uncurry max . Function.bimap1 prime_limit

-- | The largest prime factor of /n/.
--
-- > rational_prime_limit (243/125) == 5
rational_prime_limit :: Integral i => Ratio i -> i
rational_prime_limit = rat_prime_limit . Math.rational_nd

-- | Merge function for 'rat_prime_factors_m'
rat_pf_merge :: Ord t => [(t,Int)] -> [(t,Int)] -> [(t,Int)]
rat_pf_merge p q =
  case (p,q) of
    (_,[]) -> p
    ([],_) -> map (\(i,j) -> (i,-j)) q
    ((a,b):p',(c,d):q') ->
      if a < c
      then (a,b) : rat_pf_merge p' q
      else if a > c
           then (c,-d) : rat_pf_merge p q'
           else if b /= d
                then (a,b-d) : rat_pf_merge p' q'
                else rat_pf_merge p' q'

{- | Collect the prime factors in a rational number given as a
numerator/ denominator pair (n,m). Prime factors are listed in
ascending order with their positive or negative multiplicities,
depending on whether the prime factor occurs in the numerator or the
denominator (after cancelling out common factors).

> rat_prime_factors_m (1,1) == []
> rat_prime_factors_m (16,15) == [(2,4),(3,-1),(5,-1)]
> rat_prime_factors_m (10,9) == [(2,1),(3,-2),(5,1)]
> rat_prime_factors_m (81,64) == [(2,-6),(3,4)]
> rat_prime_factors_m (27,16) == [(2,-4),(3,3)]
> rat_prime_factors_m (12,7) == [(2,2),(3,1),(7,-1)]
> rat_prime_factors_m (5,31) == [(5,1),(31,-1)]
-}
rat_prime_factors_m :: Integral i => (i,i) -> [(i,Int)]
rat_prime_factors_m (n,d) = rat_pf_merge (prime_factors_m n) (prime_factors_m d)

-- | 'Ratio' variant of 'rat_prime_factors_m'
rational_prime_factors_m :: Integral i => Ratio i -> [(i,Int)]
rational_prime_factors_m = rat_prime_factors_m . Math.rational_nd

-- | Variant of 'rat_prime_factors_m' giving results in a list.
--
-- > rat_prime_factors_l (1,1) == []
-- > rat_prime_factors_l (2^5,9) == [5,-2]
-- > rat_prime_factors_l (2*2*3,7) == [2,1,0,-1]
-- > rat_prime_factors_l (3*3,11*13) == [0,2,0,0,-1,-1]
rat_prime_factors_l :: Integral i => (i,i) -> [Int]
rat_prime_factors_l x =
  case rat_prime_factors_m x of
    [] -> []
    r -> let lm = maximum (map fst r)
         in map (\i -> fromMaybe 0 (lookup i r)) (List.take_until (== lm) primes_list)

-- | 'Ratio' variant of 'rat_prime_factors_l'
--
-- > map rational_prime_factors_l [1/31,256/243] == [[0,0,0,0,0,0,0,0,0,0,-1],[8,-5]]
rational_prime_factors_l :: Integral i => Ratio i -> [Int]
rational_prime_factors_l = rat_prime_factors_l . Math.rational_nd

-- | Variant of 'rational_prime_factors_l' padding table to /k/ places.
--   It is an error for /k/ to indicate a prime less than the limit of /x/.
--
-- > map (rat_prime_factors_t 6) [(5,13),(12,7)] == [[0,0,1,0,0,-1],[2,1,0,-1,0,0]]
-- > rat_prime_factors_t 3 (9,7) == undefined
rat_prime_factors_t :: (Integral i,Show i) => Int -> (i,i) -> [Int]
rat_prime_factors_t k = List.pad_right_err 0 k . rat_prime_factors_l

-- | 'Ratio' variant of 'rat_prime_factors_t'
rational_prime_factors_t :: (Integral i,Show i) => Int -> Ratio i -> [Int]
rational_prime_factors_t n = rat_prime_factors_t n . Math.rational_nd

-- | Condense factors list to include only indicated places.
--   It is an error if a deleted factor has a non-zero entry in the table.
--
-- > rat_prime_factors_l (12,7) == [2,1,0,-1]
-- > rat_prime_factors_c [2,3,5,7] (12,7) == [2,1,0,-1]
-- > rat_prime_factors_c [2,3,7] (12,7) == [2,1,-1]
rat_prime_factors_c :: (Integral i,Show i) => [i] -> (i,i) -> [Int]
rat_prime_factors_c fc r =
  let t = rat_prime_factors_l r
      k = map prime_k_err fc
      f (ix,e) = if ix `notElem` k
                 then (if e > 0 then error "rat_prime_factors_c: non-empty factor" else Nothing)
                 else Just e
  in mapMaybe f (zip [0..] t)

-- | 'Ratio' variant of 'rat_prime_factors_t'
--
-- > map (rational_prime_factors_c [3,5,31]) [3,5,31]
rational_prime_factors_c :: (Integral i,Show i) => [i] -> Ratio i -> [Int]
rational_prime_factors_c fc = rat_prime_factors_c fc . Math.rational_nd

-- | Pretty printer for prime factors.  sup=superscript ol=overline
prime_factors_pp :: [Integer] -> String
prime_factors_pp = intercalate [Unicode.middle_dot] . map show

{- | Pretty printer for prime factors.  sup=superscript ol=overline

> prime_factors_pp_sup_ol True [2,2,-3,5] == "2²·3̅·5"
> prime_factors_pp_sup_ol False [-2,-2,-2,3,3,5,5,5,5] == "-2³·3²·5⁴"
-}
prime_factors_pp_sup_ol :: Bool -> [Integer] -> String
prime_factors_pp_sup_ol ol =
  let mk x = if x < 0 && ol then Unicode.overline (show (- x)) else show x
      f x = let x0 = head x
                n = length x
            in if n == 1 then mk x0 else mk x0 ++ Unicode.int_show_superscript n
  in intercalate [Unicode.middle_dot] . map f . group
