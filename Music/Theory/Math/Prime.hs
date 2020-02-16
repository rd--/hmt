-- | Prime number related functions.
module Music.Theory.Math.Prime where

import Data.Maybe {- base -}
import Data.Ratio {- base -}

import qualified Data.Numbers.Primes as P {- primes -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Math as T {- hmt -}

-- | Alias for 'P.primes'.
--
-- > take 12 primes_list == [2,3,5,7,11,13,17,19,23,29,31,37]
primes_list :: Integral i => [i]
primes_list = P.primes

-- | Give zero-index of prime.
--
-- map prime_k [2,3,5,7,11,13,17,19,23,29,31,37] == [0 .. 11]
prime_k :: Integral a => a -> Int
prime_k i = T.findIndex_err (== i) P.primes

{- | Generate list of factors of /n/ from /x/.

> factor primes_list 315 == [3,3,5,7]
> P.primeFactors 315 == [3,3,5,7]

As a special case 1 gives the empty list.

> factor primes_list 1 == []
> P.primeFactors 1 == []
-}
factor :: Integral i => [i] -> i -> [i]
factor x n =
    case x of
      [] -> undefined
      i:x' -> if n < i
              then [] -- ie. prime factors of 1...
              else if i * i > n
                   then [n]
                   else if rem n i == 0
                        then i : factor x (quot n i)
                        else factor x' n

-- | 'factor' of 'primes_list'.
--
-- > prime_factors 1 == []
-- > map prime_factors [1,4,231,315] == [[],[2,2],[3,7,11],[3,3,5,7]]
-- > map P.primeFactors [1,4,231,315] == [[],[2,2],[3,7,11],[3,3,5,7]]
prime_factors :: Integral i => i -> [i]
prime_factors n = factor primes_list n

-- | Collect number of occurences of each element of a sorted list.
--
-- > multiplicities [1,1,1,2,2,3] == [(1,3),(2,2),(3,1)]
multiplicities :: Eq t => [t] -> [(t,Int)]
multiplicities = T.generic_histogram_by (==) Nothing

-- | Pretty printer for historgram (multiplicites).
--
-- > multiplicities_pp [(3,2),(5,1),(7,1)] == "3×2 5×1 7×1"
multiplicities_pp :: Show t => [(t,Int)] -> String
multiplicities_pp =
  let f (x,y) = show x ++ "×" ++ show y
  in unwords . map f

-- | 'multiplicities' of 'P.primeFactors'.
--
-- > prime_factors_m 1 == []
-- > prime_factors_m 315 == [(3,2),(5,1),(7,1)]
prime_factors_m :: Integral i => i -> [(i,Int)]
prime_factors_m = multiplicities . P.primeFactors

-- | 'multiplicities_pp' of 'prime_factors_m'.
prime_factors_m_pp :: (Show i,Integral i) => i -> String
prime_factors_m_pp = multiplicities_pp . prime_factors_m

-- | Prime factors of /n/ and /d/.
rat_prime_factors :: Integral i => (i,i) -> ([i],[i])
rat_prime_factors = T.bimap1 P.primeFactors

-- | 'Ratio' variant of 'rat_prime_factors'
rational_prime_factors :: Integral i => Ratio i -> ([i],[i])
rational_prime_factors = rat_prime_factors . T.rational_nd

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
-}
rat_prime_factors_m :: Integral i => (i,i) -> [(i,Int)]
rat_prime_factors_m (n,d) = rat_pf_merge (prime_factors_m n) (prime_factors_m d)

-- | 'Ratio' variant of 'rat_prime_factors_m'
rational_prime_factors_m :: Integral i => Ratio i -> [(i,Int)]
rational_prime_factors_m = rat_prime_factors_m . T.rational_nd

-- | Variant of 'rational_prime_factors_m' giving results in a table
-- up to the /n/th prime (one-indexed).
--
-- > rat_prime_factors_t 6 (1,1) == [0,0,0,0,0,0]
-- > rat_prime_factors_t 6 (12,7) == [2,1,0,-1,0,0]
-- > rat_prime_factors_t (prime_k 13 + 1) (32,9) == [5,-2,0,0,0,0]
rat_prime_factors_t :: (Integral i,Show i) => Int -> (i,i) -> [Int]
rat_prime_factors_t n x =
  let r = rat_prime_factors_m x
  in if null r
     then replicate n 0
     else if maximum (map fst r) > (primes_list !! (n - 1))
          then error (show ("rat_prime_factors_t:",n,x,r))
          else map (\i -> fromMaybe 0 (lookup i r)) (take n P.primes)

-- | 'Ratio' variant of 'rat_prime_factors_t'
--
-- > rational_prime_factors_t 3 (256/243) == [8,-5,0]
rational_prime_factors_t :: (Integral i,Show i) => Int -> Ratio i -> [Int]
rational_prime_factors_t n = rat_prime_factors_t n . T.rational_nd
