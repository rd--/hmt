-- | Clarence Barlow. \"Two Essays on Theory\".
-- /Computer Music Journal/, 11(1):44-60, 1987.
-- Translated by Henning Lohner.
module Music.Theory.Interval.Barlow_1987 where

import Data.List
import Data.Maybe
import Data.Numbers.Primes {- primes -}
import Data.Ratio
import Text.Printf

-- | Barlow's /indigestibility/ function for prime numbers.
--
-- > map barlow [1,2,3,5,7,11,13] == [0,1,8/3,32/5,72/7,200/11,288/13]
barlow :: (Integral a,Fractional b) => a -> b
barlow p =
    let p' = fromIntegral p
        square n = n * n
    in 2 * (square (p' - 1) / p')

-- | Generate list of factors of /n/ from /x/.
--
-- > factor primes 315 == [3,3,5,7]
factor :: Integral a => [a] -> a -> [a]
factor x n =
    case x of
      [] -> undefined
      i:x' -> if i * i > n
              then [n]
              else if rem n i == 0
                   then i : factor x (quot n i)
                   else factor x' n

-- | 'factor' /n/ from 'primes'.
--
-- > prime_factors 315 == [3,3,5,7]
prime_factors :: Integral a => a -> [a]
prime_factors = factor primes

-- | Collect number of occurences of each element of a sorted list.
--
-- > multiplicities [1,1,1,2,2,3] == [(1,3),(2,2),(3,1)]
multiplicities :: (Eq a,Integral n) => [a] -> [(a,n)]
multiplicities =
    let f x = case x of
                [] -> undefined
                e:_ -> (e,genericLength x)
    in map f . group

-- | 'multiplicities' '.' 'prime_factors'.
--
-- > prime_factors_m 315 == [(3,2),(5,1),(7,1)]
prime_factors_m :: Integral a => a -> [(a,a)]
prime_factors_m = multiplicities . prime_factors

-- | Merging function for 'rational_prime_factors_m'.
merge :: (Ord a,Num b,Eq b) => [(a,b)] -> [(a,b)] -> [(a,b)]
merge p q =
    case (p,q) of
      (_,[]) -> p
      ([],_) -> map (\(i,j) -> (i,-j)) q
      ((a,b):p',(c,d):q') ->
          if a < c
          then (a,b) : merge p' q
          else if a > c
               then (c,-d) : merge p q'
               else if b /= d
                    then (a,b-d) : merge p' q'
                    else merge p' q'

-- | Collect the prime factors in a rational number given as a
-- numerator/ denominator pair (n,m). Prime factors are listed in
-- ascending order with their positive or negative multiplicities,
-- depending on whether the prime factor occurs in the numerator or
-- the denominator (after cancelling out common factors).
--
-- > rational_prime_factors_m (16,15) == [(2,4),(3,-1),(5,-1)]
-- > rational_prime_factors_m (10,9) == [(2,1),(3,-2),(5,1)]
-- > rational_prime_factors_m (81,64) == [(2,-6),(3,4)]
-- > rational_prime_factors_m (27,16) == [(2,-4),(3,3)]
-- > rational_prime_factors_m (12,7) == [(2,2),(3,1),(7,-1)]
rational_prime_factors_m :: Integral b => (b,b) -> [(b,b)]
rational_prime_factors_m (n,m) =
    let n' = prime_factors_m n
        m' = prime_factors_m m
    in merge n' m'

-- | Variant of 'rational_prime_factors_m' giving results in a table
-- up to the /n/th prime.
--
-- > rational_prime_factors_t 6 (12,7) == [2,1,0,-1,0,0]
rational_prime_factors_t :: Integral b => Int -> (b,b) -> [b]
rational_prime_factors_t n x =
    let r = rational_prime_factors_m x
    in map (\i -> fromMaybe 0 (lookup i r)) (take n primes)

-- | Compute the disharmonicity of the interval /(p,q)/ using the
-- prime valuation function /pv/.
--
-- > map (disharmonicity barlow) [(9,10),(8,9)] ~= [12.733333,8.333333]
disharmonicity :: (Integral a,Num b) => (a -> b) -> (a,a) -> b
disharmonicity pv (p,q) =
    let n = rational_prime_factors_m (p,q)
    in sum [abs (fromIntegral j) * pv i | (i,j) <- n]

-- | The reciprocal of 'disharmonicity'.
--
-- > map (harmonicity barlow) [(9,10),(8,9)] ~= [0.078534,0.120000]
harmonicity :: (Integral a,Fractional b) => (a -> b) -> (a,a) -> b
harmonicity pv = recip . disharmonicity pv

-- | Variant of 'harmonicity' with 'Ratio' input.
harmonicity_r :: (Integral a,Fractional b) => (a -> b) -> Ratio a -> b
harmonicity_r pv = harmonicity pv . from_rational

-- | Interval ratio to cents.
--
-- > map cents [16%15,16%9] == [111.73128526977776,996.0899982692251]
cents :: (Real a,Floating b) => a -> b
cents x = 1200 * logBase 2 (realToFrac x)

-- | 'uncurry' ('%').
to_rational :: Integral a => (a,a) -> Ratio a
to_rational = uncurry (%)

-- | Make 'numerator' 'denominator' pair of /n/.
from_rational :: Integral t => Ratio t -> (t, t)
from_rational n = (numerator n,denominator n)

-- | Set of 1. interval size (cents), 2. intervals as product of
-- powers of primes, 3. frequency ratio and 4. harmonicity value.
type Table_2_Row = (Double,[Integer],Rational,Double)

-- | Table 2 (p.45)
--
-- > length (table_2 0.06) == 24
table_2 :: Double -> [Table_2_Row]
table_2 z =
    let g n = n <= 2 && n >= 1
        r = nub (sort (filter g [(p%q) | p <- [1..81],q <- [1..81]]))
        h = map (harmonicity_r barlow) r
        f = (> z) . snd
        k (i,j) = (cents i,rational_prime_factors_t 6 (from_rational i),i,j)
    in map k (filter f (zip r h))

-- | Pretty printer for 'Table_2_Row' values.
--
-- > mapM_ (putStrLn . table_2_pp) (table_2 0.06)
--
-- >    0.000 |  0  0  0  0  0  0 |  1:1  | Infinity
-- >  111.731 |  4 -1 -1  0  0  0 | 15:16 | 0.076531
-- >  182.404 |  1 -2  1  0  0  0 |  9:10 | 0.078534
-- >  203.910 | -3  2  0  0  0  0 |  8:9  | 0.120000
-- >  231.174 |  3  0  0 -1  0  0 |  7:8  | 0.075269
-- >  266.871 | -1 -1  0  1  0  0 |  6:7  | 0.071672
-- >  294.135 |  5 -3  0  0  0  0 | 27:32 | 0.076923
-- >  315.641 |  1  1 -1  0  0  0 |  5:6  | 0.099338
-- >  386.314 | -2  0  1  0  0  0 |  4:5  | 0.119048
-- >  407.820 | -6  4  0  0  0  0 | 64:81 | 0.060000
-- >  435.084 |  0  2  0 -1  0  0 |  7:9  | 0.064024
-- >  498.045 |  2 -1  0  0  0  0 |  3:4  | 0.214286
-- >  519.551 | -2  3 -1  0  0  0 | 20:27 | 0.060976
-- >  701.955 | -1  1  0  0  0  0 |  2:3  | 0.272727
-- >  764.916 |  1 -2  0  1  0  0 |  9:14 | 0.060172
-- >  813.686 |  3  0 -1  0  0  0 |  5:8  | 0.106383
-- >  884.359 |  0 -1  1  0  0  0 |  3:5  | 0.110294
-- >  905.865 | -4  3  0  0  0  0 | 16:27 | 0.083333
-- >  933.129 |  2  1  0 -1  0  0 |  7:12 | 0.066879
-- >  968.826 | -2  0  0  1  0  0 |  4:7  | 0.081395
-- >  996.090 |  4 -2  0  0  0  0 |  9:16 | 0.107143
-- > 1017.596 |  0  2 -1  0  0  0 |  5:9  | 0.085227
-- > 1088.269 | -3  1  1  0  0  0 |  8:15 | 0.082873
-- > 1200.000 |  1  0  0  0  0  0 |  1:2  | 1.000000
table_2_pp :: Table_2_Row -> String
table_2_pp (i,j,k,l) =
    let i' = printf "%8.3f" i
        j' = intercalate " " (map (printf "%2d") j)
        k' = let (p,q) = from_rational k in printf "%2d:%-2d" q p
        l' = printf "%1.6f" l
    in intercalate " | " [i',j',k',l']
