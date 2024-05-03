{- | Erkki Kurenniemi.
Chords, scales, and divisor lattices (2003)
<https://beige.org/projects/dimi/CSDL2.pdf>
-}
module Music.Theory.Tuning.Kurenniemi2003 where

import Data.List {- base -}
import Data.Ratio {- base -}

import qualified Data.Numbers.Primes as Primes {- primes -}

import qualified Music.Theory.Geometry.Matrix as Matrix {- hmt-base -}

import qualified Music.Theory.Pitch as Pitch {- hmt -}

{- | In a divisibility network, two numbers are connected if they share a common divisor.

>>> [(i, j) | i <- [1..9], j <- [1 .. 9], i < j, areDivisible i j]
[(2,4),(2,6),(2,8),(3,6),(3,9),(4,6),(4,8),(6,8),(6,9)]
-}
areDivisible :: Integral a => a -> a -> Bool
areDivisible i j = gcd i j > 1

{- | Regular numbers are numbers whose only prime divisors are 2, 3, and 5, c.f. <https://oeis.org/A051037>

>>> [i | i <- [1 .. 60], isRegular i]
[1,2,3,4,5,6,8,9,10,12,15,16,18,20,24,25,27,30,32,36,40,45,48,50,54,60]

>>> [i | i <- [70 .. 100], isRegular i]
[72,75,80,81,90,96,100]
-}
isRegular :: Integral a => a -> Bool
isRegular i =
  let k = Primes.primeFactors i
  in all (`elem` [2, 3, 5]) k

{- | An n-smooth number is an integer whose prime factors are all less than or equal to n.

>>> [i | i <- [70 .. 100], isNSmooth 5 i]
[72,75,80,81,90,96,100]
-}
isNSmooth :: Integral a => a -> a -> Bool
isNSmooth n i =
  let k = Primes.primeFactors i
  in null k || maximum k <= n

-- | Given f0 in Hertz determine frequency of harmonic k.
kToHz :: Double -> Integer -> Double
kToHz f0 k = fromIntegral k * f0

-- | Given f0 in Hertz determine midi note number of harmonic k.
kToMidi :: Double -> Integer -> Integer
kToMidi f0 k = Pitch.cps_to_midi (kToHz f0 k)

-- | Given f0 in Hertz determine pitch of harmonic k.
kToPitch :: Double -> Integer -> Pitch.Pitch
kToPitch f0 k = Pitch.midi_to_pitch Pitch.pc_spell_ks (kToMidi f0 k)

{- | Given f0 in Hertz determine name of harmonic k.

>>> kToName 0.4 1
"Ab-6"

>>> kToName 32.70 1
"C1"
-}
kToName :: Double -> Integer -> String
kToName f0 k = Pitch.pitch_pp_iso (kToPitch f0 k)

-- | Name of k given f0 of C1.
kC1 :: Integer -> String
kC1 = kToName (Pitch.octpc_to_cps (1 :: Integer, 0))

-- | Pitch names of harmonics given f0 of C1.
c1Names :: [Integer] -> String
c1Names = unwords . map kC1

-- | Pitch names of divisors.
divisorsC1Names :: Integer -> String
divisorsC1Names = c1Names . divisors

{- | List of the integers that divide n.

>>> divisors 1729
[1,7,13,19,91,133,247,1729]

>>> divisors 60
[1,2,3,4,5,6,10,12,15,20,30,60]
-}
divisors :: Integral a => a -> [a]
divisors n = [x | x <- [1 .. n], n `rem` x == 0]

{- | Count the integers that divide n. (divisorSigma)

>>> numberOfDivisors 1729 == length (divisors 1729)
True

>>> numberOfDivisors 60
12

>>> 2^6 * 3^3 * 5^1
8640

>>> numberOfDivisors 8640
56

>>> 2^9 * 3^3 * 5^2
345600

>>> numberOfDivisors 345600
120
-}
numberOfDivisors :: Integral a => a -> Int
numberOfDivisors n = product (map ((+ 1) . length) (group (Primes.primeFactors n)))

-- | The tonal rotation matrix.
rotationMatrix :: Matrix.M33 Double
rotationMatrix =
  ( (0.335136, 0.531178, 0.778161)
  , (0.94217, -0.188943, -0.276797)
  , (0, -0.825924, 0.563781)
  )

-- | log of integer.
ilog :: Integer -> Double
ilog = log . fromIntegral

-- | Linear to linear rescaling
linlin :: Fractional n => (n, n) -> (n, n) -> n -> n
linlin (sl, sr) (dl, dr) n =
  let m = (dr - dl) / (sr - sl)
      a = dl - (m * sl)
  in n * m + a

{- | A more exact measure of majorness / minorness, the M-index

>>> mIndex [4, 5, 6]
-0.22047079493414568

>>> mIndex [5, 6, 8]
-0.23681136625736124

>>> mIndex [6, 8, 10]
-0.3333333333333332

>>> mIndex [10, 12, 15]
0.22047079493414592

>>> mIndex [2, 3, 5]
-0.3333333333333333

>>> mIndex [1 .. 6]
-0.464362682718022

>>> mIndex [8, 10, 12, 15]
1.1102230246251565e-16

>>> mIndex [40, 48, 60, 75]
0.1263568442719089

>>> mIndex [36, 45, 54, 64]
-0.14288515298055404

>>> mIndex [1 .. 1000]
-1.0
-}
mIndex :: [Integer] -> Double
mIndex c =
  let l = foldr1 lcm c
      g = foldr1 gcd c
      lg = ilog g
      ll = ilog l
      f = linlin (lg, ll) (-1, 1)
  in sum (map (f . ilog) c) / genericLength c

{- | One-indexed prime number, p(i) is the ith prime.

>>> map nthPrime [2, 3, 5, 7, 11]
[3,5,11,17,31]
-}
nthPrime :: (Integral i, Integral p) => i -> p
nthPrime i = Primes.primes `genericIndex` (i - 1)

{- | e(i) is the multiplicity of p(i) in the prime factorization of f.

>>> map (\f -> map (nthMultiplicity f) [1 .. 3]) (divisors 8640)
[[0,0,0],[1,0,0],[0,1,0],[2,0,0],[0,0,1],[1,1,0],[3,0,0],[0,2,0],[1,0,1],[2,1,0],[0,1,1],[4,0,0],[1,2,0],[2,0,1],[3,1,0],[0,3,0],[1,1,1],[5,0,0],[2,2,0],[3,0,1],[0,2,1],[4,1,0],[1,3,0],[2,1,1],[6,0,0],[3,2,0],[4,0,1],[1,2,1],[5,1,0],[2,3,0],[3,1,1],[0,3,1],[4,2,0],[5,0,1],[2,2,1],[6,1,0],[3,3,0],[4,1,1],[1,3,1],[5,2,0],[6,0,1],[3,2,1],[4,3,0],[5,1,1],[2,3,1],[6,2,0],[4,2,1],[5,3,0],[6,1,1],[3,3,1],[5,2,1],[6,3,0],[4,3,1],[6,2,1],[5,3,1],[6,3,1]]
-}
nthMultiplicity :: (Integral f, Integral i, Integral r) => f -> i -> r
nthMultiplicity f i =
  let p = nthPrime i
  in genericLength (filter (== p) (Primes.primeFactors f))

-- | Is i/j an integer, and is the integer prime.
rationalDividesImmediately :: Rational -> Rational -> Bool
rationalDividesImmediately i j =
  let r = i / j
  in denominator r == 1 && Primes.isPrime (numerator r)

{- | i/j is prime

>>> length [(i, j) | i <- divisors 60, j <- divisors 60, i < j, dividesImmediately j i]
20
-}
dividesImmediately :: Integer -> Integer -> Bool
dividesImmediately i j = rationalDividesImmediately (i % 1) (j % 1)
