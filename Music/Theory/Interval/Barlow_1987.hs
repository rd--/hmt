-- | Clarence Barlow. \"Two Essays on Theory\".
-- /Computer Music Journal/, 11(1):44-60, 1987.
-- Translated by Henning Lohner.
module Music.Theory.Interval.Barlow_1987 where

import Data.List {- base -}
import Data.Ratio {- base -}
import Text.Printf {- base -}

import qualified Music.Theory.Math as T {- hmt -}
import qualified Music.Theory.Math.Prime as T {- hmt -}
import qualified Music.Theory.Tuning as T {- hmt -}

{- | Barlow's /indigestibility/ function for prime numbers.

>>> map barlow [1,2,3,5,7,11,13] == [0,1,8/3,32/5,72/7,200/11,288/13]
True
-}
barlow :: (Integral a,Fractional b) => a -> b
barlow p =
    let p' = fromIntegral p
        square n = n * n
    in 2 * (square (p' - 1) / p')

{- | Compute the disharmonicity of the interval /(p,q)/ using the prime valuation function /pv/.

>>> map (disharmonicity barlow) [(9,10),(8,9)] == ([12 + 11/15,8 + 1/3] :: [Rational])
True
-}
disharmonicity :: (Integral a,Num b) => (a -> b) -> (a,a) -> b
disharmonicity pv (p,q) =
    let n = T.rat_prime_factors_m (p,q)
    in sum [abs (fromIntegral j) * pv i | (i,j) <- n]

{- | The reciprocal of 'disharmonicity'.

>>> map (harmonicity barlow) [(9,10),(8,9),(2,1)] == ([15/191,3/25,1] :: [Rational])
True
-}
harmonicity :: (Integral a,Fractional b) => (a -> b) -> (a,a) -> b
harmonicity pv = recip . disharmonicity pv

harmonicity_m :: (Eq b,Integral a,Fractional b) => (a -> b) -> (a,a) -> Maybe b
harmonicity_m pv = T.recip_m . disharmonicity pv

{- | Variant of 'harmonicity' with 'Ratio' input.

>>> harmonicity_r barlow 1 == 1/0
True
-}
harmonicity_r :: (Integral a,Fractional b) => (a -> b) -> Ratio a -> b
harmonicity_r pv = harmonicity pv . T.rational_nd

-- | Variant of 'harmonicity_r' with output in (0,100), infinity maps to 100.
harmonicity_r_100 :: (RealFrac b, Integral a) => (a -> b) -> Ratio a -> Int
harmonicity_r_100 pv x =
  case harmonicity_m pv (T.rational_nd x) of
    Nothing -> 100
    Just y -> round (y * 100)

-- | Set of 1. interval size (cents), 2. intervals as product of
-- powers of primes, 3. frequency ratio and 4. harmonicity value.
type Table_2_Row = (Double,[Int],Rational,Double)

-- | Given ratio /r/ generate 'Table_2_Row'
mk_table_2_row :: Rational -> Table_2_Row
mk_table_2_row r =
  (T.fratio_to_cents r
  ,T.rat_prime_factors_t 6 (T.rational_nd r)
  ,r
  ,harmonicity_r barlow r)

{- | Table 2 (p.45)

>>> length (table_2 0.06)
24

>>> length (table_2 0.04)
66
-}
table_2 :: Double -> [Table_2_Row]
table_2 z =
    let g n = n <= 2 && n >= 1
        r = nub (sort (filter g [p % q | p <- [1..81],q <- [1..81]]))
        f (_,_,_,h) = h > z
    in filter f (map mk_table_2_row r)

{- | Pretty printer for 'Table_2_Row' values.

> mapM_ (putStrLn . table_2_pp) (table_2 0.06)

@
   0.000 |  0  0  0  0  0  0 |  1:1  | Infinity
 111.731 |  4 -1 -1  0  0  0 | 15:16 | 0.076531
 182.404 |  1 -2  1  0  0  0 |  9:10 | 0.078534
 203.910 | -3  2  0  0  0  0 |  8:9  | 0.120000
 231.174 |  3  0  0 -1  0  0 |  7:8  | 0.075269
 266.871 | -1 -1  0  1  0  0 |  6:7  | 0.071672
 294.135 |  5 -3  0  0  0  0 | 27:32 | 0.076923
 315.641 |  1  1 -1  0  0  0 |  5:6  | 0.099338
 386.314 | -2  0  1  0  0  0 |  4:5  | 0.119048
 407.820 | -6  4  0  0  0  0 | 64:81 | 0.060000
 435.084 |  0  2  0 -1  0  0 |  7:9  | 0.064024
 498.045 |  2 -1  0  0  0  0 |  3:4  | 0.214286
 519.551 | -2  3 -1  0  0  0 | 20:27 | 0.060976
 701.955 | -1  1  0  0  0  0 |  2:3  | 0.272727
 764.916 |  1 -2  0  1  0  0 |  9:14 | 0.060172
 813.686 |  3  0 -1  0  0  0 |  5:8  | 0.106383
 884.359 |  0 -1  1  0  0  0 |  3:5  | 0.110294
 905.865 | -4  3  0  0  0  0 | 16:27 | 0.083333
 933.129 |  2  1  0 -1  0  0 |  7:12 | 0.066879
 968.826 | -2  0  0  1  0  0 |  4:7  | 0.081395
 996.090 |  4 -2  0  0  0  0 |  9:16 | 0.107143
1017.596 |  0  2 -1  0  0  0 |  5:9  | 0.085227
1088.269 | -3  1  1  0  0  0 |  8:15 | 0.082873
1200.000 |  1  0  0  0  0  0 |  1:2  | 1.000000
@
-}
table_2_pp :: Table_2_Row -> String
table_2_pp (i,j,k,l) =
    let i' = printf "%8.3f" i
        j' = unwords (map (printf "%2d") j)
        k' = let (p,q) = T.rational_nd k in printf "%2d:%-2d" q p
        l' = printf "%1.6f" l
    in intercalate " | " [i',j',k',l']
