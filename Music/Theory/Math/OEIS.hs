-- | The On-Line Encyclopedia of Integer Sequences, <http://oeis.org/>
module Music.Theory.Math.OEIS where

import Data.Bits {- base -}
import Data.List {- base -}
import Data.Ratio {- base -}

import qualified Music.Theory.Math as Math {- hmt -}

{- | <http://oeis.org/A000010>

Euler totient function phi(n): count numbers <= n and prime to n.

> [1,1,2,2,4,2,6,4,6,4,10,4,12,6,8,8,16,6,18,8,12,10,22,8,20,12] `isPrefixOf` a000010
-}
a000010 :: Integral n => [n]
a000010 =
  let phi n = genericLength (filter (==1) (map (gcd n) [1..n]))
  in map phi [1::Integer ..]

{- | <http://oeis.org/A000045>

Fibonacci numbers

> [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610] `isPrefixOf` a000045
-}
a000045 :: Num n => [n]
a000045 = 0 : 1 : zipWith (+) a000045 (tail a000045)

{- | <http://oeis.org/A000051>

a(n) = 2^n + 1

> [2,3,5,9,17,33,65,129,257,513,1025,2049,4097,8193,16385,32769,65537,131073] `isPrefixOf` a000051
-}
a000051 :: Num n => [n]
a000051 = iterate ((subtract 1) . (* 2)) 2

{- | <http://oeis.org/A000120>

1's-counting sequence: number of 1's in binary expansion of n (or the binary weight of n).

> [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,1,2,2,3,2,3,3] `isPrefixOf` a000120
-}
a000120 :: [Int]
a000120 = let r = [0] : (map . map) (+ 1) (scanl1 (++) r) in concat r

{- | <http://oeis.org/A000079>

Powers of 2: a(n) = 2^n

> [1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536] `isPrefixOf` a000079
-}
a000079 :: Num n => [n]
a000079 = iterate (* 2) 1

{- | <http://oeis.org/A000142>

Factorial numbers: n! = 1*2*3*4*...*n
(order of symmetric group S_n, number of permutations of n letters).

> [1,1,2,6,24,120,720,5040,40320,362880,3628800,39916800,479001600,6227020800] `isPrefixOf` a000142
-}
a000142 :: (Enum n, Num n) => [n]
a000142 = 1 : zipWith (*) [1..] a000142

{- | https://oeis.org/A000201

Lower Wythoff sequence (a Beatty sequence): a(n) = floor(n*phi), where phi = (1+sqrt(5))/2 = A001622

> [1,3,4,6,8,9,11,12,14,16,17,19,21,22,24,25,27,29,30,32,33,35,37,38,40,42] `isPrefixOf` a000201

> import Sound.SC3.Plot {- hsc3-plot -}
> plot_p1_imp [take 128 a000201 :: [Int]]
-}
a000201 :: Integral n => [n]
a000201 =
  let f (x:xs) (y:ys) = y : f xs (delete (x + y) ys)
      f _ _ = error "a000201"
  in f [1..] [1..]

{- | <https://oeis.org/A000204>

Lucas numbers (beginning with 1): L(n) = L(n-1) + L(n-2) with L(1) = 1, L(2) = 3

> [1,3,4,7,11,18,29,47,76,123,199,322,521,843,1364,2207,3571,5778,9349,15127] `isPrefixOf` a000204
-}
a000204 :: Num n => [n]
a000204 = 1 : 3 : zipWith (+) a000204 (tail a000204)

{- | <https://oeis.org/A000217>

Triangular numbers: a(n) = binomial(n+1,2) = n(n+1)/2 = 0 + 1 + 2 + ... + n.

> [0,1,3,6,10,15,21,28,36,45,55,66,78,91,105,120,136,153,171,190,210,231,253,276] `isPrefixOf` a000217
-}
a000217 :: (Enum n,Num n) => [n]
a000217 = scanl1 (+) [0..]

{- | <http://oeis.org/A000225>

a(n) = 2^n - 1 (Sometimes called Mersenne numbers, although that name is usually reserved for A001348)

> [0,1,3,7,15,31,63,127,255,511,1023,2047,4095,8191,16383,32767,65535] `isPrefixOf` a000225
-}
a000225 :: Num n => [n]
a000225 = iterate ((+ 1) . (* 2)) 0

{- | <http://oeis.org/A000290>

The squares of the non-negative integers.

> [0,1,4,9,16,25,36,49,64,81,100] `isPrefixOf` a000290
-}
a000290 :: Integral n => [n]
a000290 = let square n = n * n in map square [0..]

{- | <https://oeis.org/A000292>

Tetrahedral (or triangular pyramidal) numbers: a(n) = C(n+2,3) = n*(n+1)*(n+2)/6.

> [0,1,4,10,20,35,56,84,120,165,220,286,364,455,560,680,816,969,1140,1330,1540] `isPrefixOf` a000292
-}
a000292 :: (Enum n,Num n) => [n]
a000292 = scanl1 (+) a000217

{- | <https://oeis.org/A000930>

Narayana's cows sequence.

> [1,1,1,2,3,4,6,9,13,19,28,41,60] `isPrefixOf` a000930
-}
a000930 :: Num n => [n]
a000930 = 1 : 1 : 1 : zipWith (+) a000930 (drop 2 a000930)

{- | <https://oeis.org/A000931>

Padovan sequence (or Padovan numbers)

> [1,0,0,1,0,1,1,1,2,2,3,4,5,7,9,12,16,21,28,37,49,65,86,114,151,200,265] `isPrefixOf` a000931
-}
a000931 :: Num n => [n]
a000931 = 1 : 0 : 0 : zipWith (+) a000931 (tail a000931)

{- | <https://oeis.org/A001008>

Numerators of harmonic numbers H(n) = Sum_{i=1..n} 1/i

[1,3,11,25,137,49,363,761,7129,7381,83711,86021,1145993,1171733,1195757,2436559] `isPrefixOf` a001008
-}
a001008 :: Integral i => [i]
a001008 = map numerator (scanl1 (+) (map (1 %) [1..]))

{- | <https://oeis.org/A001333>

Numerators of continued fraction convergents to sqrt(2).

[1,1,3,7,17,41,99,239,577,1393,3363,8119,19601,47321,114243,275807,665857] `isPrefixOf` a001333
-}
a001333 :: Num n => [n]
a001333 = 1 : 1 : zipWith (+) a001333 (map (* 2) (tail a001333))

{- | <http://oeis.org/A001687>

a(n) = a(n-2) + a(n-5).

[0,1,0,1,0,1,1,1,2,1,3,2,4,4,5,7,7,11,11,16,18,23,29,34,45,52,68,81,102,126,154] `isPrefixOf` a001687
-}
a001687 :: Num n => [n]
a001687 = 0 : 1 : 0 : 1 : 0 : zipWith (+) a001687 (drop 3 a001687)

{- | <https://oeis.org/A001950>

Upper Wythoff sequence (a Beatty sequence): a(n) = floor(n*phi^2), where phi = (1+sqrt(5))/2

> [2,5,7,10,13,15,18,20,23,26,28,31,34,36,39,41,44,47,49,52,54,57,60,62,65] `isPrefixOf` a001950
-}
a001950 :: Integral n => [n]
a001950 = zipWith (+) a000201 [1..]

-- | <http://oeis.org/A002267>
--
-- The 15 supersingular primes.
a002267 :: Num n => [n]
a002267 = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 41, 47, 59, 71]

{- | <https://oeis.org/A002487>

Stern's diatomic series (or Stern-Brocot sequence)

> [0,1,1,2,1,3,2,3,1,4,3,5,2,5,3,4,1,5,4,7,3,8,5,7,2,7,5,8,3,7,4,5] `isPrefixOf` a002487
-}
a002487 :: Num n => [n]
a002487 =
  let f (a:a') (b:b') = a + b : a : f a' b'
      f _ _ = error "a002487?"
      x = 1 : 1 : f (tail x) x
  in 0 : x

-- | <http://oeis.org/A003269>
--
-- [0,1,1,1,1,2,3,4,5,7,10,14,19,26,36,50,69,95,131,181,250,345,476,657] `isPrefixOf` a003269
a003269 :: Num n => [n]
a003269 = 0 : 1 : 1 : 1 : zipWith (+) a003269 (drop 3 a003269)

{- | <http://oeis.org/A003520>

a(n) = a(n-1) + a(n-5); a(0) = ... = a(4) = 1.

> [1,1,1,1,1,2,3,4,5,6,8,11,15,20,26,34,45,60,80,106,140,185,245,325,431] `isPrefixOf` a003520
-}
a003520 :: Num n => [n]
a003520 = 1 : 1 : 1 : 1 : 1 : zipWith (+) a003520 (drop 4 a003520)

{- | <https://oeis.org/A003849>

The infinite Fibonacci word (start with 0, apply 0->01, 1->0, take limit).

> [0,1,0,0,1,0,1,0,0,1,0,0,1,0,1,0,0,1,0,1,0,0,1,0,0,1,0,1,0,0,1,0,0,1,0,1,0] `isPrefixOf` a003849
-}
a003849 :: Num n => [n]
a003849 =
  let fws = [1] : [0] : zipWith (++) fws (tail fws)
  in tail (concat fws)

{- | <http://oeis.org/A004001>

Hofstadter-Conway sequence: a(n) = a(a(n-1)) + a(n-a(n-1)) with a(1) = a(2) = 1.

> [1,1,2,2,3,4,4,4,5,6,7,7,8,8,8,8,9,10,11,12,12,13,14,14,15,15,15,16,16,16,16,16] `isPrefixOf` a004001

> plot_p1_ln [take 250 a004001]
> plot_p1_ln [zipWith (-) a004001 (map (`div` 2) [1 .. 2000])]

-}
a004001 :: [Int]
a004001 =
  let h n x =
        let x' = a004001 !! (x - 1) + a004001 !! (n - x - 1)
        in x' : h (n + 1) x'
  in 1 : 1 : h 3 1

{- | <http://oeis.org/A004718>

Per Nørgård's "infinity sequence"

> take 32 a004718 == [0,1,-1,2,1,0,-2,3,-1,2,0,1,2,-1,-3,4,1,0,-2,3,0,1,-1,2,-2,3,1,0,3,-2,-4,5]

> plot_p1_imp [take 1024 a004718]

<https://www.tandfonline.com/doi/abs/10.1080/17459737.2017.1299807>
<https://arxiv.org/pdf/1402.3091.pdf>

-}
a004718 :: Num n => [n]
a004718 = 0 : concat (transpose [map (+ 1) a004718, map negate (tail a004718)])

{- | <http://oeis.org/A005728>

Number of fractions in Farey series of order n.

> [1,2,3,5,7,11,13,19,23,29,33,43,47,59,65,73,81,97,103,121,129,141,151] `isPrefixOf` a005728
-}
a005728 :: Integral i => [i]
a005728 =
  let phi n = genericLength (filter (==1) (map (gcd n) [1..n]))
      f n = if n == 0 then 1 else f (n - 1) + phi n
  in map f [0::Integer ..]

{- | <http://oeis.org/A005811>

Number of runs in binary expansion of n (n>0); number of 1's in Gray code for n

> take 32 a005811 == [0,1,2,1,2,3,2,1,2,3,4,3,2,3,2,1,2,3,4,3,4,5,4,3,2,3,4,3,2,3,2,1]
-}
a005811 :: Integral n => [n]
a005811 =
  let f (x:xs) = x : f (xs ++ [x + x `mod` 2, x + 1 - x `mod` 2])
      f _ = error "A005811?"
  in 0 : f [1]

{- | <http://oeis.org/A047999>

Total number of odd entries in first n rows of Pascal's triangle: a(0) = 0, a(1) = 1, a(2k) = 3*a(k), a(2k+1) = 2*a(k) + a(k+1).

> [0,1,3,5,9,11,15,19,27,29,33,37,45,49,57,65,81,83,87,91,99,103,111,119,135,139] `isPrefixOf` a006046
-}
a006046 :: [Int]
a006046 = map (sum . concat) (inits a047999_tbl)

{- | <http://oeis.org/A006842>

Triangle read by rows: row n gives numerators of Farey series of order n.

> [0,1,0,1,1,0,1,1,2,1,0,1,1,1,2,3,1,0,1,1,1,2,1,3,2,3,4,1,0,1,1,1,1,2,1,3] `isPrefixOf` a006842
> plot_p1_imp [take 200 (a006842 :: [Int])]
> plot_p1_pt [take 10000 (a006842 :: [Int])]
-}
a006842 :: Integral i => [i]
a006842 = map numerator (concatMap Math.farey [1..])

{- | <http://oeis.org/A006843>

Triangle read by rows: row n gives denominators of Farey series of order n

> [1,1,1,2,1,1,3,2,3,1,1,4,3,2,3,4,1,1,5,4,3,5,2,5,3,4,5,1,1,6,5,4,3,5,2,5] `isPrefixOf` a006843
> plot_p1_imp [take 200 (a006843 :: [Int])]
> plot_p1_pt [take 10000 (a006843 :: [Int])]
-}
a006843 :: Integral i => [i]
a006843 = map denominator (concatMap Math.farey [1..])

{- | <https://oeis.org/A007318>

Pascal's triangle read by rows

[[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1]] `isPrefixOf` a007318
-}
a007318 :: Integral i => [[i]]
a007318 =
  let f r = zipWith (+) ([0] ++ r) (r ++ [0])
  in iterate f [1]

{- | <https://oeis.org/A008277>

Triangle of Stirling numbers of the second kind, S2(n,k), n >= 1, 1 <= k <= n.

[1,1,1,1,3,1,1,7,6,1,1,15,25,10,1,1,31,90,65,15,1,1,63,301,350,140,21,1] `isPrefixOf` a008277
-}
a008277 :: (Enum n,Num n) => [n]
a008277 = concat a008277_tbl

a008277_tbl :: (Enum n,Num n) => [[n]]
a008277_tbl = map tail $ a048993_tbl

{- | <http://oeis.org/A008278>

Triangle of Stirling numbers of 2nd kind, S(n,n-k+1), n >= 1, 1<=k<=n.

[1,1,1,1,3,1,1,6,7,1,1,10,25,15,1,1,15,65,90,31,1,1,21,140,350,301,63,1] `isPrefixOf` a008278
-}
a008278 :: (Enum n,Num n) => [n]
a008278 = concat a008278_tbl

a008278_tbl :: (Enum n,Num n) => [[n]]
a008278_tbl =
  let f p =
        let q = reverse (zipWith (*) [1..] (reverse p))
        in zipWith (+) ([0] ++ q) (p ++ [0])
  in iterate f [1]

{- | <http://oeis.org/A017817>

a(n) = a(n-3) + a(n-4), with a(0)=1, a(1)=a(2)=0, a(3)=1

> [1,0,0,1,1,0,1,2,1,1,3,3,2,4,6,5,6,10,11,11,16,21,22,27,37,43,49,64,80,92] `isPrefixOf` a017817
-}
a017817 :: Num n => [n]
a017817 = 1 : 0 : 0 : 1 : zipWith (+) a017817 (tail a017817)


{- | <http://oeis.org/A030308>

Triangle T(n,k): Write n in base 2, reverse order of digits, to get the n-th row

> take 9 a030308 == [[0],[1],[0,1],[1,1],[0,0,1],[1,0,1],[0,1,1],[1,1,1],[0,0,0,1]]
-}
a030308 :: (Eq n,Num n) => [[n]]
a030308 =
   let f l = case l of
         [] -> [1]
         0:b -> 1 : b
         1:b -> 0 : f b
         _ -> error "A030308?"
   in iterate f [0]

{- | <http://oeis.org/A047999>

Sierpiński's triangle (or gasket): triangle, read by rows, formed by reading Pascal's triangle mod 2.

> [1,1,1,1,0,1,1,1,1,1,1,0,0,0,1,1,1,0,0,1,1,1,0,1,0,1,0,1,1,1,1,1,1,1,1,1,1,0,0] `isPrefixOf` a047999
-}
a047999 :: [Int]
a047999 = concat a047999_tbl

a047999_tbl :: [[Int]]
a047999_tbl = iterate (\r -> zipWith xor ([0] ++ r) (r ++ [0])) [1]

{- | <https://oeis.org/A048993>

Triangle of Stirling numbers of 2nd kind, S(n,k), n >= 0, 0 <= k <= n.

> [1,0,1,0,1,1,0,1,3,1,0,1,7,6,1,0,1,15,25,10,1,0,1,31,90,65,15,1] `isPrefixOf` a048993
-}
a048993 :: (Enum n,Num n) => [n]
a048993 = concat a048993_tbl

a048993_tbl :: (Enum n,Num n) => [[n]]
a048993_tbl = iterate (\row -> [0] ++ (zipWith (+) row $ zipWith (*) [1..] $ tail row) ++ [1]) [1]

{- | <http://oeis.org/A049455>

Triangle read by rows, numerator of fractions of a variant of the Farey series.

> [0,1,0,1,1,0,1,1,2,1,0,1,1,2,1,3,2,3,1,0,1,1,2,1,3,2,3,1,4,3,5,2,5,3,4,1,0] `isPrefixOf` a049455
> plot_p1_imp [take 200 (a049455 :: [Int])]
> plot_p1_pt [take 10000 (a049455 :: [Int])]
-}
a049455 :: Integral n => [n]
a049455 = map fst (concat Math.stern_brocot_tree_lhs)

{- | <http://oeis.org/A049456>

Triangle read by rows, denominator of fractions of a variant of the Farey series.

[1,1,1,2,1,1,3,2,3,1,1,4,3,5,2,5,3,4,1,1,5,4,7,3,8,5,7,2,7,5,8,3,7,4,5,1,1,6,5,9] `isPrefixOf` a049456
> plot_p1_imp [take 200 (a049456 :: [Int])]
> plot_p1_pt [take 10000 (a049456 :: [Int])]
-}
a049456 :: Integral n => [n]
a049456 = map snd (concat Math.stern_brocot_tree_lhs)

{- | <http://oeis.org/A073334>

The "rhythmic infinity system" of Danish composer Per Nørgård

> take 24 a073334 == [3,5,8,5,8,13,8,5,8,13,21,13,8,13,8,5,8,13,21,13,21,34,21,13]
> plot_p1_imp [take 200 (a073334 :: [Int])]
-}
a073334 :: Num n => [n]
a073334 =
  let f n = a000045 !! ((a005811 !! n) + 4)
  in 3 : map f [1..]

-- | <http://oeis.org/A080992>
--
-- Entries in Durer's magic square.
a080992 :: Num n => [n]
a080992 =
  [16,03,02,13
  ,05,10,11,08
  ,09,06,07,12
  ,04,15,14,01]

{- | <http://oeis.org/A083866>

Positions of zeros in Per Nørgård's infinity sequence (A004718).

> take 24 a083866 == [0,5,10,17,20,27,34,40,45,54,65,68,75,80,85,90,99,105,108,119,130,136,141,150]
-}
a083866 :: (Enum n,Num n) => [n]
a083866 = map snd (filter ((== (0::Int)) . fst) (zip a004718 [0..]))

-- | <http://oeis.org/A126709>
--
-- Loh-Shu magic square, attributed to the legendary Fu Xi (Fuh-Hi).
a126709 :: Num n => [n]
a126709 =
  [4,9,2
  ,3,5,7
  ,8,1,6]

-- | <http://oeis.org/A126710>
--
-- Jaina inscription of the twelfth or thirteenth century, Khajuraho, India.
a126710 :: Num n => [n]
a126710 =
  [07,12,01,14
  ,02,13,08,11
  ,16,03,10,05
  ,09,06,15,04]

-- | <http://oeis.org/A126976>
--
-- Agrippa (Magic Square of the Sun)
a126976 :: Num n => [n]
a126976 =
  [06,32,03,34,35,01
  ,07,11,27,28,08,30
  ,19,14,16,15,23,24
  ,18,20,22,21,17,13
  ,25,29,10,09,26,12
  ,36,05,33,04,02,31]

{- | <http://oeis.org/A255723>

Another variant of Per Nørgård's "infinity sequence"

> take 24 a255723 == [0,-2,-1,2,-2,-4,1,0,-1,-3,0,1,2,0,-3,4,-2,-4,1,0,-4,-6,3,-2]
> plot_p1_imp [take 400 (a255723 :: [Int])]
-}
a255723 :: Num n => [n]
a255723 = 0 : concat (transpose [map (subtract 2) a255723
                                ,map (-1 -) a255723
                                ,map (+ 2) a255723
                                ,tail a255723])

{- | <http://oeis.org/A256184>

First of two variations by Per Nørgård of his "infinity sequence"

> take 24 a256184 == [0,-2,-1,2,-4,-3,1,-3,-2,-2,0,1,4,-6,-5,3,-5,-4,-1,-1,0,3,-5,-4]
-}
a256184 :: Num n => [n]
a256184 = 0 : concat (transpose [map (subtract 2) a256184
                                ,map (subtract 1) a256184
                                ,map negate (tail a256184)])

{- | <http://oeis.org/A256185>

Second of two variations by Per Nørgård of his "infinity sequence"

> take 24 a256185 == [0,-3,-2,3,-6,1,2,-5,0,-3,0,-5,6,-9,4,-1,-2,-3,-2,-1,-4,5,-8,3]
-}
a256185 :: Num n => [n]
a256185 = 0 : concat (transpose [map (subtract 3) a256185
                                ,map (-2 -) a256185
                                ,map negate (tail a256185)])
