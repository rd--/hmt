-- | The On-Line Encyclopedia of Integer Sequences, <http://oeis.org/>
module Music.Theory.Math.OEIS where

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

{- | <http://oeis.org/A000290>

The squares of the non-negative integers.

> [0,1,4,9,16,25,36,49,64,81,100] `isPrefixOf` a000290
-}
a000290 :: Integral n => [n]
a000290 = let square n = n * n in map square [0..]

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

{- | <https://oeis.org/A003849>

The infinite Fibonacci word (start with 0, apply 0->01, 1->0, take limit).

> [0,1,0,0,1,0,1,0,0,1,0,0,1,0,1,0,0,1,0,1,0,0,1,0,0,1,0,1,0,0,1,0,0,1,0,1,0] `isPrefixOf` a003849
-}
a003849 :: Num n => [n]
a003849 =
  let fws = [1] : [0] : zipWith (++) fws (tail fws)
  in tail (concat fws)

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

{- | <http://oeis.org/A073334>

The "rhythmic infinity system" of Danish composer Per Nørgård

> take 24 a073334 == [3,5,8,5,8,13,8,5,8,13,21,13,8,13,8,5,8,13,21,13,21,34,21,13]
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
