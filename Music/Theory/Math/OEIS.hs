-- | The On-Line Encyclopedia of Integer Sequences, <http://oeis.org/>
module Music.Theory.Math.OEIS where

import Data.List {- base -}

-- | <http://oeis.org/A000045>
--
-- Fibonacci numbers
--
-- > take 16 a000045 == [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610]
a000045 :: Num n => [n]
a000045 = 0 : 1 : zipWith (+) a000045 (tail a000045)

-- | <http://oeis.org/A000290>
--
-- The squares of the non-negative integers.
--
-- > import Data.List
-- > [0,1,4,9,16,25,36,49,64,81,100] `isInfixOf` a000290
a000290 :: Integral n => [n]
a000290 = let square n = n * n in map square [0..]

-- | <http://oeis.org/A002267>
--
-- The 15 supersingular primes.
a002267 :: Num n => [n]
a002267 = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 41, 47, 59, 71]

{- | <http://oeis.org/A004718>

Per Nørgård's "infinity sequence"

> take 32 a004718 == [0,1,-1,2,1,0,-2,3,-1,2,0,1,2,-1,-3,4,1,0,-2,3,0,1,-1,2,-2,3,1,0,3,-2,-4,5]

> import Sound.SC3.Plot {- hsc3-plot -}
> plotImpulses [take 1024 a004718]

<https://www.tandfonline.com/doi/abs/10.1080/17459737.2017.1299807>
<https://arxiv.org/pdf/1402.3091.pdf>

-}
a004718 :: Num n => [n]
a004718 = 0 : concat (transpose [map (+ 1) a004718, map negate (tail a004718)])

{- | <http://oeis.org/A005811>

> take 32 a005811 == [0,1,2,1,2,3,2,1,2,3,4,3,2,3,2,1,2,3,4,3,4,5,4,3,2,3,4,3,2,3,2,1]
-}
a005811 :: Integral n => [n]
a005811 =
  let f (x:xs) = x : f (xs ++ [x + x `mod` 2, x + 1 - x `mod` 2])
      f _ = error "A005811?"
  in 0 : f [1]

-- | <http://oeis.org/A030308>
--
-- > take 9 a030308 == [[0],[1],[0,1],[1,1],[0,0,1],[1,0,1],[0,1,1],[1,1,1],[0,0,0,1]]
a030308 :: (Eq n,Num n) => [[n]]
a030308 =
   let f l = case l of
         [] -> [1]
         0:b -> 1 : b
         1:b -> 0 : f b
         _ -> error "A030308?"
   in iterate f [0]

-- | <http://oeis.org/A073334>
--
-- > take 24 a073334 == [3,5,8,5,8,13,8,5,8,13,21,13,8,13,8,5,8,13,21,13,21,34,21,13]
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

take 24 a255723 == [0,-2,-1,2,-2,-4,1,0,-1,-3,0,1,2,0,-3,4,-2,-4,1,0,-4,-6,3,-2]
-}
a255723 :: Num n => [n]
a255723 = 0 : concat (transpose [map (subtract 2) a255723
                                ,map (-1 -) a255723
                                ,map (+ 2) a255723
                                ,tail a255723])

{- | <http://oeis.org/A256184>

take 24 a256184 == [0,-2,-1,2,-4,-3,1,-3,-2,-2,0,1,4,-6,-5,3,-5,-4,-1,-1,0,3,-5,-4]
-}
a256184 :: Num n => [n]
a256184 = 0 : concat (transpose [map (subtract 2) a256184
                                ,map (subtract 1) a256184
                                ,map negate (tail a256184)])

{- | <http://oeis.org/A256185>

take 24 a256185 == [0,-3,-2,3,-6,1,2,-5,0,-3,0,-5,6,-9,4,-1,-2,-3,-2,-1,-4,5,-8,3]
-}
a256185 :: Num n => [n]
a256185 = 0 : concat (transpose [map (subtract 3) a256185
                                ,map (-2 -) a256185
                                ,map negate (tail a256185)])
