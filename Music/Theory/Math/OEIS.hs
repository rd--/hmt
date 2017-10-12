-- | The On-Line Encyclopedia of Integer Sequences, <http://oeis.org/>
module Music.Theory.Math.OEIS where

-- | <http://oeis.org/A000290>
--
-- The squares of the non-negative integers.
--
-- > import Data.List
-- > [0,1,4,9,16,25,36,49,64,81,100] `isInfixOf` a000290
a000290 :: Integral n => [n]
a000290 = let square n = n * n in map square [0..]

-- | <http://oeis.org/A002267>
a002267 :: Num n => [n]
a002267 = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 41, 47, 59, 71]

-- | <http://oeis.org/A126709>
--
-- Loh-Shu magic square, attributed to the legendary Fu Xi (Fuh-Hi).
a126709 :: Num n => [n]
a126709 = [4, 9, 2, 3, 5, 7, 8, 1, 6]

-- | <http://oeis.org/A126710>
--
-- Jaina inscription of the twelfth or thirteenth century, Khajuraho, India.
a126710 :: Num n => [n]
a126710 = [7, 12, 1, 14, 2, 13, 8, 11, 16, 3, 10, 5, 9, 6, 15, 4]
