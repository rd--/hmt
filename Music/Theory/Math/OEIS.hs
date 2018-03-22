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

-- | <http://oeis.org/A080992>
--
-- Entries in Durer's magic square.
a080992 :: Num n => [n]
a080992 =
  [16,03,02,13
  ,05,10,11,08
  ,09,06,07,12
  ,04,15,14,01]

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
