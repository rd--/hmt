{- | Robert Morris. \"A Similarity Index for Pitch-Class
Sets\". Perspectives of New Music, 18(2):445-460, 1980.
-}
module Music.Theory.Metric.Morris_1980 where

import Data.Ratio {- base -}

import qualified Music.Theory.Math.Z as Z {- hmt-base -}

import qualified Music.Theory.Z.Forte_1973 as Forte {- hmt -}

{- | Sim

>>> Forte.z_icv Z.z12 [0,1,3,6]
[1,1,2,0,1,1]

>>> Forte.z_icv Z.z12 [0,2,4,7]
[0,2,1,1,2,0]

>>> sim [0,1,3,6] [0,2,4,7]
6

>>> sim [0,1,2,4,5,8] [0,1,3,7]
9
-}
sim :: (Integral i, Num n) => [i] -> [i] -> n
sim r s =
  let r' = Forte.z_icv Z.z12 r
      s' = Forte.z_icv Z.z12 s
      t = zipWith (-) r' s'
  in sum (map abs t)

{- | Asim

>>> asim [0,1,3,6] [0,2,4,7] == 6/12
True

>>> asim [0,1,2,4,5,8] [0,1,3,7] == 9/21
True

>>> asim [0,1,2,3,4] [0,1,4,5,7] == 2/5
True

>>> asim [0,1,2,3,4] [0,2,4,6,8] == 3/5
True

>>> asim [0,1,4,5,7] [0,2,4,6,8] == 3/5
True
-}
asim :: Integral i => [i] -> [i] -> Ratio i
asim r s =
  let r' = Forte.z_icv Z.z12 r
      s' = Forte.z_icv Z.z12 s
  in sim r s % (sum r' + sum s')
