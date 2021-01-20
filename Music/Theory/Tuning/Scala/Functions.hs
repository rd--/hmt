-- | Scala functions, <http://www.huygens-fokker.org/scala/help.htm>
module Music.Theory.Tuning.Scala.Functions where

import Data.List {- base -}
--import Data.Maybe {- base -}

import qualified Music.Theory.Tuning as T {- hmt -}
--import qualified Music.Theory.Tuning.Scala as T {- hmt -}
--import qualified Music.Theory.Tuning.Scala.Interval as T {- hmt -}

{- | <http://www.huygens-fokker.org/scala/help.htm#EQUALTEMP>

> map round (equaltemp 12 2 13) == [0,100,200,300,400,500,600,700,800,900,1000,1100,1200]
> map round (equaltemp 13 3 14) == [0,146,293,439,585,732,878,1024,1170,1317,1463,1609,1756,1902]
> map round (equaltemp 12.5 3 14) == [0,152,304,456,609,761,913,1065,1217,1369,1522,1674,1826,1978]
-}
equaltemp :: Double -> Double -> Int -> [Double]
equaltemp division octave scale_size =
  let step = T.fratio_to_cents octave / division
  in take scale_size [0,step ..]

{- | <http://www.huygens-fokker.org/scala/help.htm#LINEARTEMP>

> let py = lineartemp 12 2 () (3/2 :: Rational) 3
> py == [1/1,2187/2048,9/8,32/27,81/64,4/3,729/512,3/2,6561/4096,27/16,16/9,243/128,2/1]
-}
lineartemp :: (Fractional n, Ord n) => Int -> n -> () -> n -> Int -> [n]
lineartemp scale_size octave _degree_of_fifth fifth down =
  let geom i m = i : geom (i * m) m
      geom_oct i = map T.fold_ratio_to_octave_err . geom i
      lhs = take (down + 1) (geom_oct 1 (1 / fifth))
      rhs = tail (take (scale_size - down) (geom_oct 1 fifth))
  in sort (lhs ++ rhs) ++ [octave]
