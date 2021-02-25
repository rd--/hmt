-- | Scala functions, <http://www.huygens-fokker.org/scala/help.htm>
module Music.Theory.Tuning.Scala.Functions where

import Data.List {- base -}

import qualified Music.Theory.Array.Text as Text {- hmt -}
import qualified Music.Theory.List as List {- hmt -}
import qualified Music.Theory.Math as Math {- hmt -}
import qualified Music.Theory.Show as Show {- hmt -}
import qualified Music.Theory.Tuning as Tuning {- hmt -}
import qualified Music.Theory.Tuning.Scala as Scala {- hmt -}
import qualified Music.Theory.Tuning.Scala.Interval as Interval {- hmt -}

{- | <http://www.huygens-fokker.org/scala/help.htm#EQUALTEMP>

> map round (equaltemp 12 2 13) == [0,100,200,300,400,500,600,700,800,900,1000,1100,1200]
> map round (equaltemp 13 3 14) == [0,146,293,439,585,732,878,1024,1170,1317,1463,1609,1756,1902]
> map round (equaltemp 12.5 3 14) == [0,152,304,456,609,761,913,1065,1217,1369,1522,1674,1826,1978]
-}
equaltemp :: Double -> Double -> Int -> [Double]
equaltemp division octave scale_size =
  let step = Tuning.fratio_to_cents octave / division
  in take scale_size [0,step ..]

{- | <http://www.huygens-fokker.org/scala/help.htm#LINEARTEMP>

> let py = lineartemp 12 2 () (3/2 :: Rational) 3
> py == [1/1,2187/2048,9/8,32/27,81/64,4/3,729/512,3/2,6561/4096,27/16,16/9,243/128,2/1]
-}
lineartemp :: (Fractional n, Ord n) => Int -> n -> () -> n -> Int -> [n]
lineartemp scale_size octave _degree_of_fifth fifth down =
  let geom i m = i : geom (i * m) m
      geom_oct i = map Tuning.fold_ratio_to_octave_err . geom i
      lhs = take (down + 1) (geom_oct 1 (1 / fifth))
      rhs = tail (take (scale_size - down) (geom_oct 1 fifth))
  in sort (lhs ++ rhs) ++ [octave]

interval_hist_ratios :: (Fractional t,Ord t) => [t] -> [(t,Int)]
interval_hist_ratios x = List.histogram [(if p < q then p * 2 else p) / q | p <- x, q <- x, p /= q]

intervals_list_ratios_r :: Interval.INTNAM -> [Rational] -> IO ()
intervals_list_ratios_r nam_db rat = do
  let hst = interval_hist_ratios rat
      ln (r,n) = let nm = maybe "" snd (Interval.intnam_search_ratio nam_db r)
                     c = Tuning.ratio_to_cents r
                     i = Math.real_round_int (c / 100)
                 in [show i,show n,Show.ratio_pp r,Show.real_pp 1 c,nm]
      tbl = map ln hst
      pp = Text.table_pp Text.table_opt_plain
  putStrLn (unlines (pp tbl))

{- | <http://www.huygens-fokker.org/scala/help.htm#SHOW_INTERVALS>

> mapM_ intervals_list_ratios (words "pyth_12 kepler1")
-}
intervals_list_ratios :: String -> IO ()
intervals_list_ratios scl_nm = do
  nam_db <- Interval.load_intnam
  scl <- Scala.scl_load scl_nm
  intervals_list_ratios_r nam_db (tail (Scala.scale_ratios_req scl))
