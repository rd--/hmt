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

>>> map round (equaltemp 12 2 13)
[0,100,200,300,400,500,600,700,800,900,1000,1100,1200]

>>> map round (equaltemp 13 3 14)
[0,146,293,439,585,732,878,1024,1170,1317,1463,1609,1756,1902]

>>> map round (equaltemp 12.5 3 14)
[0,152,304,456,609,761,913,1065,1217,1369,1522,1674,1826,1978]
-}
equaltemp :: Double -> Double -> Int -> [Double]
equaltemp division octave scale_size =
  let step = Tuning.fratio_to_cents octave / division
  in take scale_size [0, step ..]

{- | <http://www.huygens-fokker.org/scala/help.htm#LINEARTEMP>

>>> lineartemp 12 2 () (3/2 :: Rational) 3
[1 % 1,2187 % 2048,9 % 8,32 % 27,81 % 64,4 % 3,729 % 512,3 % 2,6561 % 4096,27 % 16,16 % 9,243 % 128,2 % 1]
-}
lineartemp :: (Fractional n, Ord n) => Int -> n -> () -> n -> Int -> [n]
lineartemp scale_size octave _degree_of_fifth fifth down =
  let geom i m = i : geom (i * m) m
      geom_oct i = map Tuning.fold_ratio_to_octave_err . geom i
      lhs = take (down + 1) (geom_oct 1 (1 / fifth))
      rhs = List.tail_err (take (scale_size - down) (geom_oct 1 fifth))
  in sort (lhs ++ rhs) ++ [octave]

-- * Intervals

interval_hist_ratios :: (Fractional t, Ord t) => [t] -> [(t, Int)]
interval_hist_ratios x = List.histogram [(if p < q then p * 2 else p) / q | p <- x, q <- x, p /= q]

intervals_list_ratios_r :: Interval.IntNam -> [Rational] -> IO ()
intervals_list_ratios_r nam_db rat = do
  let hst = interval_hist_ratios rat
      ln (r, n) =
        let nm = maybe "" snd (Interval.intnam_search_ratio nam_db r)
            c = Tuning.ratio_to_cents r
            i = Math.real_round_int (c / 100)
        in [show i, show n, Show.ratio_pp r, Show.real_pp 1 c, nm]
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
  intervals_list_ratios_r nam_db (List.tail_err (Scala.scale_ratios_req True scl))

-- * Intervals

-- | Given interval function (ie. '-' or '/') and scale generate interval half-matrix.
interval_half_matrix :: (t -> t -> u) -> [t] -> [[u]]
interval_half_matrix interval_f =
  let tails' = filter ((>= 2) . length) . tails
      f l = case l of
        [] -> []
        i : l' -> map (`interval_f` i) l'
  in map f . tails'

interval_half_matrix_tbl :: (t -> String) -> (t -> t -> t) -> [t] -> [[String]]
interval_half_matrix_tbl show_f interval_f scl =
  let f n l = replicate n "" ++ map show_f l
  in zipWith f [1 ..] (interval_half_matrix interval_f scl)

intervals_half_matrix :: (Scala.Scale -> [t]) -> (t -> t -> t) -> (t -> String) -> String -> IO ()
intervals_half_matrix scl_f interval_f show_f nm = do
  scl <- Scala.scl_load nm
  let txt = interval_half_matrix_tbl show_f interval_f (scl_f scl)
      pp = Text.table_pp Text.table_opt_plain
  putStrLn (unlines (pp txt))

-- > mapM_ (intervals_half_matrix_cents 0) (words "pyth_12 kepler1")
intervals_half_matrix_cents :: Int -> String -> IO ()
intervals_half_matrix_cents k = intervals_half_matrix (Scala.scale_cents True) (-) (Show.real_pp k)

-- > mapM_ (intervals_half_matrix_ratios) (words "pyth_12 kepler1")
intervals_half_matrix_ratios :: String -> IO ()
intervals_half_matrix_ratios = intervals_half_matrix (Scala.scale_ratios_req True) (/) Show.ratio_pp

{-
> r = [3*5,3*7,3*11,5*7,5*11,7*11]
> r = let u = [1,3,5,7,9,11] in [i*j*k | i <- u, j <- u, k <- u, i < j, j < k]
> intervals_matrix_wr Show.ratio_pp (interval_matrix_ratio r)
-}
interval_matrix_ratio :: [Rational] -> [[Rational]]
interval_matrix_ratio x = let f i = map (\j -> if j < i then j * 2 / i else j / i) x in map f x

interval_matrix_cents :: [Tuning.Cents] -> [[Tuning.Cents]]
interval_matrix_cents x = let f i = map (\j -> if j < i then j + 1200 - i else j - i) x in map f x

intervals_matrix_wr :: (t -> String) -> [[t]] -> IO ()
intervals_matrix_wr pp_f x = do
  let txt = map (map pp_f) x
      pp = Text.table_pp Text.table_opt_plain
  putStrLn (unlines (pp txt))

intervals_matrix :: (Scala.Scale -> [t]) -> ([t] -> [[t]]) -> (t -> String) -> String -> IO ()
intervals_matrix scl_f tbl_f pp_f nm = do
  scl <- Scala.scl_load nm
  intervals_matrix_wr pp_f (tbl_f (scl_f scl))

-- > mapM_ (intervals_matrix_cents 0) (words "pyth_12 kepler1")
intervals_matrix_cents :: Int -> String -> IO ()
intervals_matrix_cents k = intervals_matrix (Scala.scale_cents True) interval_matrix_cents (Show.real_pp k)

-- > mapM_ intervals_matrix_ratios (words "pyth_12 kepler1")
intervals_matrix_ratios :: String -> IO ()
intervals_matrix_ratios = intervals_matrix (Scala.scale_ratios_req True) interval_matrix_ratio Show.ratio_pp
