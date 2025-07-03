-- | Tuning, Harry Partch
module Music.Theory.Tuning.Partch where

import Data.Ratio {- base -}

import qualified Data.Map.Strict as Map {- containers -}
import qualified Music.Theory.Array.Text as Array.Text {- hmt-base -}
import qualified Music.Theory.Show as Show {- hmt-base -}

import qualified Music.Theory.Tuning as Tuning {- hmt -}

orelate :: Integral i => Ratio i -> i -> Ratio i
orelate r m = Tuning.fold_ratio_to_octave_err (r * (m % 1))

urelate :: Integral i => Ratio i -> i -> Ratio i
urelate r m = Tuning.fold_ratio_to_octave_err (r * (1 % m))

{- | Incipient tonality diamond map

>>> Map.elems (itd_map [4 .. 6])
[1 % 1,8 % 5,4 % 3,5 % 4,1 % 1,5 % 3,3 % 2,6 % 5,1 % 1]
-}
itd_map :: [Integer] -> Map.Map (Int, Int) Rational
itd_map relation =
  let limit = length relation
      z = map (orelate 1) relation
      c0 = zip (map (\n -> (n, 0)) [0 .. limit - 1]) z
      cN =
        [ ((i, k), urelate (z !! i) (relation !! k))
        | i <- [0 .. limit - 1]
        , k <- [1 .. limit - 1]
        ]
  in Map.fromList (c0 ++ cN)

map_to_table :: t -> (Int, Int) -> Map.Map (Int, Int) t -> [[t]]
map_to_table k (nr, nc) m =
  [[Map.findWithDefault k (i, j) m | j <- [0 .. nc - 1]] | i <- [0 .. nr - 1]]

{- | 'map_to_table' of 'itd_map'.

>>> itd_tbl [4 .. 13] !! 0
[1 % 1,8 % 5,4 % 3,8 % 7,1 % 1,16 % 9,8 % 5,16 % 11,4 % 3,16 % 13]
-}
itd_tbl :: [Integer] -> [[Rational]]
itd_tbl r =
  let err = error "itd_tbl"
      n = length r
  in map_to_table err (n, n) (itd_map r)

{- | Incipient tonality diamond table pretty-print

>>> putStr $ itd_tbl_pp [4 .. 6]
1/1 8/5 4/3
5/4 1/1 5/3
3/2 6/5 1/1

>>> putStr $ itd_tbl_pp [4 .. 13]
 1/1   8/5   4/3  8/7  1/1 16/9   8/5 16/11   4/3 16/13
 5/4   1/1   5/3 10/7  5/4 10/9   1/1 20/11   5/3 20/13
 3/2   6/5   1/1 12/7  3/2  4/3   6/5 12/11   1/1 24/13
 7/4   7/5   7/6  1/1  7/4 14/9   7/5 14/11   7/6 14/13
 1/1   8/5   4/3  8/7  1/1 16/9   8/5 16/11   4/3 16/13
 9/8   9/5   3/2  9/7  9/8  1/1   9/5 18/11   3/2 18/13
 5/4   1/1   5/3 10/7  5/4 10/9   1/1 20/11   5/3 20/13
11/8 11/10  11/6 11/7 11/8 11/9 11/10   1/1  11/6 22/13
 3/2   6/5   1/1 12/7  3/2  4/3   6/5 12/11   1/1 24/13
13/8 13/10 13/12 13/7 13/8 13/9 13/10 13/11 13/12   1/1
-}
itd_tbl_pp :: [Integer] -> String
itd_tbl_pp =
  unlines
  . Array.Text.table_pp Array.Text.table_opt_plain
  . map (map Show.rational_pp)
  . itd_tbl
