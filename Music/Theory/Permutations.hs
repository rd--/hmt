module Music.Theory.Permutations (permutation
                                 ,apply_permutation
                                 ,non_invertible
                                 ,from_cycles
                                 ,two_line,one_line
                                 ,multiplication_table
                                 ,compose
                                 ,permutations_l
                                 ,multiset_permutations) where

import Data.List
import qualified Data.Permute as P
import qualified Math.Combinatorics.Multiset as C

-- | Variant of `elemIndices' that requires `e' to be unique in `p'.
elem_index_unique :: (Eq a) => a -> [a] -> Int
elem_index_unique e p =
    case elemIndices e p of
      [i] -> i
      _ -> error "elem_index_unique"

-- | Generate the permutation from `p' to `q', that is the permutation
--   that, applied to `p' will give `q'.
permutation :: (Eq a) => [a] -> [a] -> P.Permute
permutation p q =
    let n = length p
        f x = elem_index_unique x p
    in P.listPermute n (map f q)

-- | Apply permutation `f' to `p'.
apply_permutation :: (Eq a) => P.Permute -> [a] -> [a]
apply_permutation f p = map (p !!) (P.elems f)

-- | True if the inverse of `p' is `p'.
non_invertible :: P.Permute -> Bool
non_invertible p = p == P.inverse p

-- | Generate a permutation from the cycles `c'.
from_cycles :: [[Int]] -> P.Permute
from_cycles c = P.cyclesPermute (sum (map length c)) c

-- | Generate all permutations of size `n'.
permutations_n :: Int -> [P.Permute]
permutations_n n =
    let f p = let r = P.next p
              in maybe [p] (\np -> p : f np) r
    in f (P.permute n)

-- | Generate all permutations of `i'.
permutations_l :: (Eq a) => [a] -> [[a]]
permutations_l i =
    let f p = apply_permutation p i
    in map f (permutations_n (length i))

-- | Generate all distinct permutations of a multi-set.
multiset_permutations :: (Ord a) => [a] -> [[a]]
multiset_permutations = C.permutations . C.fromList

-- | Composition of `q` then `p`.
compose :: P.Permute -> P.Permute -> P.Permute
compose p q =
    let n = P.size q
        i = [1 .. n]
        j = apply_permutation p i
        k = apply_permutation q j
    in permutation i k

-- | Two line notation of `p'.
two_line :: P.Permute -> ([Int],[Int])
two_line p =
    let n = P.size p
        i = [1..n]
    in (i,apply_permutation p i)

-- | One line notation of `p'.
one_line :: P.Permute -> [Int]
one_line = snd . two_line

-- | Multiplication table of symmetric group `n'.
multiplication_table :: Int -> [[P.Permute]]
multiplication_table n =
    let ps = permutations_n n
        f p = map (compose p) ps
    in map f ps

{-
let p = permutation [1..4] [4,3,2,1] -- [[0,3],[1,2]]
let q = permutation [1..4] [2,3,4,1] -- [[0,1,2,3]]
(p,non_invertible p,P.cycles p,apply_permutation p [1..4])
(q,non_invertible q,P.cycles q,apply_permutation q [1..4])

let f = from_cycles [[0,3],[1,2]]
let g = from_cycles [[0,1,2,3]]
apply_permutation f [1..4] -- [4,3,2,1]
apply_permutation g [1..4] -- [2,3,4,1]

let p = permutation [1..5] [3,2,1,5,4] -- [[0,2],[1],[3,4]]
let q = permutation [1..5] [2,5,4,3,1] -- [[0,1,4],[2,3]]
let r = permutation [1..5] [2,4,5,1,3] -- [[0,1,3],[2,4]]
(non_invertible p,P.cycles p,apply_permutation p [1..5])
(non_invertible q,P.cycles q,apply_permutation q [1..5])
(non_invertible r,P.cycles r,apply_permutation r [1..5])

let f = from_cycles ([[0,2],[1],[3,4]])
let g = from_cycles ([[0,1,4],[2,3]])
let h = from_cycles ([[0,1,3],[2,4]])
let i = f `compose` g
apply_permutation f [1..5] == [3,2,1,5,4]
apply_permutation g [1..5] == [2,5,4,3,1]
apply_permutation h [1..5] == [2,4,5,1,3]
apply_permutation i [1..5] == [2,4,5,1,3]
map one_line [f,g,h,i]

map one_line (permutations_n 3)
map (map one_line) (multiplication_table 3)

  123 132 213 231 312 321
  132 123 312 321 213 231
  213 231 123 132 321 312
  231 213 321 312 123 132
  312 321 132 123 231 213
  321 312 231 213 132 123

map P.cycles (permutations_n 4)
partition not (map non_invertible (permutations_n 4))
-}
