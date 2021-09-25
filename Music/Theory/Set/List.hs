-- | Set operations on lists.
module Music.Theory.Set.List where

import Control.Monad {- base -}
import Data.List {- base -}

import qualified Math.Combinatorics.Multiset as M {- multiset-comb -}

import qualified Music.Theory.List as T {- hmt-base -}

-- | 'sort' then 'nub'.
--
-- > set [3,3,3,2,2,1] == [1,2,3]
set :: (Ord a) => [a] -> [a]
set = nub . sort

-- | Size of powerset of set of cardinality /n/, ie. @2@ '^' /n/.
--
-- > map n_powerset [6..9] == [64,128,256,512]
n_powerset :: Integral n => n -> n
n_powerset = (^) 2

-- | Powerset, ie. set of all subsets.
--
-- > sort (powerset [1,2]) == [[],[1],[1,2],[2]]
-- > map length (map (\n -> powerset [1..n]) [6..9]) == [64,128,256,512]
powerset :: [a] -> [[a]]
powerset = filterM (const [True,False])

-- | Variant where result is sorted and the empty set is not given.
--
-- > powerset_sorted [1,2,3] == [[1],[2],[3],[1,2],[1,3],[2,3],[1,2,3]]
powerset_sorted :: Ord a => [a] -> [[a]]
powerset_sorted = tail . T.sort_by_two_stage_on length id . powerset

-- | Two element subsets.
--
-- > pairs [1,2,3] == [(1,2),(1,3),(2,3)]
pairs :: [a] -> [(a,a)]
pairs s =
    case s of
      [] -> []
      x:s' -> [(x,y) | y <- s'] ++ pairs s'

{- | Three element subsets.

> triples [1..4] == [(1,2,3),(1,2,4),(1,3,4),(2,3,4)]

> import Music.Theory.Combinations
> let f n = genericLength (triples [1..n]) == nk_combinations n 3
> all f [1..15]
-}
triples :: [a] -> [(a,a,a)]
triples s =
    case s of
      [] -> []
      x:s' -> [(x,y,z) | (y,z) <- pairs s'] ++ triples s'

-- | Set expansion (ie. to multiset of degree /n/).
--
-- > expand_set 4 [1,2,3] == [[1,1,2,3],[1,2,2,3],[1,2,3,3]]
expand_set :: (Ord a) => Int -> [a] -> [[a]]
expand_set n xs =
    if length xs >= n
    then [xs]
    else nub (concatMap (expand_set n) [sort (y : xs) | y <- xs])

{- | All distinct multiset partitions, see 'M.partitions'.

> partitions "aab" == [["aab"],["a","ab"],["b","aa"],["b","a","a"]]
> partitions "abc" == [["abc"],["bc","a"],["b","ac"],["c","ab"],["c","b","a"]]
-}
partitions :: Eq a => [a] -> [[[a]]]
partitions = map (map M.toList . M.toList) . M.partitions . M.fromListEq

{- | Cartesian product of two sets.

> cartesian_product "abc" [1,2] == [('a',1),('a',2),('b',1),('b',2),('c',1),('c',2)]
> cartesian_product "abc" "" == []
-}
cartesian_product :: [a] -> [b] -> [(a,b)]
cartesian_product p q = [(i,j) | i <- p, j <- q]

-- | List form of n-fold cartesian product.
--
-- > length (nfold_cartesian_product [[1..13],[1..4]]) == 52
-- > length (nfold_cartesian_product ["abc","de","fgh"]) == 3 * 2 * 3
nfold_cartesian_product :: [[a]] -> [[a]]
nfold_cartesian_product l =
    case l of
      [] -> []
      [_] -> []
      [x,y] -> [[i,j] | i <- x, j <- y]
      x:l' -> concatMap (\e -> map (e :) (nfold_cartesian_product l')) x

{- | Generate all distinct cycles, aka necklaces, with elements taken from a multiset.

> concatMap multiset_cycles [replicate i 0 ++ replicate (6 - i) 1 | i <- [0 .. 6]]
-}
multiset_cycles :: Ord t => [t] -> [[t]]
multiset_cycles = M.cycles . M.fromList
