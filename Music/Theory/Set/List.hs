-- | Set operations on lists.
module Music.Theory.Set.List where

import Control.Monad {- base -}
import Data.List {- base -}
import qualified Math.Combinatorics.Multiset as M {- multiset-comb -}

import qualified Music.Theory.List as T {- hmt -}

-- | Remove duplicate elements with 'nub' and then 'sort'.
--
-- > set_l [3,3,3,2,2,1] == [1,2,3]
set :: (Ord a) => [a] -> [a]
set = sort . nub

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
-- > powerset' [1,2,3] == [[1],[2],[3],[1,2],[1,3],[2,3],[1,2,3]]
powerset' :: Ord a => [a] -> [[a]]
powerset' = tail . T.sort_by_two_stage length id . powerset

-- | Two element subsets.
--
-- > pairs [1,2,3] == [(1,2),(1,3),(2,3)]
pairs :: [a] -> [(a,a)]
pairs s =
    case s of
      [] -> []
      x:s' -> [(x,y) | y <- s'] ++ pairs s'

-- | Three element subsets.
--
-- > triples [1..4] == [(1,2,3),(1,2,4),(1,3,4),(2,3,4)]
--
-- > let f n = genericLength (triples [1..n]) == nk_combinations n 3
-- > in all f [1..15]
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

-- | All distinct multiset partitions, see 'M.partitions'.
--
-- > partitions "aab" == [["aab"],["a","ab"],["b","aa"],["b","a","a"]]
--
-- > partitions "abc" == [["abc"]
-- >                     ,["bc","a"],["b","ac"],["c","ab"]
-- >                     ,["c","b","a"]]
partitions :: Eq a => [a] -> [[[a]]]
partitions = map (map M.toList . M.toList) . M.partitions . M.fromListEq

-- | Cartesian product of two sets.
--
-- > let r = [('a',1),('a',2),('b',1),('b',2),('c',1),('c',2)]
-- > in cartesian_product "abc" [1,2] == r
cartesian_product :: [a] -> [b] -> [(a,b)]
cartesian_product p q = [(i,j) | i <- p, j <- q]
