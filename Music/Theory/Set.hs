-- | Set operations on lists.
module Music.Theory.Set where

import Control.Monad
import Data.List
import qualified Data.Set as S {- containers -}
import qualified Math.Combinatorics.Multiset as M {- multiset-comb -}

-- | Synonym for 'S.Set'.
type Set = S.Set

-- | Remove duplicate elements with 'nub' and then 'sort'.
--
-- > set_l [3,3,3,2,2,1] == [1,2,3]
set_l :: (Ord a) => [a] -> [a]
set_l = sort . nub

set :: (Ord a) => [a] -> Set a
set = S.fromList

-- | Powerset, ie. set of all subsets.
--
-- > sort (powerset_l [1,2]) == [[],[1],[1,2],[2]]
powerset_l :: [a] -> [[a]]
powerset_l = filterM (const [True,False])

-- > powerset (set [1,2])
powerset :: Ord a => Set a -> Set (Set a)
powerset = S.fromList . map S.fromList . powerset_l . S.elems

-- | Two element subsets (cf [2] . powerset).
--
-- > dyads_l [1,2,3] == [(1,2),(1,3),(2,3)]
dyads_l :: [a] -> [(a,a)]
dyads_l s =
    case s of
      [] -> []
      x:xs -> [(x,y) | y <- xs] ++ dyads_l xs

dyads :: Ord a => Set a -> Set (a,a)
dyads = set . dyads_l . S.elems

-- | Set expansion (ie. to multiset of degree /n/).
--
-- > se 4 [1,2,3] == [[1,1,2,3],[1,2,2,3],[1,2,3,3]]
se :: (Ord a) => Int -> [a] -> [[a]]
se n xs =
    if length xs >= n
    then [xs]
    else nub (concatMap (se n) [sort (y : xs) | y <- xs])

-- | All distinct multiset partitions, see 'M.partitions'.
--
-- > partitions_l "aab" == [["aab"],["a","ab"],["b","aa"],["b","a","a"]]
--
-- > partitions_l "abc" == [["abc"]
-- >                       ,["bc","a"],["b","ac"]
-- >                       ,["c","ab"],["c","b","a"]]
partitions_l :: Eq a => [a] -> [[[a]]]
partitions_l = map (map M.toList . M.toList) . M.partitions . M.fromListEq

-- | Cartesian product of two sets.
--
-- > let r = [('a',1),('a',2),('b',1),('b',2),('c',1),('c',2)]
-- > in cartesian_product "abc" [1,2] == r
cartesian_product :: [a] -> [b] -> [(a,b)]
cartesian_product p q = [(i,j) | i <- p, j <- q]
