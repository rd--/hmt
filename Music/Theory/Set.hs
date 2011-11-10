-- | Set operations on lists.
module Music.Theory.Set where

import Control.Monad
import Data.List

-- | Remove duplicate elements with 'nub' and then 'sort'.
--
-- > set [3,3,3,2,2,1] == [1,2,3]
set :: (Ord a) => [a] -> [a]
set = sort . nub

-- | Powerset, ie. set of all subsets.
--
-- > sort (powerset [1,2]) == [[],[1],[1,2],[2]]
powerset :: [a] -> [[a]]
powerset = filterM (const [True,False])

-- | Two element subsets (cf [2] . powerset).
--
-- > dyads [1,2,3] == [(1,2),(1,3),(2,3)]
dyads :: [a] -> [(a,a)]
dyads s =
    case s of
      [] -> []
      x:xs -> [(x,y) | y <- xs] ++ dyads xs

-- | Set expansion.
--
-- > se 4 [1,2,3] == [[1,1,2,3],[1,2,2,3],[1,2,3,3]]
se :: (Ord a) => Int -> [a] -> [[a]]
se n xs =
    if length xs == n
    then [xs]
    else nub (concatMap (se n) [sort (y : xs) | y <- xs])
