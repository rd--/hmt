-- | Combination functions.
module Music.Theory.Combinations where

import qualified Music.Theory.Permutations as T

-- | Number of /k/ element combinations of a set of /n/ elements.
--
-- > (nk_combinations 6 3,nk_combinations 13 3) == (20,286)
nk_combinations :: Integral a => a -> a -> a
nk_combinations n k = T.nk_permutations n k `div` T.factorial k

-- | /k/ element subsets of /s/.
--
-- > combinations 3 [1..4] == [[1,2,3],[1,2,4],[1,3,4],[2,3,4]]
-- > length (combinations 3 [1..5]) == nk_combinations 5 3
combinations :: Int -> [a] -> [[a]]
combinations k s =
    case (k,s) of
      (0,_) -> [[]]
      (_,[]) -> []
      (_,e:s') -> map (e :) (combinations (k - 1) s') ++ combinations k s'
