module Music.Theory.Set where

import Control.Monad
import Data.List

-- | Remove duplicate elements and sort.
set :: (Ord a) => [a] -> [a]
set = sort . nub

-- | Powerset, ie. set of all all subsets.
powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

-- | Two element subsets (cf [2] . powerset).
dyads :: [a] -> [(a,a)]
dyads [] = []
dyads (x:xs) = dyads xs ++ [ (x,y) | y <- xs ]

-- | Set expansion
se :: (Ord a) => Int -> [a] -> [[a]]
se n xs = if length xs == n 
          then [xs] 
          else nub (concatMap (se n) [sort (y : xs) | y <- xs])
