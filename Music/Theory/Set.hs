module Music.Theory.Set (powerset, permutations) where

import Control.Monad

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

permutations :: [a] -> [[a]]
permutations []     = [[]]
permutations (x:xs) = [ zs | ys <- permutations xs , zs <- everywhere x ys ]

everywhere :: a -> [a] -> [[a]]
everywhere x []     = [[x]]
everywhere x (y:ys) = (x:y:ys) : [ y:zs | zs <- everywhere x ys ]
