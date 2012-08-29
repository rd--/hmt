-- | Set operations on 'Set's.
module Music.Theory.Set.Set where

import qualified Data.Set as S {- containers -}
import qualified Music.Theory.Set.List as L

set :: (Ord a) => [a] -> S.Set a
set = S.fromList

-- > powerset (set [1,2])
powerset :: Ord a => S.Set a -> S.Set (S.Set a)
powerset = S.fromList . map S.fromList . L.powerset . S.elems

pairs :: Ord a => S.Set a -> S.Set (a,a)
pairs = set . L.pairs . S.elems
