-- | Robert Morris and D. Starr. \"The Structure of All-Interval Series\".
-- /Journal of Music Theory/, 18:364-389, 1974.
module Music.Theory.Z12.Morris_1974 where

import qualified Control.Monad.Logic as L {- logict -}

-- | 'L.msum' '.' 'map' 'return'.
--
-- > L.observeAll (fromList [1..7]) == [1..7]
fromList :: L.MonadPlus m => [a] -> m a
fromList = L.msum . map return

-- | 'L.MonadLogic' all-interval series.
--
-- > map (length . L.observeAll . all_interval_m) [4,6,8,10] == [2,4,24,288]
-- > [0,1,3,2,9,5,10,4,7,11,8,6] `elem` L.observeAll (all_interval_m 12)
-- > length (L.observeAll (all_interval_m 12)) == 3856
all_interval_m :: L.MonadLogic m => Int -> m [Int]
all_interval_m n =
    let recur k p q = -- k = length p
            if k == n
            then return (reverse p)
            else do i <- fromList [1 .. n - 1]
                    L.guard (i `notElem` p)
                    let j:_ = p
                        m = abs ((i - j) `mod` n)
                    L.guard (m `notElem` q)
                    recur (k + 1) (i : p) (m : q)
    in recur 1 [0] []

-- | 'L.observeAll' of 'all_interval_m'.
--
-- > let r = [[0,1,5,2,4,3],[0,2,1,4,5,3],[0,4,5,2,1,3],[0,5,1,4,2,3]]
-- > in all_interval 6 == r
all_interval :: Int -> [[Int]]
all_interval = L.observeAll . all_interval_m
