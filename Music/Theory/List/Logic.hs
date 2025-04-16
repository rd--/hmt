-- | List/Logic functions.
module Music.Theory.List.Logic where

import Control.Monad {- base -}

import qualified Control.Monad.Logic as Logic {- logict -}

import qualified Music.Theory.List as List {- hmt-base -}

-- | 'Logic.MonadLogic' value to enumerate indices for all embeddings of /q/ in /p/.
all_embeddings_m :: (Eq t, MonadPlus m, Logic.MonadLogic m) => [t] -> [t] -> m [Int]
all_embeddings_m p q =
  let q_n = length q
      recur p' q' n k =
        -- n = length k
        if n == q_n
          then return (reverse k)
          else do
            (m, c) <- msum (map return p')
            let k0 = List.head_err k
                c' = List.head_err q'
            guard (c == c' && (null k || m > k0))
            let p'' = List.tail_err p'
                q'' = List.tail_err q'
            recur p'' q'' (n + 1) (m : k)
  in recur (zip [0 ..] p) q 0 []

{- | 'Logic.observeAll' of 'all_embeddings_m'

>> all_embeddings "all_embeddings" "leg"
[[1,4,12],[1,7,12],[2,4,12],[2,7,12]]
-}
all_embeddings :: Eq t => [t] -> [t] -> [[Int]]
all_embeddings p = Logic.observeAll . all_embeddings_m p
