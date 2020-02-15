-- | Syntonic tuning.
module Music.Theory.Tuning.Syntonic where

import Data.List {- base -}

import Music.Theory.Tuning {- hmt -}
import Music.Theory.Tuning.Type {- hmt -}

-- | Construct an isomorphic layout of /r/ rows and /c/ columns with
-- an upper left value of /(i,j)/.
mk_isomorphic_layout :: Integral a => a -> a -> (a,a) -> [[(a,a)]]
mk_isomorphic_layout n_row n_col top_left =
    let (a,b) `plus` (c,d) = (a+c,b+d)
        mk_seq 0 _ _ = []
        mk_seq n i z = z : mk_seq (n-1) i (z `plus` i)
        left = mk_seq n_row (-1,1) top_left
    in map (mk_seq n_col (-1,2)) left

-- | A minimal isomorphic note layout.
--
-- > let [i,j,k] = mk_isomorphic_layout 3 5 (3,-4)
-- > [i,take 4 j,(2,-4):take 4 k] == minimal_isomorphic_note_layout
minimal_isomorphic_note_layout :: [[(Int,Int)]]
minimal_isomorphic_note_layout =
    [[(3,-4),(2,-2),(1,0),(0,2),(-1,4)]
       ,[(2,-3),(1,-1),(0,1),(-1,3)]
    ,[(2,-4),(1,-2),(0,0),(-1,2),(-2,4)]]

-- | Make a rank two regular temperament from a list of /(i,j)/
-- positions by applying the scalars /a/ and /b/.
rank_two_regular_temperament :: Integral a => a -> a -> [(a,a)] -> [a]
rank_two_regular_temperament a b = let f (i,j) = i * a + j * b in map f

-- | Syntonic tuning system based on 'mk_isomorphic_layout' of @5@
-- rows and @7@ columns starting at @(3,-4)@ and a
-- 'rank_two_regular_temperament' with /a/ of @1200@ and indicated
-- /b/.
mk_syntonic_tuning :: Int -> [Cents]
mk_syntonic_tuning b =
  let l = mk_isomorphic_layout 5 7 (3,-4)
      t = map (rank_two_regular_temperament 1200 b) l
  in nub (sort (map (\x -> fromIntegral (x `mod` 1200)) (concat t)))

{- | 'mk_syntonic_tuning' of @697@.

> tn_divisions syntonic_697 == 17

> let c = [0,79,194,273,309,388,467,503,582,697,776,812,891,970,1006,1085,1164]
> tn_cents_i syntonic_697 == c
-}
syntonic_697 :: Tuning
syntonic_697 = Tuning (Right (mk_syntonic_tuning 697)) Nothing

-- | 'mk_syntonic_tuning' of @702@.
--
-- > tn_divisions syntonic_702 == 17
--
-- > let c = [0,24,114,204,294,318,408,498,522,612,702,792,816,906,996,1020,1110]
-- > tn_cents_i syntonic_702 == c
syntonic_702 :: Tuning
syntonic_702 = Tuning (Right (mk_syntonic_tuning 702)) Nothing
