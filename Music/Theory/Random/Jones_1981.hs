{- | Kevin Jones. "Compositional Applications of Stochastic Processes".
  Computer Music Journal, 5(2):45-58, 1981.
-}
module Music.Theory.Random.Jones_1981 where

import Data.List {- base -}
import Data.Maybe {- base -}
import System.Random {- random -}

-- * Stochastic Finite State Grammars

data G a = T a | P (G a) (G a) deriving (Eq, Show)

type Rule k a = k -> a -> Maybe (a, a)
type Probablities k r = (r, [(k, r)])
type SFSG k a r = (Rule k a, Probablities k r)

-- > p_verify (1/2,[('a',1/4),('b',1/4)]) == True
p_verify :: (Eq a, Num a) => Probablities k a -> Bool
p_verify (t, k) = sum (t : map snd k) == 1

p_select :: (Ord a, Num a) => Probablities k a -> a -> Maybe (Maybe k)
p_select (t, k) =
  let windex w n = findIndex (n <) (scanl1 (+) w)
      (kk, kn) = unzip k
      f i = case i of
        0 -> Nothing
        _ -> Just (kk !! (i - 1))
  in fmap f . windex (t : kn)

-- > let p = (1/2,[('a',1/4),('b',1/4)])
-- > map (p_select_err p) [0,0.5,0.75] == [Nothing,Just 'a',Just 'b']
p_select_err :: (Ord a, Num a) => Probablities k a -> a -> Maybe k
p_select_err p = fromMaybe (error "p_select") . p_select p

g_collect :: G a -> [a]
g_collect g =
  case g of
    T e -> [e]
    P p q -> g_collect p ++ g_collect q

unfold :: (RandomGen g, Random r, Ord r, Num r) => SFSG k a r -> a -> g -> (G a, g)
unfold (r, p) st g =
  let (n, g') = randomR (0, 1) g
  in case p_select_err p n of
      Nothing -> (T st, g')
      Just k ->
        case r k st of
          Nothing -> (T st, g')
          Just (i, j) ->
            let (i', g'') = unfold (r, p) i g'
                (j', g''') = unfold (r, p) j g''
            in (P i' j', g''')

sfsg_chain :: (RandomGen g, Random r, Ord r, Num r) => SFSG k a r -> a -> g -> [G a]
sfsg_chain gr st g =
  let (x, g') = unfold gr st g
  in x : sfsg_chain gr st g'

sfsg_chain_n :: (RandomGen g, Random r, Ord r, Num r) => Int -> SFSG k a r -> a -> g -> [G a]
sfsg_chain_n n gr st = take n . sfsg_chain gr st
