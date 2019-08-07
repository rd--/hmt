-- | Kevin Jones. "Compositional Applications of Stochastic Processes".
--   Computer Music Journal, 5(2):45-58, 1981.
module Music.Theory.Random.Jones1981 where

import Data.List {- base -}
import Data.Maybe {- base -}
import System.Random {- random -}

-- * Stochastic Finite State Grammars

data G a = E | T a | P (G a) (G a) deriving (Eq,Show)
data K k = K_E | K_T | K_P k deriving (Eq,Show)

type Rule k a = k -> a -> Maybe (a,a)
type Probablities k r = (r,r,[(k,r)])
type SFSG k a r = (Rule k a,Probablities k r)

-- > p_verify (1/4,1/4,[('a',1/4),('b',1/4)]) == True
p_verify :: (Eq a,Num a) => Probablities k a -> Bool
p_verify (e,t,k) = sum (e:t:map snd k) == 1

p_select :: (Ord a, Num a) => Probablities k a -> a -> Maybe (K k)
p_select (e,t,k) =
  let windex w n = findIndex (n <) (scanl1 (+) w)
      (kk,kn) = unzip k
      f i = case i of
              0 -> K_E
              1 -> K_T
              n -> K_P (kk !! (n - 2))
  in fmap f . windex (e:t:kn)

-- > let p = (1/4,1/4,[('a',1/4),('b',1/4)])
-- > map (p_select_err p) [0,0.25..0.75] == [K_E,K_T,K_P 'a',K_P 'b']
p_select_err :: (Ord a, Num a) => Probablities k a -> a -> K k
p_select_err p = fromMaybe (error "p_select") . p_select p

p_choose :: (RandomGen g,Random n,Ord n,Num n) => Probablities k n -> g -> (Maybe (K k),g)
p_choose p g = let (n,g') = randomR (0,1) g in (p_select p n,g')

g_collect :: G a -> [a]
g_collect g =
    case g of
      E -> []
      T e -> [e]
      P p q -> g_collect p ++ g_collect q

unfold :: (RandomGen g,Random r,Ord r,Num r) => SFSG k a r -> a -> g -> (G a,g)
unfold (r,p) st g =
    let (n,g') = randomR (0,1) g
    in case p_select p n of
         Just K_T -> (T st,g')
         Just (K_P k) -> case r k st of
                           Nothing -> (E,g')
                           Just (i,j) ->
                               let (i',g'') = unfold (r,p) i g'
                                   (j',g''') = unfold (r,p) j g''
                               in (P i' j',g''')
         _ -> (E,g')

chain :: (RandomGen t,Random r,Ord r,Num r) => SFSG k a r -> a -> t -> [G a]
chain gr st g =
    let (x,g') = unfold gr st g
    in x : chain gr st g'

chain_n :: (RandomGen g,Random r,Ord r,Num r) => Int -> SFSG k a r -> a -> g -> [G a]
chain_n n gr st = take n . chain gr st
