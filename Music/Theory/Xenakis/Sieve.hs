module Music.Theory.Xenakis.Sieve where

import qualified Data.List as L

data Sieve = Empty
           | L (Integer,Integer)
           | Union Sieve Sieve
           | Intersection Sieve Sieve
             deriving (Eq,Show)

union :: [Sieve] -> Sieve
union = foldl1 Union

intersection :: [Sieve] -> Sieve
intersection = foldl1 Intersection

normalise :: Sieve -> Sieve
normalise s =
    case s of
      Empty -> Empty
      L (m,i) -> L (m,i `mod` m)
      Union s0 s1 -> Union (normalise s0) (normalise s1)
      Intersection s0 s1 -> Intersection (normalise s0) (normalise s1)

is_normal :: Sieve -> Bool
is_normal s = s == normalise s

element :: Sieve -> Integer -> Bool
element s n =
    case s of
      Empty -> False
      L (m,i) -> n `mod` m == i `mod` m && n >= i
      Union s0 s1 -> element s0 n || element s1 n
      Intersection s0 s1 -> element s0 n && element s1 n

merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge f p q =
    case (p,q) of
      ([],q') -> q'
      (p',[]) -> p'
      (i:p',j:q') -> case i `f` j of
                       GT -> j : merge f (i:p') q'
                       _ -> i : merge f p' (j:q')

-- | Note that building a sieve that contains an intersection clause
--   that has no elements gives _|_.
build :: Sieve -> [Integer]
build s =
    let u_f = map head . L.group
        i_f = let g [x,_] = [x]
                  g _ = []
              in concatMap g . L.group
    in case s of
         Empty -> []
         L (m,i) -> [i, i+m ..]
         Union s0 s1 -> u_f (merge compare (build s0) (build s1))
         Intersection s0 s1 -> i_f (merge compare (build s0) (build s1))

differentiate :: (Num a) => [a] -> [a]
differentiate l = zipWith (-) (tail l) l

euclid :: (Integral a) => a -> a -> a
euclid i j =
    let k = i `mod` j
    in if k == 0 then j else euclid j k

de_meziriac :: (Integral a) => a -> a -> a
de_meziriac i j =
    let f t = if (t * i) `mod` j /= 1
              then f (t + 1)
              else t
    in if j == 1 then 1 else f 1

reduce_intersection :: (Integral t) => (t,t) -> (t,t) -> Maybe (t,t)
reduce_intersection (m1,i1) (m2,i2) =
    let d = euclid m1 m2
        i1' = i1 `mod` m1
        i2' = i2 `mod` m2
        c1 = m1 `div` d
        c2 = m2 `div` d
        m3 = d * c1 * c2
        t = de_meziriac c1 c2
        i3 = (i1' + t * (i2' - i1') * c1) `mod` m3
    in if d /= 1 && (i1' - i2') `mod` d /= 0
       then Nothing
       else Just (m3,i3)

reduce :: Sieve -> Sieve
reduce s =
    let f g s1 s2 =
            let s1' = reduce s1
                s2' = reduce s2
                s' = g s1' s2'
            in if s1 == s1' && s2 == s2'
               then s'
               else reduce s'
    in case s of
         Empty -> Empty
         L _ -> s
         Union s1 Empty -> s1
         Union s1 s2 -> f Union s1 s2
         Intersection s1 Empty -> s1
         Intersection (L p) (L q) -> maybe Empty L (reduce_intersection p q)
         Intersection s1 s2 -> f Intersection s1 s2
