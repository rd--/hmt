-- | \"Sieves\" by Iannis Xenakis and John Rahn
-- /Perspectives of New Music/
-- Vol. 28, No. 1 (Winter, 1990), pp. 58-78
module Music.Theory.Xenakis.Sieve where

import qualified Data.List as L

-- | Synonym for 'Integer'
type I = Integer

-- | A Sieve.
data Sieve = Empty -- ^ 'Empty' 'Sieve'
           | L (I,I) -- ^ Primitive 'Sieve' of /modulo/ and /index/
           | Union Sieve Sieve -- ^ 'Union' of two 'Sieve's
           | Intersection Sieve Sieve -- ^ 'Intersection' of two 'Sieve's
             deriving (Eq,Show)

-- | The 'Union' of a list of 'Sieve's, ie. 'foldl1' 'Union'.
union :: [Sieve] -> Sieve
union = foldl1 Union

-- | The 'Intersection' of a list of 'Sieve's, ie. 'foldl1' 'Intersection'.
intersection :: [Sieve] -> Sieve
intersection = foldl1 Intersection

-- | Unicode synonym for 'Union'.
(∪) :: Sieve -> Sieve -> Sieve
(∪) = Union

-- | Unicode synonym for 'Intersection'.
(∩) :: Sieve -> Sieve -> Sieve
(∩) = Intersection

-- | Variant of 'L', ie. 'curry' 'L'.
--
-- > l 15 19 == L (15,19)
l :: I -> I -> Sieve
l = curry L

-- | In a /normal/ 'Sieve' /m/ is '>' /i/.
--
-- > normalise (L (15,19)) == L (15,4)
normalise :: Sieve -> Sieve
normalise s =
    case s of
      Empty -> Empty
      L (m,i) -> L (m,i `mod` m)
      Union s0 s1 -> Union (normalise s0) (normalise s1)
      Intersection s0 s1 -> Intersection (normalise s0) (normalise s1)

-- | Predicate to test if a 'Sieve' is /normal/.
--
-- > is_normal (L (15,4)) == True
is_normal :: Sieve -> Bool
is_normal s = s == normalise s

-- | Predicate to determine if an 'I' is an element of the 'Sieve'.
--
-- > map (element (L (3,1))) [1..4] == [True,False,False,True]
-- > map (element (L (15,4))) [4,19 .. 49] == [True,True,True,True]
element :: Sieve -> I -> Bool
element s n =
    case s of
      Empty -> False
      L (m,i) -> n `mod` m == i `mod` m && n >= i
      Union s0 s1 -> element s0 n || element s1 n
      Intersection s0 s1 -> element s0 n && element s1 n

-- | Given a comparison function, merge two ascending lists.
--
-- > merge compare [1,3,5] [2,4] == [1..5]
merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge f p q =
    case (p,q) of
      ([],q') -> q'
      (p',[]) -> p'
      (i:p',j:q') -> case i `f` j of
                       GT -> j : merge f (i:p') q'
                       _ -> i : merge f p' (j:q')

-- | Construct the sequence defined by a 'Sieve'.  Note that building
-- a sieve that contains an intersection clause that has no elements
-- gives @_|_@.
--
-- > take 8 (build (union (map (l 12) [0,2,4,5,7,9,11])))
build :: Sieve -> [I]
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

-- | Variant of 'build' that gives the first /n/ places.
--
-- > buildn 6 (union (map (l 8) [0,3,6])) == [0,3,6,8,11,14]
buildn :: Int -> Sieve -> [I]
buildn n = take n . build

-- | Standard differentiation function.
--
-- > differentiate [1,3,6,10] == [2,3,4]
-- > differentiate [0,2,4,5,7,9,11,12] == [2,2,1,2,2,2,1]
differentiate :: (Num a) => [a] -> [a]
differentiate x = zipWith (-) (tail x) x


-- | Euclid's algorithm for computing the greatest common divisor.
--
-- > euclid 1989 867 == 51
euclid :: (Integral a) => a -> a -> a
euclid i j =
    let k = i `mod` j
    in if k == 0 then j else euclid j k

-- | Bachet De Méziriac's algorithm.
--
-- > de_meziriac 15 4 == 3 && euclid 15 4 == 1
de_meziriac :: (Integral a) => a -> a -> a
de_meziriac i j =
    let f t = if (t * i) `mod` j /= 1
              then f (t + 1)
              else t
    in if j == 1 then 1 else f 1

-- | Attempt to reduce the 'Intersection' of two 'L' nodes to a
-- singular 'L' node.
--
-- > reduce_intersection (3,2) (4,7) == Just (12,11)
-- > reduce_intersection (12,11) (6,11) == Just (12,11)
-- > reduce_intersection (12,11) (8,7) == Just (24,23)
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

-- | Reduce the number of nodes at a 'Sieve'.
--
-- > reduce (L (3,2) ∪ Empty) == L (3,2)
-- > reduce (L (3,2) ∩ Empty) == L (3,2)
-- > reduce (L (3,2) ∩ L (4,7)) == L (12,11)
-- > reduce (L (6,9) ∩ L (15,18)) == L (30,3)
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
