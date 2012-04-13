-- | Shared list functions.
module Music.Theory.List where

import Data.Function
import Data.List
import Data.List.Split {- split -}

-- | Bracket sequence with left and right values.
--
-- > bracket ('<','>') "1,2,3" == "<1,2,3>"
bracket :: (a,a) -> [a] -> [a]
bracket (l,r) x = l : x ++ [r]

genericRotate_left :: Integral i => i -> [a] -> [a]
genericRotate_left n =
    let f (p,q) = q ++ p
    in f . genericSplitAt n

-- | Left rotation.
--
-- > rotate_left 1 [1..3] == [2,3,1]
-- > rotate_left 3 [1..5] == [4,5,1,2,3]
rotate_left :: Int -> [a] -> [a]
rotate_left = genericRotate_left

genericRotate_right :: Integral n => n -> [a] -> [a]
genericRotate_right n = reverse . genericRotate_left n . reverse

-- | Right rotation.
--
-- > rotate_right 1 [1..3] == [3,1,2]
rotate_right :: Int -> [a] -> [a]
rotate_right = genericRotate_right

-- | Rotate left by /n/ 'mod' /#p/ places.
--
-- > rotate 8 [1..5] == [4,5,1,2,3]
rotate :: (Integral n) => n -> [a] -> [a]
rotate n p =
    let m = n `mod` genericLength p
    in genericRotate_left m p

-- | Rotate right by /n/ places.
--
-- > rotate_r 8 [1..5] == [3,4,5,1,2]
rotate_r :: (Integral n) => n -> [a] -> [a]
rotate_r = rotate . negate

-- | All rotations.
--
-- > rotations [0,1,3] == [[0,1,3],[1,3,0],[3,0,1]]
rotations :: [a] -> [[a]]
rotations p = map (`rotate_left` p) [0 .. length p - 1]

genericAdj2 :: (Integral n) => n -> [t] -> [(t,t)]
genericAdj2 n l =
    case l of
      p:q:_ -> (p,q) : genericAdj2 n (genericDrop n l)
      _ -> []

-- | Adjacent elements of list, at indicated distance, as pairs.
--
-- > adj2 1 [1..5] == [(1,2),(2,3),(3,4),(4,5)]
-- > adj2 2 [1..4] == [(1,2),(3,4)]
-- > adj2 3 [1..5] == [(1,2),(4,5)]
adj2 :: Int -> [t] -> [(t,t)]
adj2 = genericAdj2

-- | Append first element to end of list.
--
-- > close [1..3] == [1,2,3,1]
close :: [a] -> [a]
close x =
    case x of
      [] -> []
      e:_ -> x ++ [e]

-- | 'adj2' '.' 'close'.
--
-- > adj2_cyclic 1 [1..3] == [(1,2),(2,3),(3,1)]
adj2_cyclic :: Int -> [t] -> [(t,t)]
adj2_cyclic n = adj2 n . close

-- | Interleave elements of /p/ and /q/.
--
-- > interleave [1..3] [4..6] == [1,4,2,5,3,6]
interleave :: [b] -> [b] -> [b]
interleave p q =
    let u (i,j) = [i,j]
    in concatMap u (zip p q)

-- | 'interleave' of 'rotate_left' by /i/ and /j/.
--
-- > interleave_rotations 9 3 [1..13] == [10,4,11,5,12,6,13,7,1,8,2,9,3,10,4,11,5,12,6,13,7,1,8,2,9,3]
interleave_rotations :: Int -> Int -> [b] -> [b]
interleave_rotations i j s = interleave (rotate_left i s) (rotate_left j s)

-- | Count occurences of elements in list.
--
-- > histogram "hohoh" == [('h',3),('o',2)]
histogram :: (Ord a,Integral i) => [a] -> [(a,i)]
histogram x =
    let g = group (sort x)
        n = map genericLength g
    in zip (map head g) n

-- | List segments of length /i/ at distance /j/.
--
-- > segments 2 1 [1..5] == [[1,2],[2,3],[3,4],[4,5]]
-- > segments 2 2 [1..5] == [[1,2],[3,4]]
segments :: Int -> Int -> [a] -> [[a]]
segments i j p =
    let q = take i p
        p' = drop j p
    in if length q /= i then [] else q : segments i j p'

-- | 'foldl1' 'intersect'.
--
-- > intersect_l [[1,2],[1,2,3],[1,2,3,4]] == [1,2]
intersect_l :: Eq a => [[a]] -> [a]
intersect_l = foldl1 intersect

-- | 'foldl1' 'union'.
--
-- > sort (union_l [[1,3],[2,3],[3]]) == [1,2,3]
union_l :: Eq a => [[a]] -> [a]
union_l = foldl1 union

-- | Intersection of adjacent elements of list at distance /n/.
--
-- > adj_intersect 1 [[1,2],[1,2,3],[1,2,3,4]] == [[1,2],[1,2,3]]
adj_intersect :: Eq a => Int -> [[a]] -> [[a]]
adj_intersect n = map intersect_l . segments 2 n

-- | List of cycles at distance /n/.
--
-- > cycles 2 [1..6] == [[1,3,5],[2,4,6]]
-- > cycles 3 [1..9] == [[1,4,7],[2,5,8],[3,6,9]]
-- > cycles 4 [1..8] == [[1,5],[2,6],[3,7],[4,8]]
cycles :: Int -> [a] -> [[a]]
cycles n = transpose . splitEvery n

-- * Association lists

-- | Collate values of equal keys at /assoc/ list.
--
-- > collate [(1,'a'),(2,'b'),(1,'c')] == [(1,"ac"),(2,"b")]
collate :: Ord a => [(a,b)] -> [(a,[b])]
collate =
    let f l = (fst (head l), map snd l)
    in map f . groupBy ((==) `on` fst) . sortBy (compare `on` fst)

-- | Make /assoc/ list with given /key/.
--
-- > with_key 'a' [1..3] == [('a',1),('a',2),('a',3)]
with_key :: k -> [v] -> [(k,v)]
with_key h = zip (repeat h)

-- | Intervals to values, zero is /n/.
--
-- > dx_d 5 [1,2,3] == [5,6,8,11]
dx_d :: (Num a) => a -> [a] -> [a]
dx_d = scanl (+)

-- | Integrate, ie. pitch class segment to interval sequence.
--
-- > d_dx [5,6,8,11] == [1,2,3]
d_dx :: (Num a) => [a] -> [a]
d_dx l = zipWith (-) (tail l) l

-- | Elements of /p/ not in /q/.
--
-- > [1,2,3] `difference` [1,2] == [3]
difference :: (Eq a) => [a] -> [a] -> [a]
difference p q =
    let f e = e `notElem` q
    in filter f p

-- | Is /p/ a subset of /q/, ie. is 'intersect' of /p/ and /q/ '==' /p/.
--
-- > is_subset [1,2] [1,2,3] == True
is_subset :: Eq a => [a] -> [a] -> Bool
is_subset p q = p `intersect` q == p

-- | Is /p/ a superset of /q/, ie. 'flip' 'is_subset'.
--
-- > is_superset [1,2,3] [1,2] == True
is_superset :: Eq a => [a] -> [a] -> Bool
is_superset = flip is_subset

-- | Is /p/ a subsequence of /q/, ie. synonym for 'isInfixOf'.
--
-- > subsequence [1,2] [1,2,3] == True
subsequence :: (Eq a) => [a] -> [a] -> Bool
subsequence = isInfixOf

-- | Variant of 'elemIndices' that requires /e/ to be unique in /p/.
--
-- > elem_index_unique 'a' "abcda" == undefined
elem_index_unique :: (Eq a) => a -> [a] -> Int
elem_index_unique e p =
    case elemIndices e p of
      [i] -> i
      _ -> error "elem_index_unique"
