-- | Tom Johnson. \"Networks\". In Conference on Mathematics and
-- Computation in Music, Berlin, May 2007.
module Music.Theory.Block_Design.Johnson_2007 where

import Data.Function
import Data.List
import Data.List.Split {- split -}

-- * List

genericRotate :: Integral i => i -> [a] -> [a]
genericRotate n =
    let f (p,q) = q ++ p
    in f . genericSplitAt n

-- | Left rotation.
--
-- > rotate 1 [1..3] == [2,3,1]
rotate :: Int -> [a] -> [a]
rotate = genericRotate

genericRotate_right :: Integral n => n -> [a] -> [a]
genericRotate_right n = reverse . genericRotate n . reverse

-- | Right rotation.
--
-- > rotate_right 1 [1..3] == [3,1,2]
rotate_right :: Int -> [a] -> [a]
rotate_right = genericRotate_right

-- | Make /assoc/ list with given /key/.
with_key :: k -> [v] -> [(k,v)]
with_key h = zip (repeat h)

genericAdj2 :: (Integral n) => n -> [t] -> [(t,t)]
genericAdj2 n l =
    case l of
      p:q:_ -> (p,q) : genericAdj2 n (genericDrop n l)
      _ -> []

-- | Adjacent elements of list at indicated distance as pairs.
--
-- > adj2 1 [1..5] == [(1,2),(2,3),(3,4),(4,5)]
-- > adj2 2 [1..4] == [(1,2),(3,4)]
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

-- | 'adj2' @2@.
--
-- > pairs [1..4] == [(1,2),(3,4)]
pairs :: [t] -> [(t,t)]
pairs = adj2 2

-- | Interleave elements of /p/ and /q/.
--
-- > interleave [1..3] [4..6] == [1,4,2,5,3,6]
interleave :: [b] -> [b] -> [b]
interleave p q =
    let u (i,j) = [i,j]
    in concatMap u (zip p q)

-- | 'interleave' of 'rotate' by /i/ and /j/.
--
-- > interleave_rotations 9 3 [1..13] == [10,4,11,5,12,6,13,7,1,8,2,9,3,10,4,11,5,12,6,13,7,1,8,2,9,3]
interleave_rotations :: Int -> Int -> [b] -> [b]
interleave_rotations i j s = interleave (rotate i s) (rotate j s)

-- | Collate values of equal keys at /assoc/ list.
--
-- > coll [(1,'a'),(2,'b'),(1,'c')] == [(1,"ac"),(2,"b")]
coll :: Ord a => [(a,b)] -> [(a,[b])]
coll =
    let f l = (fst (head l), map snd l)
    in map f . groupBy ((==) `on` fst) . sortBy (compare `on` fst)

-- * Designs

data Design i = Design [i] [[i]]

-- * Johnson (7,3,1), (13,4,1) and (12,4,3)

-- > c_7_3_1 == [1,3,4,2,7,6,5]
c_7_3_1 :: (Num i) => [i]
c_7_3_1 = [1,3,4,2,7,6,5]

-- > b_7_3_1 == ([[1,2,3],[3,4,7],[2,4,6],[2,5,7],[1,6,7],[3,5,6],[1,4,5]]
-- >            ,[[1,2,4],[2,3,7],[4,6,7],[2,5,6],[1,5,7],[1,3,6],[3,4,5]])
b_7_3_1 :: (Ord i,Num i) => ([[i]], [[i]])
b_7_3_1 =
    let c = c_7_3_1
        f i (j1,j2) = sort [i,j1,j2]
    in (zipWith f (rotate 3 c) (adj2_cyclic 1 c)
       ,zipWith f c (adj2_cyclic 1 (rotate 2 c)))

d_7_3_1 :: (Enum n,Ord n,Num n) => (Design n,Design n)
d_7_3_1 =
    (Design [1..7] (fst b_7_3_1)
    ,Design [1..7] (snd b_7_3_1))

-- > length n_7_3_1 == 7 && sort n_7_3_1 == n_7_3_1
n_7_3_1 :: Num i => [(i,i)]
n_7_3_1 = [(3,4),(3,11),(4,1),(4,3),(4,5),(4,7),(5,2)]

-- > b_13_4_1 == ([[1,2,4,10],[2,3,5,11],[3,4,6,12],[4,5,7,13],[1,5,6,8],[2,6,7,9],[3,7,8,10],[4,8,9,11],[5,9,10,12],[6,10,11,13],[1,7,11,12],[2,8,12,13]]
-- >             ,[[4,8,9,11],[5,9,10,12],[6,10,11,13],[1,7,11,12],[2,8,12,13],[1,3,9,13],[1,2,4,10],[2,3,5,11],[3,4,6,12],[4,5,7,13],[1,5,6,8],[2,6,7,9]])
b_13_4_1 :: (Enum i,Num i,Ord i) => ([[i]], [[i]])
b_13_4_1 =
    let c = [1..13]
        c' = rotate 7 c
        d = interleave_rotations 9 3 c
        e = interleave_rotations 3 10 c
        f (i1,i2) (j1,j2) = sort [i1,i2,j1,j2]
    in (zipWith f (adj2 1 c) (adj2 2 d)
       ,zipWith f (adj2 1 c') (adj2 2 e))

d_13_4_1 :: (Enum n,Ord n,Num n) => (Design n,Design n)
d_13_4_1 =
    (Design [1..13] (fst b_13_4_1)
    ,Design [1..13] (snd b_13_4_1))

-- > length n_13_4_1 == 13 && sort n_13_4_1 == n_13_4_1
n_13_4_1 :: Num i => [(i,i)]
n_13_4_1 = [(3,0),(3,2),(3,5),(3,7),(3,10),(4,0),(4,3),(4,5),(4,8),(4,10),(5,1),(5,3),(5,6)]

-- > histogram "histogram" == [('a',1),('g',1),('h',1),('i',1),('m',1),('o',1),('r',1),('s',1),('t',1)]
histogram :: (Ord a,Integral i) => [a] -> [(a,i)]
histogram x =
    let g = group (sort x)
        n = map genericLength g
    in zip (map head g) n

-- > window 2 1 [1..5] == [[1,2],[2,3],[3,4],[4,5]]
-- > window 2 2 [1..5] == [[1,2],[3,4]]
window :: Int -> Int -> [a] -> [[a]]
window i j p =
    let q = take i p
        p' = drop j p
    in if length q /= i then [] else q : window i j p'

-- > intersect_l [[1,2],[1,2,3],[1,2,3,4]] == [1,2]
intersect_l :: Eq a => [[a]] -> [a]
intersect_l = foldl1 intersect

-- > sort (union_l [[1,3],[2,3],[3]]) == [1,2,3]
union_l :: Eq a => [[a]] -> [a]
union_l = foldl1 union

-- > adj_intersect [[1,2],[1,2,3],[1,2,3,4]] == [[1,2],[1,2,3]]
adj_intersect :: Eq a => [[a]] -> [[a]]
adj_intersect = map intersect_l . window 2 1

-- > cycles 3 [1..9] == [[1,4,7],[2,5,8],[3,6,9]]
cycles :: Int -> [a] -> [[a]]
cycles n = transpose . splitEvery n

-- > histogram (concat b_12_4_3) == [(1,11),(2,11),(3,11),(4,11),(5,11),(6,11),(7,11),(8,11),(9,11),(10,11),(11,11),(12,11)]
-- > histogram (map (sort.concat) (splitEvery 3 b_12_4_3)) == [([1,2,3,4,5,6,7,8,9,10,11,12],11)]
-- > map length (adj_intersect b_12_4_3) == [0,0,3,0,0,3,0,0,3,0,0,3,0,0,3,0,0,3,0,0,3,0,0,3,0,0,3,0,0,3,0,0]
-- > map (map length . adj_intersect) (cycles 3 b_12_4_3) == [[1,1,1,1,1,1,1,1,1,1],[2,2,2,2,2,2,2,2,2,2],[1,1,1,1,1,1,1,1,1,1]]
-- > map adj_intersect (cycles 3 b_12_4_3) == [[[12],[12],[12],[12],[12],[12],[12],[12],[12],[12]]
-- >                                          ,[[8,9],[7,8],[6,7],[5,6],[4,5],[3,4],[2,3],[1,2],[1,11],[10,11]]
-- >                                          ,[[3],[2],[1],[11],[10],[9],[8],[7],[6],[5]]]
b_12_4_3 :: Integral i => [[i]]
b_12_4_3 =
    [[1,5,7,12]
    ,[2,8,9,10]
    ,[3,4,6,11]
    ,[4,6,11,12]
    ,[1,7,8,9]
    ,[2,3,5,10]
    ,[3,5,10,12]
    ,[6,7,8,11]
    ,[1,2,4,9]
    ,[2,4,9,12]
    ,[5,6,7,10]
    ,[1,3,8,11]
    ,[1,3,8,12]
    ,[4,5,6,9]
    ,[2,7,10,11]
    ,[2,7,11,12]
    ,[3,4,5,8]
    ,[1,6,9,10]
    ,[1,6,10,12]
    ,[2,3,4,7]
    ,[5,8,9,11]
    ,[5,9,11,12]
    ,[1,2,3,6]
    ,[4,7,8,10]
    ,[4,8,10,12]
    ,[1,2,5,11]
    ,[3,6,7,9]
    ,[3,7,9,12]
    ,[1,4,10,11]
    ,[2,5,6,8]
    ,[2,6,8,12]
    ,[3,9,10,11]
    ,[1,4,5,7]]

-- > length n_12_4_3 == 12 && sort n_12_4_3 == n_12_4_3
n_12_4_3 :: Num i => [(i,i)]
n_12_4_3 = [(3,2),(3,5),(3,6),(3,9),(3,10),(4,1),(4,4),(4,7),(4,8),(4,11),(5,0),(5,3)]

-- Local Variables:
-- truncate-lines:t
-- End:
