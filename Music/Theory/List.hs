-- | List functions.
module Music.Theory.List where

import Data.Function {- base -}
import Data.List {- base -}
import qualified Data.List.Ordered as L {- data-ordlist -}
import Data.List.Split {- split -}
import Data.Maybe {- base -}

-- | Bracket sequence with left and right values.
--
-- > bracket ('<','>') "1,2,3" == "<1,2,3>"
bracket :: (a,a) -> [a] -> [a]
bracket (l,r) x = l : x ++ [r]

-- | Variant where brackets are sequences.
--
-- > bracket_l ("<:",":>") "1,2,3" == "<:1,2,3:>"
bracket_l :: ([a],[a]) -> [a] -> [a]
bracket_l (l,r) s = l ++ s ++ r

-- | Generic form of 'rotate_left'.
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

-- | Generic form of 'rotate_right'.
genericRotate_right :: Integral n => n -> [a] -> [a]
genericRotate_right n = reverse . genericRotate_left n . reverse

-- | Right rotation.
--
-- > rotate_right 1 [1..3] == [3,1,2]
rotate_right :: Int -> [a] -> [a]
rotate_right = genericRotate_right

-- | Rotate left by /n/ 'mod' /#p/ places.
--
-- > rotate 1 [1..3] == [2,3,1]
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

-- | Generic form of 'adj2'.
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
-- > interleave ".+-" "abc" == ".a+b-c"
-- > interleave [1..3] [] == []
interleave :: [b] -> [b] -> [b]
interleave p q =
    let u (i,j) = [i,j]
    in concatMap u (zip p q)

-- | Variant that continues with the longer input.
--
-- > interleave_continue ".+-" "abc" == ".a+b-c"
-- > interleave_continue [1..3] [] == [1..3]
-- > interleave_continue [] [1..3] == [1..3]
interleave_continue :: [a] -> [a] -> [a]
interleave_continue p q =
    case (p,q) of
      ([],_) -> q
      (_,[]) -> p
      (i:p',j:q') -> i : j : interleave_continue p' q'

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
cycles n = transpose . chunksOf n

-- * Association lists

-- | Given accesors for /key/ and /value/ collate input.
--
-- > let r = [('A',"a"),('B',"bd"),('C',"ce"),('D',"f")]
-- > in collate_on fst snd (zip "ABCBCD" "abcdef")
collate_on :: (Eq k,Ord k) => (a -> k) -> (a -> v) -> [a] -> [(k,[v])]
collate_on f g =
    let h l = case l of
                [] -> error "collate_on"
                l0:_ -> (f l0,map g l)
    in map h . groupBy ((==) `on` f) . sortBy (compare `on` f)

-- | 'collate_on' of 'fst' and 'snd'.
--
-- > collate (zip [1,2,1] "abc") == [(1,"ac"),(2,"b")]
collate :: Ord a => [(a,b)] -> [(a,[b])]
collate = collate_on fst snd

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
-- > d_dx [] == []
d_dx :: (Num a) => [a] -> [a]
d_dx l = if null l then [] else zipWith (-) (tail l) l

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

-- | Find adjacent elements of list that bound element under given
-- comparator.
--
-- > let f = find_bounds compare (adj [1..5])
-- > in map f [1,3.5,5] == [Just (1,2),Just (3,4),Nothing]
find_bounds :: (t -> s -> Ordering) -> [(t,t)] -> s -> Maybe (t,t)
find_bounds f l x =
    case l of
      (p,q):l' -> if f p x /= GT && f q x == GT
                  then Just (p,q)
                  else find_bounds f l' x
      _ -> Nothing

-- | Variant of 'drop' from right of list.
--
-- > dropRight 1 [1..9] == [1..8]
dropRight :: Int -> [a] -> [a]
dropRight n = reverse . drop n . reverse

-- | Variant of 'dropWhile' from right of list.
--
-- > dropWhileRight Data.Char.isDigit "A440" == "A"
dropWhileRight :: (a -> Bool) -> [a] -> [a]
dropWhileRight p = reverse . dropWhile p . reverse

-- | Apply /f/ at first element, and /g/ at all other elements.
--
-- > at_head negate id [1..5] == [-1,2,3,4,5]
at_head :: (a -> b) -> (a -> b) -> [a] -> [b]
at_head f g x =
    case x of
      [] -> []
      e:x' -> f e : map g x'

-- | Apply /f/ at all but last element, and /g/ at last element.
--
-- > at_last (* 2) negate [1..4] == [2,4,6,-4]
at_last :: (a -> b) -> (a -> b) -> [a] -> [b]
at_last f g x =
    case x of
      [] -> []
      [i] -> [g i]
      i:x' -> f i : at_last f g x'

-- | Separate list into an initial list and a last element tuple.
--
-- > separate_last [1..5] == ([1..4],5)
separate_last :: [a] -> ([a],a)
separate_last x =
    let e:x' = reverse x
    in (reverse x',e)

-- | Replace directly repeated elements with 'Nothing'.
--
-- > indicate_repetitions "abba" == [Just 'a',Just 'b',Nothing,Just 'a']
indicate_repetitions :: Eq a => [a] -> [Maybe a]
indicate_repetitions =
    let f l = case l of
                [] -> []
                e:l' -> Just e : map (const Nothing) l'
    in concatMap f . group

-- > adjacent_groupBy (<) [1,2,3,2,4,1,5,9] == [[1,2,3],[2,4],[1,5,9]]
adjacent_groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
adjacent_groupBy f p =
    case p of
      [] -> []
      [x] -> [[x]]
      x:y:p' -> let r = adjacent_groupBy f (y:p')
                    r0:r' = r
                in if f x y
                   then (x:r0) : r'
                   else [x] : r

-- > group_just [Just 1,Nothing,Nothing,Just 4,Just 5]
group_just :: [Maybe a] -> [[Maybe a]]
group_just = groupBy ((==) `on` isJust)

-- | Given a comparison function, merge two ascending lists.
--
-- > mergeBy compare [1,3,5] [2,4] == [1..5]
merge_by :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge_by = L.mergeBy

-- | 'mergeBy' 'compare'.
merge :: Ord a => [a] -> [a] -> [a]
merge = L.merge

-- | Merge list of sorted lists given comparison function.  Note that
-- this is not equal to 'L.mergeAll'.
merge_set_by :: (a -> a -> Ordering) -> [[a]] -> [a]
merge_set_by f = foldr (merge_by f) []

-- | 'merge_set_by' of 'compare'.
--
-- > merge_set [[1,3,5,7,9],[2,4,6,8],[10]] == [1..10]
merge_set :: Ord a => [[a]] -> [a]
merge_set = merge_set_by compare

-- | Predicate to determine if all elements of the list are '=='.
all_eq :: Eq n => [n] -> Bool
all_eq = (== 1) . length . nub

-- | 'groupBy' of 'sortBy'.
--
-- > let r = [[('1','a'),('1','c')],[('2','d')],[('3','b'),('3','e')]]
-- > in sort_group_on fst (zip "13123" "abcde") == r
sort_group_on :: Ord b => (a -> b) -> [a] -> [[a]]
sort_group_on f = groupBy ((==) `on` f) . sortBy (compare `on` f)
