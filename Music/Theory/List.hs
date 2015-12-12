-- | List functions.
module Music.Theory.List where

import Data.Function {- base -}
import Data.List {- base -}
import qualified Data.List.Ordered as O {- data-ordlist -}
import Data.List.Split {- split -}
import Data.List.Split.Internals {- split -}
import Data.Maybe {- base -}

-- | Bracket sequence with left and right values.
--
-- > bracket ('<','>') "1,2,3" == "<1,2,3>"
bracket :: (a,a) -> [a] -> [a]
bracket (l,r) x = l : x ++ [r]

unbracket' :: [a] -> (Maybe a,[a],Maybe a)
unbracket' x =
    case x of
      [] -> (Nothing,[],Nothing)
      l:x' -> let (m,r) = separate_last' x' in (Just l,m,r)

-- | The first & middle & last elements of a list.
--
-- > unbracket "[12]" == Just ('[',"12",']')
unbracket :: [t] -> Maybe (t,[t],t)
unbracket x =
    case unbracket' x of
      (Just l,m,Just r) -> Just (l,m,r)
      _ -> Nothing

unbracket_err :: [t] -> (t,[t],t)
unbracket_err = fromMaybe (error "unbracket") . unbracket

-- | Variant where brackets are sequences.
--
-- > bracket_l ("<:",":>") "1,2,3" == "<:1,2,3:>"
bracket_l :: ([a],[a]) -> [a] -> [a]
bracket_l (l,r) s = l ++ s ++ r

-- | 'Splitter' comparing single element.
on_elem :: Eq a => a -> Splitter a
on_elem e = defaultSplitter { delimiter = Delimiter [(==) e] }

-- | Split at (before) the indicated element.
--
-- > split_at 'x' "axbcxdefx" == ["a","xbc","xdef","x"]
-- > split_at 'x' "xa" == ["","xa"]
--
-- > map (flip split_at "abcde") "ae_" == [["","abcde"],["abcd","e"],["abcde"]]
-- > map (flip break "abcde" . (==)) "ae_" == [("","abcde"),("abcd","e"),("abcde","")]
split_at :: Eq a => a -> [a] -> [[a]]
split_at = split . keepDelimsL . on_elem

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

-- | Rotate list so that is starts at indicated element.
--
-- > rotate_starting_from 'c' "abcde" == Just "cdeab"
-- > rotate_starting_from '_' "abc" == Nothing
rotate_starting_from :: Eq a => a -> [a] -> Maybe [a]
rotate_starting_from x l =
    case break (== x) l of
      (_,[]) -> Nothing
      (lhs,rhs) -> Just (rhs ++ lhs)

-- | Erroring variant.
rotate_starting_from_err :: Eq a => a -> [a] -> [a]
rotate_starting_from_err x =
    fromMaybe (error "rotate_starting_from: non-element") .
    rotate_starting_from x

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
interleave :: [a] -> [a] -> [a]
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

-- | Elements that appear more than once in the input.
--
-- > map duplicates ["duplicates","redundant"] == ["","dn"]
duplicates :: Ord a => [a] -> [a]
duplicates = map fst . filter (\(_,n) -> n > (1::Integer)) . histogram

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

-- | Variant of 'filter' that has a predicate to halt processing,
-- ie. 'filter' of 'takeWhile'.
--
-- > filter_halt (even . fst) ((< 5) . snd) (zip [1..] [0..])
filter_halt :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filter_halt sel end = filter sel . takeWhile end

-- * Association lists

-- | Equivalent to 'groupBy' '==' 'on' /f/.
--
-- > let r = [[(1,'a'),(1,'b')],[(2,'c')],[(3,'d'),(3,'e')],[(4,'f')]]
-- > in group_on fst (zip [1,1,2,3,3,4] "abcdef") == r
group_on :: Eq x => (a -> x) -> [a] -> [[a]]
group_on f = map (map snd) . groupBy ((==) `on` fst) . map (\x -> (f x,x))

-- | Given accesors for /key/ and /value/ collate input.
--
-- > let r = [('A',"a"),('B',"bd"),('C',"ce"),('D',"f")]
-- > in collate_on fst snd (zip "ABCBCD" "abcdef") == r
collate_on :: (Eq k,Ord k) => (a -> k) -> (a -> v) -> [a] -> [(k,[v])]
collate_on f g =
    let h l = case l of
                [] -> error "collate_on"
                l0:_ -> (f l0,map g l)
    in map h . group_on f . sortOn f

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

-- | Variant that takes initial value and separates final value.  This
-- is an appropriate function for 'mapAccumL'.
--
-- > dx_d' 5 [1,2,3] == (11,[5,6,8])
-- > dx_d' 0 [1,1,1] == (3,[0,1,2])
dx_d' :: Num t => t -> [t] -> (t,[t])
dx_d' n l =
    case reverse (scanl (+) n l) of
      e:r -> (e,reverse r)
      _ -> error "dx_d'"

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

-- | Basis of 'find_bounds_scl', indicates if /x/ is to the left or
-- right of the list, and it to the right whether equal or not.
-- 'Right' values will be correct is the list is not ascending,
-- however 'Left' values only make sense for ascending ranges.
--
-- > map (find_bounds' compare [(0,1),(1,2)]) [-1,0,1,2,3]
find_bounds' :: (t -> s -> Ordering) -> [(t,t)] -> s -> Either ((t,t),Ordering) (t,t)
find_bounds' f l x =
    let g (p,q) = f p x /= GT && f q x == GT
    in case l of
         [] -> error "find_bounds': nil"
         [(p,q)] -> if g (p,q) then Right (p,q) else Left ((p,q),f q x)
         (p,q):l' -> if f p x == GT
                     then Left ((p,q),GT)
                     else if g (p,q) then Right (p,q) else find_bounds' f l' x

decide_nearest' :: Ord o => (p -> o) -> (p,p) -> p
decide_nearest' f (p,q) = if f p < f q then p else q

-- | Decide if value is nearer the left or right value of a range.
decide_nearest :: (Num o,Ord o) => o -> (o, o) -> o
decide_nearest x = decide_nearest' (abs . (x -))

-- | Find the number that is nearest the requested value in an
-- ascending list of numbers.
--
-- > map (find_nearest_err [0,3.5,4,7]) [-1,1,3,5,7,9] == [0,0,3.5,4,7,7]
find_nearest_err :: (Num n,Ord n) => [n] -> n -> n
find_nearest_err l x =
    case find_bounds' compare (adj2 1 l) x of
      Left ((p,_),GT) -> p
      Left ((_,q),_) -> q
      Right (p,q) -> decide_nearest x (p,q)

find_nearest :: (Num n,Ord n) => [n] -> n -> Maybe n
find_nearest l x = if null l then Nothing else Just (find_nearest_err l x)

-- | Basis of 'find_bounds'.  There is an option to consider the last
-- element specially, and if equal to the last span is given.
find_bounds_scl :: Bool -> (t -> s -> Ordering) -> [(t,t)] -> s -> Maybe (t,t)
find_bounds_scl scl f l x =
    case find_bounds' f l x of
         Right r -> Just r
         Left (r,EQ) -> if scl then Just r else Nothing
         _ -> Nothing

-- | Find adjacent elements of list that bound element under given
-- comparator.
--
-- > let {f = find_bounds True compare [1..5]
-- >     ;r = [Nothing,Just (1,2),Just (3,4),Just (4,5)]}
-- > in map f [0,1,3.5,5] == r
find_bounds :: Bool -> (t -> s -> Ordering) -> [t] -> s -> Maybe (t,t)
find_bounds scl f l = find_bounds_scl scl f (adj2 1 l)

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

-- | Separate list into an initial list and perhaps the last element tuple.
--
-- > separate_last' [] == ([],Nothing)
separate_last' :: [a] -> ([a],Maybe a)
separate_last' x =
    case reverse x of
      [] -> ([],Nothing)
      e:x' -> (reverse x',Just e)

-- | Error on null input.
--
-- > separate_last [1..5] == ([1..4],5)
separate_last :: [a] -> ([a],a)
separate_last = fmap (fromMaybe (error "separate_last")) . separate_last'

-- | Replace directly repeated elements with 'Nothing'.
--
-- > indicate_repetitions "abba" == [Just 'a',Just 'b',Nothing,Just 'a']
indicate_repetitions :: Eq a => [a] -> [Maybe a]
indicate_repetitions =
    let f l = case l of
                [] -> []
                e:l' -> Just e : map (const Nothing) l'
    in concatMap f . group

-- | 'Data.List.groupBy' does not make adjacent comparisons, it
-- compares each new element to the start of the group.  This function
-- is the adjacent variant.
--
-- > groupBy (<) [1,2,3,2,4,1,5,9] == [[1,2,3,2,4],[1,5,9]]
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

-- | 'groupBy' on /structure/ of 'Maybe', ie. all 'Just' compare equal.
--
-- > let r = [[Just 1],[Nothing,Nothing],[Just 4,Just 5]]
-- > in group_just [Just 1,Nothing,Nothing,Just 4,Just 5] == r
group_just :: [Maybe a] -> [[Maybe a]]
group_just = group_on isJust

-- | Predicate to determine if all elements of the list are '=='.
all_eq :: Eq n => [n] -> Bool
all_eq = (== 1) . length . nub

-- | 'group_on' of 'sortOn'.
--
-- > let r = [[('1','a'),('1','c')],[('2','d')],[('3','b'),('3','e')]]
-- > in sort_group_on fst (zip "13123" "abcde") == r
sort_group_on :: Ord b => (a -> b) -> [a] -> [[a]]
sort_group_on f = group_on f . sortOn f

-- | Maybe cons element onto list.
--
-- > Nothing `mcons` "something" == "something"
-- > Just 's' `mcons` "omething" == "something"
mcons :: Maybe a -> [a] -> [a]
mcons e l = maybe l (:l) e

-- * Ordering

-- | Comparison function type.
type Compare_F a = a -> a -> Ordering

-- | If /f/ compares 'EQ', defer to /g/.
two_stage_compare :: Compare_F a -> Compare_F a -> Compare_F a
two_stage_compare f g p q =
    case f p q of
      EQ -> g p q
      r -> r

-- | Sequence of comparison functions, continue comparing until not EQ.
--
-- > compare (1,0) (0,1) == GT
-- > n_stage_compare [compare `on` snd,compare `on` fst] (1,0) (0,1) == LT
n_stage_compare :: [Compare_F a] -> Compare_F a
n_stage_compare l p q =
    case l of
      [] -> EQ
      f:l' -> case f p q of
                EQ -> n_stage_compare l' p q
                r -> r

-- | Invert 'Ordering'.
ordering_invert :: Ordering -> Ordering
ordering_invert o =
    case o of
      LT -> GT
      EQ -> EQ
      GT -> LT

-- | Sort sequence /a/ based on ordering of sequence /b/.
--
-- > sort_to "abc" [1,3,2] == "acb"
-- > sort_to "adbce" [1,4,2,3,5] == "abcde"
sort_to :: Ord i => [e] -> [i] -> [e]
sort_to e = map fst . sortOn snd . zip e

-- | 'flip' of 'sort_to'.
--
-- > sort_on [1,4,2,3,5] "adbce" == "abcde"
sort_on :: Ord i => [i] -> [e] -> [e]
sort_on = flip sort_to

-- | 'sortBy' of 'two_stage_compare'.
sort_by_two_stage :: (Ord b,Ord c) => (a -> b) -> (a -> c) -> [a] -> [a]
sort_by_two_stage f g = sortBy (two_stage_compare (compare `on` f) (compare `on` g))

-- | 'sortBy' of 'n_stage_compare'.
sort_by_n_stage :: Ord b => [a -> b] -> [a] -> [a]
sort_by_n_stage f = sortBy (n_stage_compare (map (compare `on`) f))

-- | Given a comparison function, merge two ascending lists.
--
-- > mergeBy compare [1,3,5] [2,4] == [1..5]
merge_by :: Compare_F a -> [a] -> [a] -> [a]
merge_by = O.mergeBy

-- | 'merge_by' 'compare' 'on'.
merge_on :: Ord x => (a -> x) -> [a] -> [a] -> [a]
merge_on f = merge_by (compare `on` f)

-- | 'O.mergeBy' of 'two_stage_compare'.
merge_by_two_stage :: Ord b => (a -> b) -> Compare_F c -> (a -> c) -> [a] -> [a] -> [a]
merge_by_two_stage f cmp g = O.mergeBy (two_stage_compare (compare `on` f) (cmp `on` g))

-- | 'mergeBy' 'compare'.
merge :: Ord a => [a] -> [a] -> [a]
merge = O.merge

-- | Merge list of sorted lists given comparison function.  Note that
-- this is not equal to 'O.mergeAll'.
merge_set_by :: (a -> a -> Ordering) -> [[a]] -> [a]
merge_set_by f = foldr (merge_by f) []

-- | 'merge_set_by' of 'compare'.
--
-- > merge_set [[1,3,5,7,9],[2,4,6,8],[10]] == [1..10]
merge_set :: Ord a => [[a]] -> [a]
merge_set = merge_set_by compare

{-| 'merge_by' variant that joins (resolves) equal elements.

> let {left p _ = p
>     ;right _ q = q
>     ;cmp = compare `on` fst
>     ;p = zip [1,3,5] "abc"
>     ;q = zip [1,2,3] "ABC"
>     ;left_r = [(1,'a'),(2,'B'),(3,'b'),(5,'c')]
>     ;right_r = [(1,'A'),(2,'B'),(3,'C'),(5,'c')]}
> in merge_by_resolve left cmp p q == left_r &&
>    merge_by_resolve right cmp p q == right_r

-}
merge_by_resolve :: (a -> a -> a) -> Compare_F a -> [a] -> [a] -> [a]
merge_by_resolve jn cmp =
    let recur p q =
            case (p,q) of
              ([],_) -> q
              (_,[]) -> p
              (l:p',r:q') -> case cmp l r of
                               LT -> l : recur p' q
                               EQ -> jn l r : recur p' q'
                               GT -> r : recur p q'
    in recur

-- | First non-ascending pair of elements.
find_non_ascending :: (a -> a -> Ordering) -> [a] -> Maybe (a,a)
find_non_ascending cmp xs =
    case xs of
      p:q:xs' -> if cmp p q == GT then Just (p,q) else find_non_ascending cmp (q:xs')
      _ -> Nothing

-- | 'isNothing' of 'find_non_ascending'.
is_ascending_by :: (a -> a -> Ordering) -> [a] -> Bool
is_ascending_by cmp = isNothing . find_non_ascending cmp

-- | 'is_ascending_by' 'compare'.
is_ascending :: Ord a => [a] -> Bool
is_ascending = is_ascending_by compare

-- | A 'zipWith' variant that always consumes an element from the left
-- hand side (lhs), but only consumes an element from the right hand
-- side (rhs) if the zip function is 'Right' and not if 'Left'.
-- There's also a secondary function to continue if the rhs ends
-- before the lhs.
zip_with_perhaps_rhs :: (a -> b -> Either c c) -> (a -> c) -> [a] -> [b] -> [c]
zip_with_perhaps_rhs f g lhs rhs =
    case (lhs,rhs) of
      ([],_) -> []
      (_,[]) -> map g lhs
      (p:lhs',q:rhs') -> case f p q of
                           Left r -> r : zip_with_perhaps_rhs f g lhs' rhs
                           Right r -> r : zip_with_perhaps_rhs f g lhs' rhs'

-- | Fill gaps in a sorted association list, range is inclusive at both ends.
--
-- > let r = [(1,'a'),(2,'x'),(3,'x'),(4,'x'),(5,'b'),(6,'x'),(7,'c'),(8,'x'),(9,'x')]
-- > in fill_gaps_ascending' 'x' (1,9) (zip [1,5,7] "abc") == r
fill_gaps_ascending :: (Enum n, Ord n) => t -> (n,n) -> [(n,t)] -> [(n,t)]
fill_gaps_ascending def_e (l,r) =
    let f i (j,e) = if j > i then Left (i,def_e) else Right (j,e)
        g i = (i,def_e)
    in zip_with_perhaps_rhs f g [l .. r]

-- | Direct definition.
fill_gaps_ascending' :: (Num n,Enum n, Ord n) => t -> (n,n) -> [(n,t)] -> [(n,t)]
fill_gaps_ascending' def (l,r) =
    let recur n x =
            if n > r
            then []
            else case x of
                   [] -> zip [n .. r] (repeat def)
                   (m,e):x' -> if n < m
                               then (n,def) : recur (n + 1) x
                               else (m,e) : recur (n + 1) x'
    in recur l

-- | 'minimum' and 'maximum' in one pass.
--
-- > minmax "minimumandmaximum" == ('a','x')
minmax :: Ord t => [t] -> (t,t)
minmax inp =
    case inp of
      [] -> error "minmax: null"
      x:xs -> let mm p (l,r) = (min p l,max p r) in foldr mm (x,x) xs

-- * Bimap

-- | Apply /f/ to both elements of a two-tuple, ie. 'bimap' /f/ /f/.
bimap1 :: (t -> u) -> (t,t) -> (u,u)
bimap1 f (p,q) = (f p,f q)

-- * Error variants

lookup_err :: Eq a => a -> [(a,x)] -> x
lookup_err n = fromMaybe (error "lookup") . lookup n
