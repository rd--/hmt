-- | Notation of a sequence of 'RQ' values as annotated 'Duration' values.
module Music.Theory.Duration.Sequence.Notate (notate,simplify,ascribe
                                             ,to_measures) where

import Data.Maybe
import Data.Ratio
import Music.Theory.Duration
import Music.Theory.Duration.Annotation
import Music.Theory.Duration.RQ

-- * Debug

{-
import Debug.Trace
debug :: (Show a) => a -> x -> x
debug = traceShow
-}

-- 1. annotate all RQ as untied
-- 2. separate into measures, adding tie annotations as required.
-- 3. separate each measure
-- 4. simplify each measure

data Tied = T | F deriving (Eq,Show)
type RQ_T = (RQ,Tied)
type RQ_S = [RQ_T]

-- > untied 3 == (3,False)
untied :: RQ -> RQ_T
untied n = (n,F)

tie_last :: [RQ] -> [RQ_T]
tie_last x =
    case x of
      [] -> []
      [i] -> [(i,T)]
      i:x' -> (i,F) : tie_last x'

-- > take_rq_set 3 [2,2,2] == Just ([(2,F),(1,T)],[1,2])
take_rq_set :: RQ -> [RQ] -> Maybe (RQ_S, [RQ])
take_rq_set i x =
    let f (m,x',t) = let m' = if isJust t then tie_last m else map untied m
                     in (m',x')
    in fmap f (split_sum i x)

-- > to_rq_sets [3,3] [2,2,2] == Just [[(2,F),(1,T)],[(1,F),(2,F)]]
-- > to_rq_sets [3,3] [6] == Just [[(3,T)],[(3,F)]]
-- > to_rq_sets [1,1,1] [3] == Just [[(1,T)],[(1,T)],[(1,F)]]
-- > to_rq_sets [3,3] [2,2,1] == Nothing
-- > to_rq_sets [3,2] [2,2,2] == Nothing
to_rq_sets :: [RQ] -> [RQ] -> Maybe [RQ_S]
to_rq_sets m x =
    case (m,x) of
      ([],[]) -> Just []
      ([],_) -> Nothing
      (i:m',_) -> case take_rq_set i x of
                    Just (r,x') -> fmap ((:) r) (to_rq_sets m' x')
                    Nothing -> Nothing

-- > split_sum_t 5 [(3,F),(2,T),(1,F)] == Just ([(3,F),(2,T)],[(1,F)])
-- > split_sum_t 4 [(3,F),(2,T),(1,F)] == Just ([(3,F),(1,T)],[(1,T),(1,F)])
split_sum_t :: RQ -> RQ_S -> Maybe (RQ_S,RQ_S)
split_sum_t d x =
    case split_sum d (map fst x) of
      Just (i,_,k) ->
          case k of
            Nothing -> Just (splitAt (length i) x)
            Just (p,q) -> let (s,(_,z):t) = splitAt (length i - 1) x
                          in Just (s ++ [(p,T)]
                                  ,((q,z) : t))
      Nothing -> Nothing

-- > to_rqt_sets [3,3] [(2,F),(2,F),(2,F)]
to_rqt_sets :: [RQ] -> [RQ_T] -> Maybe [RQ_T]
to_rqt_sets m x =
    case (m,x) of
      ([],[]) -> Just []
      ([],_) -> Nothing
      (i:m',_) -> case split_sum_t i x of
                    Just (r,x') -> fmap (r ++) (to_rqt_sets m' x')
                    Nothing -> Nothing

-- > all_just (map Just [1..3]) == Just [1..3]
-- > all_just [Just 1,Nothing] == Nothing
all_just :: [Maybe a] -> Maybe [a]
all_just x =
    case x of
      [] -> Just []
      Just i:x' -> fmap (i :) (all_just x')
      Nothing:_ -> Nothing

-- > to_measures [[1,1,1],[1,1,1]] [2,2,2] == Just [[(1,T),(1,F),(1,T)]
-- >                                               ,[(1,F),(1,T),(1,F)]]
to_measures :: [[RQ]] -> [RQ] -> Maybe [[RQ_T]]
to_measures m x =
    let m' = map sum m
    in case to_rq_sets m' x of
         Just y -> all_just (zipWith to_rqt_sets m y)
         Nothing -> Nothing

-- | No-op debug placeholder.
debug :: (Show a) => a -> x -> x
debug _ x = x

-- * List processing

-- | /dx/ to /d/, ie. @scanl1 (+)@.
--
-- > integrate [1,2,3,4] == [1,3,6,10]
integrate :: (Num a) => [a] -> [a]
integrate = scanl1 (+)

-- | Given an initial start time and a list of durations make set of
-- /start-time/ and /duration/ pairs.
--
-- > with_start_times 0 [4,3,5,2,1] == [(0,4),(4,3),(7,5),(12,2),(14,1)]
with_start_times :: (Num a) => a -> [a] -> [(a,a)]
with_start_times i xs =
    let is = map (+i) (0 : integrate xs)
    in zip is xs

-- | Variant of 'with_start_times' processing sets of durations.
--
-- > with_start_times_sets 0 [[4,3],[2,1]] == [[(0,4),(4,3)],[(7,2),(9,1)]]
-- > last (with_start_times_sets 0 [[4,3,5],[2,1],[6,3]]) == [(15,6),(21,3)]
with_start_times_sets :: (Num a) => a -> [[a]] -> [[(a, a)]]
with_start_times_sets i xs =
    let is = i : integrate (map sum xs)
    in zipWith with_start_times is xs

-- | Split sequence such that the initial part sums to precisely /n/.
-- The third element of the result indicates if it was required to
-- divide an element.  If the required sum is non positive, or the
-- input list does not sum to at least the required sum, gives
-- nothing.
--
-- > split_sum 5 [2,3,1] == Just ([2,3],[1],Nothing)
-- > split_sum 5 [2,1,3] == Just ([2,1,2],[1],Just (2,1))
-- > split_sum 2 [3%2,3%2,3%2] == Just ([3%2,1%2],[1%1,3%2],Just (1%2,1))
-- > split_sum 6 [1..10] == Just ([1..3],[4..10],Nothing)
-- > fmap (\(a,_,c)->(a,c)) (split_sum 5 [1..]) == Just ([1,2,2],Just (2,1))
-- > split_sum 0 [1..] == Nothing
-- > split_sum 3 [1,1] == Nothing
split_sum :: (Ord a, Num a) => a -> [a] -> Maybe ([a],[a],Maybe (a,a))
split_sum d l =
    let jn a (a',b,c) = (a:a',b,c)
    in if d <= 0
       then Nothing
       else case l of
              [] -> Nothing
              x:xs -> case compare d x of
                        EQ -> Just ([d],xs,Nothing)
                        LT -> Just ([d],(x-d):xs,Just (d,x-d))
                        GT -> fmap (jn x) (split_sum (d - x) xs)

-- | Error variant of 'split_sum'.
split_sum_err :: (Ord a, Num a) => a -> [a] -> ([a],[a],Maybe (a,a))
split_sum_err n = fromMaybe (error "split_sum") . split_sum n

{-
-- | Variant of 'cycle' with counter, or 'concat' '.' 'replicate'.
--
-- > cycle_n 3 [1,5] == [1,5,1,5,1,5]
-- > concat (replicate 3 [1,5]) == cycle_n 3 [1,5]
cycle_n :: Int -> [a] -> [a]
cycle_n n = concat . replicate n

-- | Given sequences of /boundaries/ and /durations/, sub-divide the
-- durations as required to not cross the boundaries.  It is an error
-- for the sum of the boundaries to be less than the sum of the
-- durations.
--
-- > boundaries (replicate 5 3) [1..5] == Just [[1],[2],[3],[3,1],[2,3]]
-- > boundaries (replicate 3 3) [4,3,2] == Just [[3,1],[2,1],[2]]
-- > boundaries [3,2,3] [3,2,3] == Just [[3],[2],[3]]
-- > boundaries (cycle_n 3 [3,2]) [1..5] == Just [[1],[2],[2,1],[2,2],[3,2]]
--
-- > let b = replicate 5 (3%2)
-- > in boundaries b [1%2,1..5%2] == Just [[1%2],[1%1],[3%2],[3%2,1%2],[1%1,3%2]]
--
-- > boundaries [] [1] == Nothing
-- > boundaries [1] [] == Nothing
boundaries :: (Num a, Ord a) => [a] -> [a] -> Maybe [[a]]
boundaries =
    let go [] [] = Just []
        go _ [] = Nothing
        go [] _ = Nothing
        go bs (d:ds) =
            case split_sum d bs of
              Just (d',bs',_) -> fmap (d' :) (go bs' ds)
              _ -> Nothing
    in go

boundaries_check :: Num a => [a] -> [a] -> Bool
boundaries_check bs ds = sum bs == sum ds
-}

boundaries_err :: (Num a, Ord a) => [a] -> [a] -> [[a]]
boundaries_err =
    let go [] [] = []
        go _ [] = error "boundaries: no durations"
        go [] _ = error "boundaries: no boundaries"
        go bs (d:ds) =
            let (d',bs',_) = split_sum_err d bs
            in d' : go bs' ds
    in go

-- | Split list into first, possibly empty 'middle', and last parts.
-- The list must have at least two elements.
--
-- > start_middle_end [1,2] == Just (1,[],2)
-- > start_middle_end "abc" == Just ('a',"b",'c')
-- > start_middle_end [1..6] == Just (1,[2..5],6)
-- > map start_middle_end [[],[1]] == [Nothing,Nothing]
start_middle_end :: [x] -> Maybe (x,[x],x)
start_middle_end xs =
    case xs of
      _:_:_ -> let n = length xs
                   x0 = xs !! 0
                   xn = xs !! (n - 1)
               in Just (x0,take (n - 2) (drop 1 xs),xn)
      _ -> Nothing

-- > start_middle_end_err [1] == undefined
start_middle_end_err :: [x] -> (x,[x],x)
start_middle_end_err =
    let m = "start_middle_end: list must have at least two elements"
    in fromMaybe (error m) . start_middle_end

{-
-- | Given a psuedo-enumeration function pair values with running sum.
--
-- > with_sum id [1..5] == [(0,1),(1,2),(3,3),(6,4),(10,5)]
-- > with_sum fromEnum "abc" == [(0,'a'),(97,'b'),(195,'c')]
with_sum :: (Num i) => (a -> i) -> [a] -> [(i,a)]
with_sum f a = zip (0 : integrate (map f a)) a

-- | Variant of 'span' where the boundary function sums the left
-- element with the given enumeration of the right element.  Intended
-- for grouping /(start-time,duration)/ pairs.
--
-- > let sd = with_start_times 0 [1,2,3]
-- > in to_boundary id 3 sd == Just ([(0,1),(1,2)],[(3,3)])
--
-- > to_boundary id 3 [(0,4)] == Nothing
to_boundary :: (Num i,Ord i) => (a->i)->i->[(i,a)]->Maybe ([(i,a)],[(i,a)])
to_boundary f b =
    let g (i,j) = if null i then Nothing else Just (i,j)
    in g . span (\(i,j) -> i + f j <= b)

to_boundary_err :: (Num i,Ord i) => (a->i)->i->[(i,a)]->([(i,a)],[(i,a)])
to_boundary_err f b = fromMaybe (error "to_boundary") . to_boundary f b
-}

-- | Applies a /join/ function to the first two elements of the list.
-- If the /join/ function succeeds the joined element is considered
-- for further coalescing.
--
-- > coalesce (\p q -> Just (p + q)) [1..5] == [15]
--
-- > let jn p q = if even p then Just (p + q) else Nothing
-- > in coalesce jn [1..5] == [1,5,9]
coalesce :: (a -> a -> Maybe a) -> [a] -> [a]
coalesce f xs =
    case xs of
      (x1:x2:xs') ->
          case f x1 x2 of
            Nothing -> x1 : coalesce f (x2:xs')
            Just x' -> coalesce f (x':xs')
      _ -> xs

-- | Variant of 'zip' that retains elements of the right hand list
-- where elements of the left hand list meet the given predicate.
--
-- > zip_hold even [1..5] "abc" == [(1,'a'),(2,'b'),(3,'b'),(4,'c'),(5,'c')]
-- > zip_hold odd [1..5] "abc" == [(1,'a'),(2,'a'),(3,'b'),(4,'b'),(5,'c')]
zip_hold :: (x -> Bool) -> [x] -> [a] -> [(x,a)]
zip_hold fn =
    let go [] _ = []
        go _ [] = error "zip_hold"
        go (x:xs) (i:is) = let is' = if fn x then i:is else is
                           in (x,i) : go xs is'
    in go

{-
let d = boundaries_err [3,3,3,3,3] [4,3,5,2,1]
in with_start_times_sets 0 d == [[(0,3),(3,1)],[(4,2),(6,1)]
                                ,[(7,2),(9,3)],[(12,2)],[(14,1)]]

let xs = [3%4,2%1,5%4,9%4,1%4,3%2,1%2,7%4,1%1,5%2,11%4,3%2]
in with_start_times 0 xs == [(0,3/4),(3/4,2),(11/4,5/4)
                            ,(4,9/4),(25/4,1/4),(13/2,3/2)
                            ,(8,1/2),(17/2,7/4),(41/4,1)
                            ,(45/4,5/2),(55/4,11/4),(33/2,3/2)]

let {xs = [3%4,2%1,5%4,9%4,1%4,3%2,1%2,7%4,1%1,5%2,11%4,3%2]
    ;d = boundaries_err (replicate 12 (3%2)) xs}
in with_start_times_sets 0 d == [[(0,3/4)]
                                ,[(3/4,3/4),(3/2,5/4)]
                                ,[(11/4,1/4),(3,1)]
                                ,[(4,1/2),(9/2,3/2),(6,1/4)]
                                ,[(25/4,1/4)]
                                ,[(13/2,1),(15/2,1/2)]
                                ,[(8,1/2)]
                                ,[(17/2,1/2),(9,5/4)]
                                ,[(41/4,1/4),(21/2,3/4)]
                                ,[(45/4,3/4),(12,3/2),(27/2,1/4)]
                                ,[(55/4,5/4),(15,3/2)]
                                ,[(33/2,3/2)]]

-}

-- * D

-- | Tuple of /start-time/, /duration/, /tied-left/ and /tied-right/.
type D = (RQ,RQ,Bool,Bool)

-- | Duration of 'D'.
d_duration :: D -> RQ
d_duration (_,x,_,_) = x

-- | Given a set of /(start-time,duration)/ make a set of tied 'D'.
--
-- > let d = [(0,1,False,True),(1,1,True,True),(2,1,True,False)]
-- > in tied_r_to_d (with_start_times 0 [1,1,1]) == d
tied_r_to_d :: [(RQ,RQ)] -> [D]
tied_r_to_d xs =
    case xs of
      [] -> []
      [(s,d)] -> [(s,d,False,False)]
      _ -> let ((s0,d0),xs',(sn,dn)) = start_middle_end_err xs
               f (s,d) = (s,d,True,True)
            in (s0,d0,False,True) : map f xs' ++ [(sn,dn,True,False)]

-- | Composition of 'boundaries', 'with_start_times_sets' and
-- 'tied_r_to_d' such that the resulting 'D' ties each sub-divided
-- duration.
--
-- > boundaries_d (replicate 8 3) [4,3,5,2,1,7,2]
boundaries_d :: [RQ] -> [RQ] -> [D]
boundaries_d xs ds =
    let bs = boundaries_err xs ds
    in concatMap tied_r_to_d (with_start_times_sets 0 bs)

{-
-- n = boundary
-- i = phase
sep_at :: RQ -> RQ -> R -> [D]
sep_at =
    let go l n i x =
            let i' = n - (i `rq_mod` n)
            in if x > i'
               then let d = (i,i',l,True)
                    in d : go True n (i + i') (x - i')
               else [(i,x,l,False)]
    in go False

sep_at 1 (1%2) 1
sep_at 1 (1%3) (6%3)
-}

-- | Given /phase/ separate an /RQ/ duration if un-representable by a
-- single /cmn/ duration (ie. requires tie).
--
-- > map (sep_unrep 0) [3,4,5] == [Nothing,Nothing,Just (4,1)]
--
-- > let {ph = [1,3%8,1]; d = [5%4,5%8,4]}
-- > in zipWith sep_unrep ph d == [Just (1%1,1%4),Just (1%8,1%2),Nothing]
sep_unrep :: RQ -> RQ -> Maybe (RQ,RQ)
sep_unrep i x =
    let i' = denominator i == 1
        j = case numerator x of
              5 -> Just (1,4)
              7 -> Just (3,4)
              _ -> Nothing
        f (n,m) = (n%denominator x,m%denominator x)
        swap (a,b) = (b,a)
    in case j of
         Nothing -> Nothing
         Just j' -> Just (f (if i' then swap j' else j'))

-- | Variant of 'sep_unrep' processing 'D'.  If separated the elements
-- are tied.  Instead of a 'Maybe' pair the result is a list of either
-- one or two elements.
--
-- > let {ph = [1,3%8,1]
-- >     ;d = [5%4,5%8,4]
-- >     ;f i x = sep_unrep_d (i,x,False,False)}
-- > in zipWith f ph d == [[(1,1,False,True),(2,1/4,True,False)]
-- >                      ,[(3/8,1/8,False,True),(1/2,1/2,True,False)]
-- >                      ,[(1,4,False,False)]]
sep_unrep_d :: D -> [D]
sep_unrep_d d =
    let (i,x,l,r) = d
    in case sep_unrep i x of
         Nothing -> [d]
         Just (x0,x1) -> [(i,x0,l,True),(i+x0,x1,True,r)]

-- | Composition of 'boundaries_d' and 'sep_unrep_d'.
--
-- > separate [3,3] [2,2,2]
--
-- > let xs = [3%4,2%1,5%4,9%4,1%4,3%2,1%2,7%4,1%1,5%2,11%4,3%2]
-- > in separate (replicate 36 (1%2)) xs
separate :: [RQ] -> [RQ] -> [D]
separate ns = concatMap sep_unrep_d . boundaries_d ns

-- | group to n, or to multiple of
--
-- > group_boundary_lenient id [1,1,1] [2,1%2,1%2] == [[2%1],[1%2,1%2]]
-- > group_boundary_lenient id [1,1,1] [3/2,1,1]
-- > group_boundary_lenient id [3,3,3] (cycle [1,2,3]) == [[1,2],[3],[1,2]]
group_boundary_lenient :: (a -> RQ) -> [RQ] -> [a] -> [[a]]
group_boundary_lenient dur_f =
    let go _ [] [] _ = []
        go _ _ [] _ = error "group_boundary_lenient: no boundaries?"
        go _ _ _ [] = error "group_boundary_lenient: no durations?"
        --go _ js _ [] = [reverse js]
        go _ js _ [x] = [reverse (x:js)]
        go c js (n:ns) (x:xs) =
            let c' = c + dur_f x
            in case compare c' n of
                 EQ -> reverse (x:js) : go 0 [] ns xs
                 LT -> go c' (x:js) (n:ns) xs
                 GT -> let c'' = c' - n
                       in if c'' `rq_divisible_by` n
                          then reverse (x:js) : go 0 [] ns xs
                          else go c'' (x:js) ns xs
    in go 0 []

group_boundary_lenient_d :: [RQ] -> [D] -> [[D]]
group_boundary_lenient_d = group_boundary_lenient d_duration

{-
let {i = [1,1%2,2,1%3,5%3,1%8,1%2,7%8]
    ;b = replicate 7 1
    ;j = separate b i
    ;k = group_boundary_lenient_d b j}
in (map (map d_duration) k)
-}

{-
-- | Keeps the /zero/ duration chord element in the same measure.
--
-- > group_boundary_strict' id [2,1] [2,0,1] == [[(0,2),(2,0)],[(2,1)]]
group_boundary_strict' :: (Ord i,Num i) => (a->i) -> [i] -> [a] -> [[(i,a)]]
group_boundary_strict' f bs is =
    let is' = with_sum f is
        bs' = integrate bs
        go [] _ = []
        go (j:js) zs = let (x,y) = to_boundary_err f j zs
                       in x : go js y
    in go bs' is'

-- | Variant on 'group_boundary_lenient'.
--
-- > group_boundary_strict id [3,3,3] (cycle [1,2,3]) == [[1,2],[3],[1,2]]
--
-- > let g = group_boundary_strict id
-- > in g [3,2,3] [1,0,1,1,0,2,0,1,1,1] == [[1,0,1,1,0],[2,0],[1,1,1]]
--
-- > let g = group_boundary_strict id (replicate 3 3)
-- > in g (cycle [1,2,3]) == [[1,2],[3],[1,2]]
--
-- > let g = group_boundary_strict id (replicate 3 3)
-- > in g [1,0,1,1,0,2,0,1,1,1,1] == [[1,0,1,1,0],[2,0,1],[1,1,1]]
group_boundary_strict :: (Ord a, Num a) => (b -> a) -> [a] -> [b] -> [[b]]
group_boundary_strict f bs = map (map snd) . group_boundary_strict' f bs


-- > group_measures id [3,3] [2,2,2] == undefined
group_measures :: (Ord a, Num a) => (b -> a) -> [a] -> [b] -> [[b]]
group_measures = group_boundary_strict

group_boundary_strict_d :: [RQ] -> [D] -> [[D]]
group_boundary_strict_d = group_boundary_strict d_duration
-}

derive_tuplet :: [D] -> Maybe (Integer,Integer)
derive_tuplet xs =
    let xs' = map d_duration xs
        i = maximum (map denominator xs')
        smpl n = if even n then smpl (n `div` 2) else n
        i' = smpl i
        j = case i' of
              3 -> (3,2)
              5 -> (5,4)
              7 -> (7,4)
              9 -> (9,8)
              _ -> error ("derive_tuplet: " ++ show (i,i'))
    in if i' == 1
       then Nothing
       else Just j

{-
let {i = [1,1%2,2,1%3,5%3,1%8,1%2,7%8]
    ;b = replicate 7 1
    ;j = separate b i
    ;k = group_boundary_strict_d b j}
in (map (map d_duration) k,map derive_tuplet k)
-}

-- | Remove tuplet multiplier from value, ie. to give notated
-- duration.  This seems odd but is neccessary to avoid ambiguity.
-- Ie. is 1 a quarter note or a 3:2 tuplet dotted-quarter-note etc.
--
-- > map (un_tuplet (1,3)) [1,1%2,1%3] == [1 % 3,1 % 6,1 % 9]
un_tuplet :: (Integer,Integer) -> RQ -> RQ
un_tuplet (i,j) x = x * (i%j)

d_join_aligned :: D -> D -> Maybe D
d_join_aligned (s1,x1,l1,r1) (_,x2,_,r2)
    | (x1 == (1%4) && r1 && x2 `elem` [1%4,1%2,3%4]) ||
      (x1 == (1%2) && r1 && x2 `elem` [1%4,1%2,1,3%2]) ||
      (x1 == 1 && r1 && x2 `elem` [1%2,1,2]) ||
      (x1 == (3%2) && r1 && x2 `elem` [1%2,3%2]) ||
      (x1 == 2 && r1 && x2 `elem` [1,2]) = debug ("aligned-join",s1,x1,x2) (Just (s1,x1+x2,l1,r2))
    | otherwise = debug ("aligned-no-join",s1,x1,r1,x2) Nothing

-- | partial/incomplete/inaccurate
--
-- > d_join 1 (7%4,1%4,False,True) (2%1,1%4,True,False) == Nothing
d_join :: RQ -> D -> D -> Maybe D
d_join a (s1,x1,l1,r1) (s2,x2,l2,r2)
    | s1 `rq_divisible_by` a = d_join_aligned (s1,x1,l1,r1) (s2,x2,l2,r2)
    | denominator (s1 `rq_mod` 1) == 4 &&
      x1 == 1%4 &&
      r1 &&
      x2 == 1%4 &&
      not (s2 `rq_divisible_by` a) =
      debug ("non-aligned-join",a,s1,x1) (Just (s1,x1+x2,l1,r2))
    | s1 `rq_mod` 1 == 2%3 &&
      x1 == 1%3 &&
      r1 &&
      x2 == 1%3 =
      debug ("non-aligned-join",a,s1,x1) (Just (s1,x1+x2,l1,r2))
    | otherwise = debug ("non-aligned-no-join",a,s1,x1) Nothing

{-
-- error checking variant
d_join' :: RQ -> D -> D -> Maybe D
d_join' a d1 d2 =
    case d_join a d1 d2 of
      Nothing -> Nothing
      Just x -> let (_,y,_,_) = x
                in case rq_to_duration y of
                     Nothing -> error ("d_join' :" ++ show (a,d1,d2,x))
                     Just _ -> Just x
-}

-- | Type of function used by 'notate' to simplify duration sequence.
--   Arguments specify /alignment/ and /boundaries/.
type Simplify = (RQ -> [RQ] -> [D] -> [D])

-- | Simple minded two pass 'Simplify' function.  The two pass
-- structure is so that @[2,1%2,1%2]@ becomes @[2,1]@ becomes @[3]@.
simplify :: Simplify
simplify a ns xs =
    let xs' = group_boundary_lenient_d ns xs
        pass :: [[D]] -> [[D]]
        pass = map (coalesce (d_join a))
    in concat ((pass . pass) xs')

-- | Variant of 'rq_to_duration' with error message.
to_duration :: Show a => a -> RQ -> Duration
to_duration msg n =
    let err = error ("to_duration:" ++ show (msg,n))
    in fromMaybe err (rq_to_duration n)

tuplet :: (Integer,Integer) -> [Duration] -> [Duration_A]
tuplet (d,n) xs =
    let fn x = x { multiplier = n%d }
        xn = length xs
        ty = to_duration "tuplet" (sum (map duration_to_rq xs) / (d%1))
        t0 = [Begin_Tuplet (d,n,ty)]
        ts = [t0] ++ replicate (xn - 2) [] ++ [[End_Tuplet]]
    in zip (map fn xs) ts

-- | The @d0:dN@ distinction is to catch, for instance, dotted @1\/4@
-- and tuplet @1\/16@.  It'd be better to not simplify to that,
-- however the simplifier does not look ahead.
--
-- > notate_sec (separate [3,3] [2,2,2])
-- > notate_sec (separate [1,1,1] [4%3,1,2%3])
notate_sec :: [D] -> [Duration_A]
notate_sec xs =
    let ds = map d_duration xs
        add_ties_from (_,_,l,r) (d,fs) =
            let l' = if l then [Tie_Left] else []
                r' = if r then [Tie_Right] else []
            in (d,l' ++ r' ++ fs)
        xs' = case derive_tuplet xs of
                Nothing -> let f = to_duration ("notate-sec:no-tuplet",ds)
                           in map (\d -> (f d,[])) ds
                Just t -> let f = to_duration ("notate-sec:tuplet",t,ds)
                              (d0:dN) = ds
                          in if denominator d0 == 2
                             then (f d0,[]) : tuplet t (map (f . un_tuplet t) dN)
                             else tuplet t (map (f . un_tuplet t) ds)
    in zipWith add_ties_from xs xs'

-- | Notate sequence of rational quarter note durations given a
-- 'Simplify' function, a list of /unit divisions/ which must not
-- conflict with a list of /boundaries/ (ie. measures).
--
-- IMPORTANT NOTE: alignments are not handled correctly
--
-- > let n = notate (Just simplify) (replicate 6 1) [4,2] [3,3]
--
-- > import Music.Theory.Duration.Name
-- > n == [(dotted_half_note,[]),(quarter_note,[Tie_Right])
-- >      ,(half_note,[Tie_Left])]
notate :: Maybe Simplify -> [RQ] -> [RQ] -> [RQ] -> [Duration_A]
notate smp is ns xs =
    let xs' = case smp of
                Nothing -> separate is xs
                Just f -> f (head is) ns (separate is xs)
    in concatMap notate_sec (group_boundary_lenient_d is xs')

{-
-- | Variant with default 'simplify' function and constant unit
-- division of @1@.
--
-- > map (duration_to_rq . fst) (notate' [4,4] [3,3,2]) == [3,1,2,2]
notate' :: [RQ] -> [RQ] -> [Duration_A]
notate' x = notate (Just simplify) (replicate (floor (sum x)) 1) x
-}

{-
let xs = [2%3,2%3,2%3,3%2,3%2,2%3,2%3,2%3,1%2,1%2,5%2,3%2]
let xs = map (%4) [1,6,2,3]
let xs = [2%1, 3%5, 2%5]
let is = repeat (1%1)
let ns = repeat (3%1)

map (\(x,y) -> (duration_to_lilypond_type x,y)) (notate is ns xs)
separate is xs
let xs' = simplify (head is) ns (separate is xs)
group_boundary_lenient_d is xs'

let is = [1,1,1,1%2,1%2,1,1]
let ns = [2%5,1%5,1%5,1%5+1%2,1%2,1,1%10,1%10,1%10,1%10,1%10,1%6,1%6,1%6+1%7,2%7,4%7,1]
notate (Just simplify) is [1,5] ns == notate Nothing is [1,5] ns
-}

-- | Zip a list of 'Duration_A' elements duplicating elements of the
-- right hand sequence for tied durations.
--
-- > map snd (ascribe (notate' [4,4] [3,3,2]) "xyz") == "xyyz"
ascribe :: [Duration_A] -> [x] -> [(Duration_A,x)]
ascribe = zip_hold da_tied_right
