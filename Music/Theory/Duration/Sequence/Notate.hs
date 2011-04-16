module Music.Theory.Duration.Sequence.Notate
    (Duration_A
    ,Simplify,simplify
    ,notate
    ,ascribe
    ,group_boundary) where

import Data.List
import Data.Ratio
import Music.Theory.Duration

{-
import Debug.Trace
debug :: (Show a) => a -> x -> x
debug = traceShow
-}

debug :: (Show a) => a -> x -> x
debug _ x = x

type R = Rational
type D = (R,R,Bool,Bool) {- start_time duration tied_left tied_right -}
type Duration_A = (Duration,[D_Annotation])

d_duration :: D -> R
d_duration (_,x,_,_) = x

da_tied_right :: Duration_A -> Bool
da_tied_right = elem Tie_Right . snd

-- | dx -> d
integrate :: (Num a) => [a] -> [a]
integrate [] = []
integrate (x:xs) =
    let fn i c = (i + c, i + c)
    in x : snd (mapAccumL fn x xs)

-- xs = boundaries
-- d = duration
step_dur :: (Ord a, Num a) => [a] -> a -> ([a], [a])
step_dur [] _ = error "step_dur: no boundaries"
step_dur _ 0 = error "step_dur: zero duration"
step_dur (x:xs) d =
    let jn a (a',b) = (a:a',b)
    in case compare d x of
         EQ -> ([d],xs)
         LT -> ([d],(x-d):xs)
         GT -> jn x (step_dur xs (d - x))

{-
step_dur [2,1,3] 5
step_dur [3%2,3%2,3%2] 2
-}

-- xs = boundaries
-- d(s) = duration(s)
boundaries :: (Num a, Ord a) => [a] -> [a] -> [[a]]
boundaries =
    let go [] _ = []
        go _ [] = []
        go xs (d:ds) =
            let (d',xs') = step_dur xs d
            in d' : go xs' ds
    in go

{-
boundaries (repeat 3) [1..5]
boundaries (repeat (3%2)) [1%2,1..5]
-}

-- i = initial start time
-- xs = durations
with_start_times :: (Num a) => a -> [a] -> [(a,a)]
with_start_times i xs =
    let is = map (+i) (0 : integrate xs)
    in zip is xs

-- variant starting at zero and processing sets of durations
with_start_times' :: (Num a) => [[a]] -> [[(a, a)]]
with_start_times' xs =
    let is = 0 : integrate (map sum xs)
    in zipWith with_start_times is xs

{-
with_start_times 0 [4,3,5,2,1]
with_start_times' [[4,3,5],[2,1],[6,3]]
with_start_times' (boundaries [3,3,3,3,3] [4,3,5,2,1])
let xs = [3%4,2%1,5%4,9%4,1%4,3%2,1%2,7%4,1%1,5%2,11%4,3%2]
with_start_times 0 xs
with_start_times' (boundaries (repeat (3%2)) xs)
-}

-- split list into first element, possibly empty 'middle' elements,
-- and end element
start_middle_end :: [x] -> (x,[x],x)
start_middle_end xs =
    case xs of
      (_:_:_) -> let n = length xs
                     x0 = xs !! 0
                     xn = xs !! (n - 1)
                 in (x0,take (n - 2) (drop 1 xs),xn)
      _ -> error "start_middle_end: list must have at least two elements"

{-
start_middle_end []
start_middle_end [1..6]
-}

-- xs = [(start-time,duration)]
tied_r_to_d :: [(R,R)] -> [D]
tied_r_to_d xs =
    case xs of
      [] -> []
      [(s,d)] -> [(s,d,False,False)]
      _ -> let ((s0,d0),xs',(sn,dn)) = start_middle_end xs
               f (s,d) = (s,d,True,True)
            in (s0,d0,False,True) : map f xs' ++ [(sn,dn,True,False)]

boundaries_d :: [R] -> [R] -> [D]
boundaries_d xs ds =
    let bs = boundaries xs ds
    in concatMap tied_r_to_d (with_start_times' bs)

{-
boundaries_d [3,3,3,3,3,3,3,3] [4,3,5,2,1,7,2]
-}

-- | rational modulo
r_mod :: R -> R -> R
r_mod i j
    | i == j = 0
    | i < 0 = r_mod (i + j) j
    | i > j = r_mod (i - j) j
    | otherwise = i

{-
-- n = boundary
-- i = phase
sep_at :: R -> R -> R -> [D]
sep_at =
    let go l n i x =
            let i' = n - (i `r_mod` n)
            in if x > i'
               then let d = (i,i',l,True)
                    in d : go True n (i + i') (x - i')
               else [(i,x,l,False)]
    in go False

{-
sep_at 1 (1%2) 1
sep_at 1 (1%3) (6%3)
-}
-}

-- unrep = un-representable by single cmn duration (ie. requires tie)
-- i = phase
-- x = duration
sep_unrep :: R -> R -> Maybe (R,R)
sep_unrep i x =
    let i' = denominator i == 1
        j = case numerator x of
              5 -> Just (1,4)
              7 -> Just (3,4)
              _ -> Nothing
        f (n,m) = (n % denominator x,m % denominator x)
        swap (a,b) = (b,a)
    in case j of
         Nothing -> Nothing
         Just j' -> Just (f (if i' then swap j' else j'))

sep_unrep_d :: D -> [D]
sep_unrep_d d =
    let (i,x,l,r) = d
    in case sep_unrep i x of
         Nothing -> [d]
         Just (x0,x1) -> [(i,x0,l,True),(i+x0,x1,True,r)]

{-
zipWith sep_unrep [1,3%8,1] [5%4,5%8,4]
zipWith (\i x -> sep_unrep_d (i,x,False,False)) [1,3%8,1] [5%4,5%8,4]
-}

separate :: [R] -> [R] -> [D]
separate ns = concatMap sep_unrep_d . boundaries_d ns

{-
let xs = [3%4,2%1,5%4,9%4,1%4,3%2,1%2,7%4,1%1,5%2,11%4,3%2]
separate (repeat (1%2)) xs
-}

-- | group to n, or to multiple of
group_boundary :: (a -> R) -> [R] -> [a] -> [[a]]
group_boundary dur_f =
    let go _ [] [] _ = []
        go _ _ [] _ = error "group_boundary: no boundaries?"
        go _ js _ [] = [reverse js]
        go _ js _ [x] = [reverse (x:js)]
        go c js (n:ns) (x:xs) =
            let c' = c + dur_f x
            in case compare c' n of
                 EQ -> reverse (x:js) : go 0 [] ns xs
                 LT -> go c' (x:js) (n:ns) xs
                 GT -> let c'' = c' - n
                       in if c'' `divisible_by` n
                          then reverse (x:js) : go 0 [] ns xs
                          else go c'' (x:js) ns xs
    in go 0 []

{-
group_boundary id [1,1,1] [2,1%2,1%2]
-}

group_boundary_d :: [R] -> [D] -> [[D]]
group_boundary_d = group_boundary d_duration

{-
group_boundary id [3,3,3] (cycle [1,2,3])

let i = [1,1%2,2,1%3,5%3,1%8,1%2,7%8]
in group_boundary_d (repeat 1) (separate (repeat 1) i)
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
let i = [1,1%2,2,1%3,5%3,1%8,1%2,7%8]
in map derive_tuplet (group_boundary_d 1 (separate 1 i))
-}

-- remove tuplet multiplier from value (ie. to give notated duration)
-- this seems odd but is neccessary to avoid ambiguity (ie. is 1 a
-- quarter note or a 3:2 tuplet dotted-quarter-note etc.
un_tuplet :: (Integer,Integer) -> R -> R
un_tuplet (i,j) x = x * (i%j)

d_join_aligned :: D -> D -> Maybe D
d_join_aligned (s1,x1,l1,r1) (_,x2,_,r2)
    | (x1 == (1%4) && r1 && x2 `elem` [1%4,1%2,3%4]) ||
      (x1 == (1%2) && r1 && x2 `elem` [1%4,1%2,1,3%2]) ||
      (x1 == 1 && r1 && x2 `elem` [1%2,1,2]) ||
      (x1 == (3%2) && r1 && x2 `elem` [1%2,3%2]) ||
      (x1 == 2 && r1 && x2 `elem` [1,2]) = debug ("aligned-join",s1,x1,x2) (Just (s1,x1+x2,l1,r2))
    | otherwise = debug ("aligned-no-join",s1,x1,r1,x2) Nothing

divisible_by :: R -> R -> Bool
divisible_by i j = denominator (i / j) == 1

-- partial/incomplete/inaccurate
d_join :: R -> D -> D -> Maybe D
d_join a (s1,x1,l1,r1) (s2,x2,l2,r2)
    | s1 `divisible_by` a = d_join_aligned (s1,x1,l1,r1) (s2,x2,l2,r2)
    | denominator (s1 `r_mod` 1) == 4 &&
      x1 == 1%4 &&
      r1 &&
      x2 == 1%4 &&
      not (s2 `divisible_by` a) =
      debug ("non-aligned-join",a,s1,x1) (Just (s1,x1+x2,l1,r2))
    | s1 `r_mod` 1 == 2%3 &&
      x1 == 1%3 &&
      r1 &&
      x2 == 1%3 =
      debug ("non-aligned-join",a,s1,x1) (Just (s1,x1+x2,l1,r2))
    | otherwise = debug ("non-aligned-no-join",a,s1,x1) Nothing

{-
d_join 1 (7 % 4,1 % 4,False,True) (2 % 1,1 % 4,True,False)
-}

{-
-- error checking variant
d_join' :: R -> D -> D -> Maybe D
d_join' a d1 d2 =
    case d_join a d1 d2 of
      Nothing -> Nothing
      Just x -> let (_,y,_,_) = x
                in case rq_to_duration y of
                     Nothing -> error ("d_join' :" ++ show (a,d1,d2,x))
                     Just _ -> Just x
-}

coalesce :: (a -> a -> Maybe a) -> [a] -> [a]
coalesce f xs =
    case xs of
      (x1:x2:xs') -> case f x1 x2 of
                       Nothing -> x1 : coalesce f (x2:xs')
                       Just x' -> coalesce f (x':xs')
      _ -> xs

type Simplify = (R -> [R] -> [D] -> [D])

-- a = alignment
-- ns = boundaries
-- two pass, ie. [2,1%2,1%2] becomes [2,1] becomes [3]
simplify :: Simplify
simplify a ns xs =
    let xs' = group_boundary_d ns xs
        pass :: [[D]] -> [[D]]
        pass = map (coalesce (d_join a))
    in concat ((pass . pass) xs')

-- erroring variant of rq_to_duration
to_duration :: Show a => a -> R -> Duration
to_duration msg n =
    let err = error ("to_duration:" ++ show (msg,n))
    in maybe err id (rq_to_duration n)

tuplet :: (Integer,Integer) -> [Duration] -> [Duration_A]
tuplet (d,n) xs =
    let fn x = x { multiplier = n%d }
        xn = length xs
        ty = to_duration "tuplet" (sum (map duration_to_rq xs) / (d%1))
        t0 = [Begin_Tuplet (d,n,ty)]
        ts = [t0] ++ replicate (xn - 2) [] ++ [[End_Tuplet]]
    in zip (map fn xs) ts

-- the d0:dN distinction is to catch, for instance, dotted 1/4 and
-- tuplet 1/16.  it'd be better to not simplify to that, however
-- simplifier does not look ahead.
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

-- is = unit divisions (must not conflict with ns)
-- ns = boundaries (ie. measures)
-- xs = durations
-- IMPORTANT NOTE: alignments are not handled correctly
notate :: Maybe Simplify -> [R] -> [R] -> [R] -> [Duration_A]
notate smp is ns xs =
    let xs' = case smp of
                Nothing -> separate is xs
                Just f -> f (head is) ns (separate is xs)
    in concatMap notate_sec (group_boundary_d is xs')

{-
let xs = [2%3,2%3,2%3,3%2,3%2,2%3,2%3,2%3,1%2,1%2,5%2,3%2]
let xs = map (%4) [1,6,2,3]
let xs = [2 % 1, 3 % 5, 2 % 5]
let is = repeat (1%1)
let ns = repeat (3%1)

map (\(x,y) -> (duration_to_lilypond_type x,y)) (notate is ns xs)
separate is xs
let xs' = simplify (head is) ns (separate is xs)
group_boundary_d is xs'

let is = [1,1,1,1%2,1%2,1,1]
let ns = [2%5,1%5,1%5,1%5+1%2,1%2,1,1%10,1%10,1%10,1%10,1%10,1%6,1%6,1%6+1%7,2%7,4%7,1]
notate (Just simplify) is [1,5] ns == notate Nothing is [1,5] ns
-}

ascribe_fn :: (x -> Bool) -> [x] -> [a] -> [(x,a)]
ascribe_fn fn =
    let go [] _ = []
        go _ [] = error "ascribe_fn"
        go (x:xs) (i:is) = let is' = if fn x then (i:is) else is
                           in (x,i) : go xs is'
    in go

ascribe :: [Duration_A] -> [x] -> [(Duration_A,x)]
ascribe = ascribe_fn da_tied_right
