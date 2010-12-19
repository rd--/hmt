module Music.Theory.Duration.Sequence.Notate
    (Duration_A
    ,notate
    ,ascribe
    ,group_boundary) where

import Data.List
import Data.Ratio
import Music.Theory.Duration

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

-- | rational modulo
r_mod :: R -> R -> R
r_mod i j
    | i == j = 0
    | i < 0 = r_mod (i + j) j
    | i > j = r_mod (i - j) j
    | otherwise = i

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

separate :: R -> [R] -> [D]
separate n xs =
    let is = 0 : integrate xs
    in concatMap sep_unrep_d (concat (zipWith (sep_at n) is xs))

-- | group to n, or to multiple of
group_boundary :: (a -> R) -> R -> [a] -> [[a]]
group_boundary dur_f n =
    let go _ js [] = [reverse js]
        go _ js [x] = [reverse (x:js)]
        go c js (x:xs) = let c' = c + dur_f x
                         in if c' >= n && c' `r_mod` n == 0
                            then reverse (x:js) : go 0 [] xs
                            else go c' (x:js) xs
    in go 0 []

group_boundary_d :: R -> [D] -> [[D]]
group_boundary_d = group_boundary d_duration

{-
let i = [1,1%2,2,1%3,5%3,1%8,1%2,7%8]
in group_boundary_d 1 (separate 1 i)
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

-- this assumes that input is aligned
d_join :: D -> D -> Maybe D
d_join (s1,x1,l1,r1) (_,x2,_,r2)
    | x1 == 1 && r1 && x2 == 1%2 = Just (s1,x1+x2,l1,r2)
    | x1 == 1 && r1 && x2 == 1 = Just (s1,x1+x2,l1,r2)
    | x1 == 2 && r1 && x2 == 1 = Just (s1,x1+x2,l1,r2)
    | (s1 `r_mod` 1) == 2%3 && x1 == 1%3 && r1 &&
      x2 == 1%3 = Just (s1,x1+x2,l1,r2)
    | otherwise = Nothing

coalesce :: (a -> a -> Maybe a) -> [a] -> [a]
coalesce f xs =
    case xs of
      (x1:x2:xs') -> case f x1 x2 of
                       Nothing -> x1 : coalesce f (x2:xs')
                       Just x' -> coalesce f (x':xs')
      _ -> xs

-- n = boundary
simplify :: R -> [D] -> [D]
simplify n xs =
    let xs' = group_boundary_d n xs
    in concatMap (coalesce d_join) xs'

to_duration :: R -> Duration
to_duration = maybe (error "to_duration") id . rq_to_duration

tuplet :: (Integer,Integer) -> [Duration] -> [Duration_A]
tuplet (d,n) xs =
    let fn x = x { multiplier = n%d }
        xn = length xs
        t0 = [Begin_Tuplet (d,n)]
        ts = [t0] ++ replicate (xn - 2) [] ++ [[End_Tuplet]]
    in zip (map fn xs) ts

notate_sec :: [D] -> [Duration_A]
notate_sec xs =
    let ds = map d_duration xs
        add_ties_from (_,_,l,r) (d,fs) =
            let l' = if l then [Tie_Left] else []
                r' = if r then [Tie_Right] else []
            in (d,l' ++ r' ++ fs)
        xs' = case derive_tuplet xs of
                Nothing -> map (\d -> (to_duration d,[])) ds
                Just t -> tuplet t (map (to_duration . un_tuplet t) ds)
    in zipWith add_ties_from xs xs'

notate :: R -> [R] -> [Duration_A]
notate n xs =
    let xs' = simplify n (separate 1 xs)
    in concatMap notate_sec (group_boundary_d 1 xs')

{-
let i = [2%3,2%3,2%3,3%2,3%2,2%3,2%3,2%3,1%2,1%2,5%2,3%2]
in map (\(x,y) -> (duration_to_lilypond_type x,y)) (notate 3 i)
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
