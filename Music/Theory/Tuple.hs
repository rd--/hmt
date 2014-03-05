-- | Tuple functions.
--
-- Uniform tuples have types 'T2', 'T3' etc. and functions names are
-- prefixed @t2_@ etc.
--
-- Heterogenous tuples (products) are prefixed @p2_@ etc.
module Music.Theory.Tuple where

import Data.Monoid {- base -}

-- * P2 (2 product)

p2_swap :: (s,t) -> (t,s)
p2_swap (i,j) = (j,i)

-- * T2 (2-tuple, regular)

-- | Uniform two-tuple.
type T2 a = (a,a)

t2 :: [t] -> T2 t
t2 l = case l of {[p,q] -> (p,q);_ -> error "t2"}

t2_list :: T2 a -> [a]
t2_list (i,j) = [i,j]

t2_swap :: T2 t -> T2 t
t2_swap = p2_swap

t2_map :: (p -> q) -> T2 p -> T2 q
t2_map f (p,q) = (f p,f q)

t2_zipWith :: (p -> q -> r) -> T2 p -> T2 q -> T2 r
t2_zipWith f (p,q) (p',q') = (f p p',f q q')

t2_infix :: (a -> a -> b) -> T2 a -> b
t2_infix f (i,j) = i `f` j

-- | Infix 'mappend'.
--
-- > t2_join ([1,2],[3,4]) == [1,2,3,4]
t2_join :: Monoid m => T2 m -> m
t2_join = t2_infix mappend

t2_concat :: [T2 [a]] -> T2 [a]
t2_concat = t2_map mconcat . unzip

t2_sort :: Ord t => (t,t) -> (t,t)
t2_sort (p,q) = (min p q,max p q)

-- * P3 (3 product)

-- | Left rotation.
--
-- > p3_rotate_left (1,2,3) == (2,3,1)
p3_rotate_left :: (s,t,u) -> (t,u,s)
p3_rotate_left (i,j,k) = (j,k,i)

p3_fst :: (a,b,c) -> a
p3_fst (a,_,_) = a

p3_snd :: (a,b,c) -> b
p3_snd (_,b,_) = b

p3_third :: (a,b,c) -> c
p3_third (_,_,c) = c

-- * T3 (3 triple, regular)

type T3 a = (a,a,a)

t3 :: [t] -> T3 t
t3 l = case l of {[p,q,r] -> (p,q,r);_ -> error "t3"}

t3_rotate_left :: T3 t -> T3 t
t3_rotate_left = p3_rotate_left

t3_fst :: T3 t -> t
t3_fst = p3_fst

t3_snd :: T3 t -> t
t3_snd = p3_snd

t3_third :: T3 t -> t
t3_third = p3_third

t3_map :: (p -> q) -> T3 p -> T3 q
t3_map f (p,q,r) = (f p,f q,f r)

t3_zipWith :: (p -> q -> r) -> T3 p -> T3 q -> T3 r
t3_zipWith f (p,q,r) (p',q',r') = (f p p',f q q',f r r')
t3_list :: T3 a -> [a]
t3_list (i,j,k) = [i,j,k]

t3_infix :: (a -> a -> a) -> T3 a -> a
t3_infix f (i,j,k) = (i `f` j) `f` k

t3_join :: T3 [a] -> [a]
t3_join = t3_infix (++)

-- * P4 (4 product)

p4_fst :: (a,b,c,d) -> a
p4_fst (a,_,_,_) = a

p4_snd :: (a,b,c,d) -> b
p4_snd (_,b,_,_) = b

p4_third :: (a,b,c,d) -> c
p4_third (_,_,c,_) = c

p4_fourth :: (a,b,c,d) -> d
p4_fourth (_,_,_,d) = d

-- * T4 (4-tuple, regular)

type T4 a = (a,a,a,a)

t4 :: [t] -> T4 t
t4 l = case l of {[p,q,r,s] -> (p,q,r,s); _ -> error "t4"}

t4_list :: T4 t -> [t]
t4_list (p,q,r,s) = [p,q,r,s]

t4_fst :: T4 t -> t
t4_fst = p4_fst

t4_snd :: T4 t -> t
t4_snd = p4_snd

t4_third :: T4 t -> t
t4_third = p4_third

t4_fourth :: T4 t -> t
t4_fourth = p4_fourth

t4_map :: (p -> q) -> T4 p -> T4 q
t4_map f (p,q,r,s) = (f p,f q,f r,f s)

t4_zipWith :: (p -> q -> r) -> T4 p -> T4 q -> T4 r
t4_zipWith f (p,q,r,s) (p',q',r',s') = (f p p',f q q',f r r',f s s')

t4_infix :: (a -> a -> a) -> T4 a -> a
t4_infix f (i,j,k,l) = ((i `f` j) `f` k) `f` l

t4_join :: T4 [a] -> [a]
t4_join = t4_infix (++)

-- * P5 (5 product)

p5_fst :: (a,b,c,d,e) -> a
p5_fst (a,_,_,_,_) = a

p5_snd :: (a,b,c,d,e) -> b
p5_snd (_,b,_,_,_) = b

p5_third :: (a,b,c,d,e) -> c
p5_third (_,_,c,_,_) = c

p5_fourth :: (a,b,c,d,e) -> d
p5_fourth (_,_,_,d,_) = d

p5_fifth :: (a,b,c,d,e) -> e
p5_fifth (_,_,_,_,e) = e

-- * T5 (5-tuple, regular)

type T5 a = (a,a,a,a,a)

t5 :: [t] -> T5 t
t5 l = case l of {[p,q,r,s,t] -> (p,q,r,s,t); _ -> error "t5"}

t5_list :: T5 t -> [t]
t5_list (p,q,r,s,t) = [p,q,r,s,t]

t5_map :: (p -> q) -> T5 p -> T5 q
t5_map f (p,q,r,s,t) = (f p,f q,f r,f s,f t)

t5_fst :: T5 t -> t
t5_fst (p,_,_,_,_) = p

t5_snd :: T5 t -> t
t5_snd (_,q,_,_,_) = q

t5_fourth :: T5 t -> t
t5_fourth (_,_,_,t,_) = t

t5_fifth :: T5 t -> t
t5_fifth (_,_,_,_,u) = u

t5_infix :: (a -> a -> a) -> T5 a -> a
t5_infix f (i,j,k,l,m) = (((i `f` j) `f` k) `f` l) `f` m

t5_join :: T5 [a] -> [a]
t5_join = t5_infix (++)

-- * T6 (6-tuple, regular)

type T6 a = (a,a,a,a,a,a)

t6 :: [t] -> T6 t
t6 l = case l of {[p,q,r,s,t,u] -> (p,q,r,s,t,u);_ -> error "t6"}

t6_list :: T6 t -> [t]
t6_list (p,q,r,s,t,u) = [p,q,r,s,t,u]

t6_map :: (p -> q) -> T6 p -> T6 q
t6_map f (p,q,r,s,t,u) = (f p,f q,f r,f s,f t,f u)

-- * T7 (7-tuple, regular)

type T7 a = (a,a,a,a,a,a,a)

t7_list :: T7 t -> [t]
t7_list (p,q,r,s,t,u,v) = [p,q,r,s,t,u,v]

t7_map :: (p -> q) -> T7 p -> T7 q
t7_map f (p,q,r,s,t,u,v) = (f p,f q,f r,f s,f t,f u,f v)

-- * T8 (8-tuple, regular)

type T8 a = (a,a,a,a,a,a,a,a)

t8_list :: T8 t -> [t]
t8_list (p,q,r,s,t,u,v,w) = [p,q,r,s,t,u,v,w]

t8_map :: (p -> q) -> T8 p -> T8 q
t8_map f (p,q,r,s,t,u,v,w) = (f p,f q,f r,f s,f t,f u,f v,f w)

-- * T9 (9-tuple, regular)

type T9 a = (a,a,a,a,a,a,a,a,a)

t9_list :: T9 t -> [t]
t9_list (p,q,r,s,t,u,v,w,x) = [p,q,r,s,t,u,v,w,x]

t9_map :: (p -> q) -> T9 p -> T9 q
t9_map f (p,q,r,s,t,u,v,w,x) = (f p,f q,f r,f s,f t,f u,f v,f w,f x)
