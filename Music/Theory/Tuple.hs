-- | Tuple functions.
--
-- Uniform tuples have types 'T2', 'T3' etc. and functions names are
-- prefixed @t2_@ etc.
--
-- Heterogenous tuples (products) are prefixed @p2_@ etc.
module Music.Theory.Tuple where

import Data.List {- base -}

-- * P2 (2-product)

p2_from_list :: (t -> t1,t -> t2) -> [t] -> (t1,t2)
p2_from_list (f1,f2) l =
  case l of
    [c1,c2] -> (f1 c1,f2 c2)
    _ -> error "p2_from_list"

-- | Swap elements of P2
--
-- > p2_swap (1,2) == (2,1)
p2_swap :: (s,t) -> (t,s)
p2_swap (i,j) = (j,i)

-- * T2 (2-tuple, regular)

-- | Uniform two-tuple.
type T2 a = (a,a)

t2_from_list :: [t] -> T2 t
t2_from_list l = case l of {[p,q] -> (p,q);_ -> error "t2_from_list"}

t2_to_list :: T2 a -> [a]
t2_to_list (i,j) = [i,j]

t2_swap :: T2 t -> T2 t
t2_swap = p2_swap

t2_map :: (p -> q) -> T2 p -> T2 q
t2_map f (p,q) = (f p,f q)

t2_zipWith :: (p -> q -> r) -> T2 p -> T2 q -> T2 r
t2_zipWith f (p,q) (p',q') = (f p p',f q q')

t2_infix :: (a -> a -> b) -> T2 a -> b
t2_infix f (i,j) = i `f` j

-- | 't2_infix' 'mappend'.
--
-- > t2_join ([1,2],[3,4]) == [1,2,3,4]
t2_join :: Monoid m => T2 m -> m
t2_join = t2_infix mappend

-- | 't2_map' 'mconcat' of 'unzip'
--
-- > t2_concat [("ab","cd"),("ef","gh")] == ("abef","cdgh")
t2_concat :: Monoid m => [T2 m] -> T2 m
t2_concat = t2_map mconcat . unzip

-- | 'sort'
--
-- > t2_sort (2,1) == (1,2)
t2_sort :: Ord t => (t,t) -> (t,t)
t2_sort (p,q) = (min p q,max p q)

-- | 'sum'
t2_sum :: Num n => (n,n) -> n
t2_sum (i,j) = i + j

-- | 'mapM'
t2_mapM :: Monad m => (t -> m u) -> (t,t) -> m (u,u)
t2_mapM f (i,j) = f i >>= \p -> f j >>= \q -> return (p,q)

-- | 'mapM_'
t2_mapM_ :: Monad m => (t -> m u) -> (t,t) -> m ()
t2_mapM_ f (i,j) = f i >> f j >> return ()

-- * P3 (3-product)

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

t3_from_list :: [t] -> T3 t
t3_from_list l = case l of {[p,q,r] -> (p,q,r);_ -> error "t3_from_list"}

t3_to_list :: T3 a -> [a]
t3_to_list (i,j,k) = [i,j,k]

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

t3_infix :: (a -> a -> a) -> T3 a -> a
t3_infix f (i,j,k) = (i `f` j) `f` k

t3_join :: T3 [a] -> [a]
t3_join = t3_infix (++)

t3_sort :: Ord t => (t,t,t) -> (t,t,t)
t3_sort = t3_from_list . sort . t3_to_list

-- * P4 (4-product)

p4_fst :: (a,b,c,d) -> a
p4_fst (a,_,_,_) = a

p4_snd :: (a,b,c,d) -> b
p4_snd (_,b,_,_) = b

p4_third :: (a,b,c,d) -> c
p4_third (_,_,c,_) = c

p4_fourth :: (a,b,c,d) -> d
p4_fourth (_,_,_,d) = d

p4_zip :: (a,b,c,d) -> (e,f,g,h) -> ((a,e),(b,f),(c,g),(d,h))
p4_zip (a,b,c,d) (e,f,g,h) = ((a,e),(b,f),(c,g),(d,h))

-- * T4 (4-tuple, regular)

type T4 a = (a,a,a,a)

t4_from_list :: [t] -> T4 t
t4_from_list l = case l of {[p,q,r,s] -> (p,q,r,s); _ -> error "t4_from_list"}

t4_to_list :: T4 t -> [t]
t4_to_list (p,q,r,s) = [p,q,r,s]

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

-- * P5 (5-product)

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

p5_from_list :: (t -> t1, t -> t2, t -> t3, t -> t4, t -> t5) -> [t] -> (t1,t2,t3,t4,t5)
p5_from_list (f1,f2,f3,f4,f5) l =
  case l of
    [c1,c2,c3,c4,c5] -> (f1 c1,f2 c2,f3 c3,f4 c4,f5 c5)
    _ -> error "p5_from_list"

p5_to_list :: (t1 -> t, t2 -> t, t3 -> t, t4 -> t, t5 -> t) -> (t1, t2, t3, t4, t5) -> [t]
p5_to_list (f1,f2,f3,f4,f5) (c1,c2,c3,c4,c5) = [f1 c1,f2 c2,f3 c3,f4 c4,f5 c5]

-- * T5 (5-tuple, regular)

type T5 a = (a,a,a,a,a)

t5_from_list :: [t] -> T5 t
t5_from_list l = case l of {[p,q,r,s,t] -> (p,q,r,s,t); _ -> error "t5_from_list"}

t5_to_list :: T5 t -> [t]
t5_to_list (p,q,r,s,t) = [p,q,r,s,t]

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

-- * P6 (6-product)

p6_fst :: (a,b,c,d,e,f) -> a
p6_fst (a,_,_,_,_,_) = a

p6_snd :: (a,b,c,d,e,f) -> b
p6_snd (_,b,_,_,_,_) = b

p6_third :: (a,b,c,d,e,f) -> c
p6_third (_,_,c,_,_,_) = c

p6_fourth :: (a,b,c,d,e,f) -> d
p6_fourth (_,_,_,d,_,_) = d

p6_fifth :: (a,b,c,d,e,f) -> e
p6_fifth (_,_,_,_,e,_) = e

p6_sixth :: (a,b,c,d,e,f) -> f
p6_sixth (_,_,_,_,_,f) = f

-- * T6 (6-tuple, regular)

type T6 a = (a,a,a,a,a,a)

t6_from_list :: [t] -> T6 t
t6_from_list l = case l of {[p,q,r,s,t,u] -> (p,q,r,s,t,u);_ -> error "t6_from_list"}

t6_to_list :: T6 t -> [t]
t6_to_list (p,q,r,s,t,u) = [p,q,r,s,t,u]

t6_map :: (p -> q) -> T6 p -> T6 q
t6_map f (p,q,r,s,t,u) = (f p,f q,f r,f s,f t,f u)

t6_sum :: Num t => T6 t -> t
t6_sum (a,b,c,d,e,f) = a + b + c + d + e + f

-- * T7 (7-tuple, regular)

type T7 a = (a,a,a,a,a,a,a)

t7_to_list :: T7 t -> [t]
t7_to_list (p,q,r,s,t,u,v) = [p,q,r,s,t,u,v]

t7_map :: (p -> q) -> T7 p -> T7 q
t7_map f (p,q,r,s,t,u,v) = (f p,f q,f r,f s,f t,f u,f v)

-- * T8 (8-tuple, regular)

type T8 a = (a,a,a,a,a,a,a,a)

t8_to_list :: T8 t -> [t]
t8_to_list (p,q,r,s,t,u,v,w) = [p,q,r,s,t,u,v,w]

t8_map :: (p -> q) -> T8 p -> T8 q
t8_map f (p,q,r,s,t,u,v,w) = (f p,f q,f r,f s,f t,f u,f v,f w)

-- * P8 (8-product)

p8_third :: (a,b,c,d,e,f,g,h) -> c
p8_third (_,_,c,_,_,_,_,_) = c

-- * T9 (9-tuple, regular)

type T9 a = (a,a,a,a,a,a,a,a,a)

t9_to_list :: T9 t -> [t]
t9_to_list (p,q,r,s,t,u,v,w,x) = [p,q,r,s,t,u,v,w,x]

t9_from_list :: [t] -> T9 t
t9_from_list l = case l of {[p,q,r,s,t,u,v,w,x] -> (p,q,r,s,t,u,v,w,x); _ -> error "t9_from_list?"}

t9_map :: (p -> q) -> T9 p -> T9 q
t9_map f (p,q,r,s,t,u,v,w,x) = (f p,f q,f r,f s,f t,f u,f v,f w,f x)

-- * T10 (10-tuple, regular)

type T10 a = (a,a,a,a,a,a,a,a,a,a)

t10_to_list :: T10 t -> [t]
t10_to_list (p,q,r,s,t,u,v,w,x,y) = [p,q,r,s,t,u,v,w,x,y]

t10_map :: (p -> q) -> T10 p -> T10 q
t10_map f (p,q,r,s,t,u,v,w,x,y) = (f p,f q,f r,f s,f t,f u,f v,f w,f x,f y)

-- * T11 (11-tuple, regular)

type T11 a = (a,a,a,a,a,a,a,a,a,a,a)

t11_to_list :: T11 t -> [t]
t11_to_list (p,q,r,s,t,u,v,w,x,y,z) = [p,q,r,s,t,u,v,w,x,y,z]

t11_map :: (p -> q) -> T11 p -> T11 q
t11_map f (p,q,r,s,t,u,v,w,x,y,z) = (f p,f q,f r,f s,f t,f u,f v,f w,f x,f y,f z)

-- * T12 (12-tuple, regular)

type T12 t = (t,t,t,t,t,t,t,t,t,t,t,t)

t12_to_list :: T12 t -> [t]
t12_to_list (p,q,r,s,t,u,v,w,x,y,z,a) = [p,q,r,s,t,u,v,w,x,y,z,a]

t12_from_list :: [t] -> T12 t
t12_from_list l =
    case l of
      [p,q,r,s,t,u,v,w,x,y,z,a] -> (p,q,r,s,t,u,v,w,x,y,z,a)
      _ -> error "t12_from_list"

-- | 'foldr1' of 't12_to_list'.
--
-- > t12_foldr1 (+) (1,2,3,4,5,6,7,8,9,10,11,12) == 78
t12_foldr1 :: (t -> t -> t) -> T12 t -> t
t12_foldr1 f = foldr1 f . t12_to_list

-- | 'sum' of 't12_to_list'.
--
-- > t12_sum (1,2,3,4,5,6,7,8,9,10,11,12) == 78
t12_sum :: Num n => T12 n -> n
t12_sum t =
    let (n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12) = t
    in n1 + n2 + n3 + n4 + n5 + n6 + n7 + n8 + n9 + n10 + n11 + n12

-- * Family of 'uncurry' functions.

uncurry3 :: (a->b->c -> z) -> (a,b,c) -> z
uncurry3 fn (a,b,c) = fn a b c
uncurry4 :: (a->b->c->d -> z) -> (a,b,c,d) -> z
uncurry4 fn (a,b,c,d) = fn a b c d
uncurry5 :: (a->b->c->d->e -> z) -> (a,b,c,d,e) -> z
uncurry5 fn (a,b,c,d,e) = fn a b c d e
uncurry6 :: (a->b->c->d->e->f -> z) -> (a,b,c,d,e,f) -> z
uncurry6 fn (a,b,c,d,e,f) = fn a b c d e f
uncurry7 :: (a->b->c->d->e->f->g -> z) -> (a,b,c,d,e,f,g) -> z
uncurry7 fn (a,b,c,d,e,f,g) = fn a b c d e f g
uncurry8 :: (a->b->c->d->e->f->g->h -> z) -> (a,b,c,d,e,f,g,h) -> z
uncurry8 fn (a,b,c,d,e,f,g,h) = fn a b c d e f g h
uncurry9 :: (a->b->c->d->e->f->g->h->i -> z) -> (a,b,c,d,e,f,g,h,i) -> z
uncurry9 fn (a,b,c,d,e,f,g,h,i) = fn a b c d e f g h i
uncurry10 :: (a->b->c->d->e->f->g->h->i->j -> z) -> (a,b,c,d,e,f,g,h,i,j) -> z
uncurry10 fn (a,b,c,d,e,f,g,h,i,j) = fn a b c d e f g h i j
uncurry11 :: (a->b->c->d->e->f->g->h->i->j->k -> z) -> (a,b,c,d,e,f,g,h,i,j,k) -> z
uncurry11 fn (a,b,c,d,e,f,g,h,i,j,k) = fn a b c d e f g h i j k
uncurry12 :: (a->b->c->d->e->f->g->h->i->j->k->l -> z) -> (a,b,c,d,e,f,g,h,i,j,k,l) -> z
uncurry12 fn (a,b,c,d,e,f,g,h,i,j,k,l) = fn a b c d e f g h i j k l
uncurry13 :: (a->b->c->d->e->f->g->h->i->j->k->l->m -> z) -> (a,b,c,d,e,f,g,h,i,j,k,l,m) -> z
uncurry13 fn (a,b,c,d,e,f,g,h,i,j,k,l,m) = fn a b c d e f g h i j k l m
uncurry14 :: (a->b->c->d->e->f->g->h->i->j->k->l->m->n -> z) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n) -> z
uncurry14 fn (a,b,c,d,e,f,g,h,i,j,k,l,m,n) = fn a b c d e f g h i j k l m n
uncurry15 :: (a->b->c->d->e->f->g->h->i->j->k->l->m->n->o -> z) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) -> z
uncurry15 fn (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) = fn a b c d e f g h i j k l m n o
uncurry16 :: (a->b->c->d->e->f->g->h->i->j->k->l->m->n->o->p -> z) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) -> z
uncurry16 fn (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) = fn a b c d e f g h i j k l m n o p
uncurry17 :: (a->b->c->d->e->f->g->h->i->j->k->l->m->n->o->p->q -> z) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) -> z
uncurry17 fn (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) = fn a b c d e f g h i j k l m n o p q
uncurry18 :: (a->b->c->d->e->f->g->h->i->j->k->l->m->n->o->p->q->r -> z) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) -> z
uncurry18 fn (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) = fn a b c d e f g h i j k l m n o p q r
uncurry19 :: (a->b->c->d->e->f->g->h->i->j->k->l->m->n->o->p->q->r->s -> z) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) -> z
uncurry19 fn (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) = fn a b c d e f g h i j k l m n o p q r s
uncurry20 :: (a->b->c->d->e->f->g->h->i->j->k->l->m->n->o->p->q->r->s->t -> z) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) -> z
uncurry20 fn (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) = fn a b c d e f g h i j k l m n o p q r s t

-- Local Variables:
-- truncate-lines:t
-- End:
