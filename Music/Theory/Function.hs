-- | "Data.Function" related functions.
module Music.Theory.Function where

type UOp t = t -> t
type BinOp t = t -> t -> t

-- | 'const' of 'const'.
--
-- > const2 5 undefined undefined == 5
-- > const (const 5) undefined undefined == 5
const2 :: a -> b -> c -> a
const2 x _ _ = x

-- * Predicate composition.

-- | '&&' of predicates.
predicate_and :: (t -> Bool) -> (t -> Bool) -> t -> Bool
predicate_and f g x = f x && g x

-- | 'all' of predicates.
--
-- > let r = [False,False,True,False,True,False]
-- > in map (predicate_all [(> 0),(< 5),even]) [0..5] == r
predicate_all :: [t -> Bool] -> t -> Bool
predicate_all p x = all id (map ($ x) p)

-- | '||' of predicates.
predicate_or :: (t -> Bool) -> (t -> Bool) -> t -> Bool
predicate_or f g x = f x || g x

-- | 'any' of predicates, ie. logical /or/ of list of predicates.
--
-- > let r = [True,False,True,False,True,True]
-- > in map (predicate_any [(== 0),(== 5),even]) [0..5] == r
predicate_any :: [t -> Bool] -> t -> Bool
predicate_any p x = any id (map ($ x) p)

-- * Function composition.

-- . is infixr 9, this allows f . g .: h
infixr 8 .:, .::, .:::, .::::, .:::::

-- | 'fmap' '.' 'fmap', ie. @(t -> c) -> (a -> b -> t) -> a -> b -> c@.
(.:) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(.:) = fmap . fmap

-- | 'fmap' '.' '.:', ie. @(t -> d) -> (a -> b -> c -> t) -> a -> b -> c -> d@.
(.::) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(.::) = fmap . (.:)

-- | 'fmap' '.' '.::'.
(.:::) :: (Functor f, Functor g, Functor h,Functor i) => (a -> b) -> f (g (h (i a))) -> f (g (h (i b)))
(.:::) = fmap . (.::)

-- | 'fmap' '.' '.:::'.
(.::::) :: (Functor f, Functor g, Functor h,Functor i,Functor j) => (a -> b) -> f (g (h (i (j a)))) -> f (g (h (i (j b))))
(.::::) = fmap . (.:::)

-- | 'fmap' '.' '.::::'.
(.:::::) :: (Functor f, Functor g, Functor h,Functor i,Functor j,Functor k) => (a -> b) -> f (g (h (i (j (k a))))) -> f (g (h (i (j (k b)))))
(.:::::) = fmap . (.::::)

