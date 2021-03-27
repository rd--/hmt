-- | "Data.Function" related functions.
module Music.Theory.Function where

import Data.Function {- base -}

-- | Unary operator.
type UOp t = t -> t

-- | Binary operator.
type BinOp t = t -> t -> t

-- | Iterate the function /f/ /n/ times, the inital value is /x/.
--
-- > recur_n 5 (* 2) 1 == 32
-- > take (5 + 1) (iterate (* 2) 1) == [1,2,4,8,16,32]
recur_n :: Integral n => n -> (t -> t) -> t -> t
recur_n n f x = if n < 1 then x else recur_n (n - 1) f (f x)

-- | 'const' of 'const'.
--
-- > const2 5 undefined undefined == 5
-- > const (const 5) undefined undefined == 5
const2 :: a -> b -> c -> a
const2 x _ _ = x

-- * Predicate composition.

-- | '&&' of predicates, ie. do predicates /f/ and /g/ both hold at /x/.
predicate_and :: (t -> Bool) -> (t -> Bool) -> t -> Bool
predicate_and f g x = f x && g x

-- | List variant of 'predicate_and', ie. 'foldr1'
--
-- > let r = [False,False,True,False,True,False]
-- > map (predicate_all [(> 0),(< 5),even]) [0..5] == r
predicate_all :: [t -> Bool] -> t -> Bool
predicate_all = foldr1 predicate_and
--predicate_all p x = all id (map ($ x) p)

-- | '||' of predicates.
predicate_or :: (t -> Bool) -> (t -> Bool) -> t -> Bool
predicate_or f g x = f x || g x

-- | 'any' of predicates, ie. logical /or/ of list of predicates.
--
-- > let r = [True,False,True,False,True,True]
-- > map (predicate_any [(== 0),(== 5),even]) [0..5] == r
predicate_any :: [t -> Bool] -> t -> Bool
predicate_any p x = any ($ x) p

-- | '==' 'on'.
eq_on :: Eq t => (u -> t) -> u -> u -> Bool
eq_on f = (==) `on` f

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

-- * Bimap

-- | Apply /f/ to both elements of a two-tuple, ie. 'Data.Bifunctor.bimap' /f/ /f/.
bimap1 :: (t -> u) -> (t,t) -> (u,u)
bimap1 f (p,q) = (f p,f q)
