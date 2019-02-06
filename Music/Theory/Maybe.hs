-- | Extensions to "Data.Maybe".
module Music.Theory.Maybe where

import Data.Maybe {- base -}

-- | Variant with error text.
from_just :: String -> Maybe a -> a
from_just err = fromMaybe (error err)

-- | Variant of unzip.
--
-- > let r = ([Just 1,Nothing,Just 3],[Just 'a',Nothing,Just 'c'])
-- > in maybe_unzip [Just (1,'a'),Nothing,Just (3,'c')] == r
maybe_unzip :: [Maybe (a,b)] -> ([Maybe a],[Maybe b])
maybe_unzip =
    let f x = case x of
                Nothing -> (Nothing,Nothing)
                Just (i,j) -> (Just i,Just j)
    in unzip . map f

-- | Replace 'Nothing' elements with last 'Just' value.  This does not
-- alter the length of the list.
--
-- > maybe_latch 1 [Nothing,Just 2,Nothing,Just 4] == [1,2,2,4]
maybe_latch :: a -> [Maybe a] -> [a]
maybe_latch i x =
    case x of
      [] -> []
      Just e:x' -> e : maybe_latch e x'
      Nothing:x' -> i : maybe_latch i x'

-- | Variant requiring initial value is not 'Nothing'.
--
-- > maybe_latch1 [Just 1,Nothing,Nothing,Just 4] == [1,1,1,4]
maybe_latch1 :: [Maybe a] -> [a]
maybe_latch1 = maybe_latch (error "maybe_latch1")

-- | 'map' of 'fmap'.
--
-- > maybe_map negate [Nothing,Just 2] == [Nothing,Just (-2)]
maybe_map :: (a -> b) -> [Maybe a] -> [Maybe b]
maybe_map = map . fmap

-- | If either is 'Nothing' then 'False', else /eq/ of values.
maybe_eq_by :: (t -> u -> Bool) -> Maybe t -> Maybe u -> Bool
maybe_eq_by eq_fn p q =
    case (p,q) of
      (Just p',Just q') -> eq_fn p' q'
      _ -> False

-- | Join two values, either of which may be missing.
maybe_join' :: (s -> t) -> (s -> s -> t) -> Maybe s -> Maybe s -> Maybe t
maybe_join' f g p q =
    case (p,q) of
      (Nothing,_) -> fmap f q
      (_,Nothing) -> fmap f p
      (Just p',Just q') -> Just (p' `g` q')

-- | 'maybe_join'' of 'id'
maybe_join :: (t -> t -> t) -> Maybe t -> Maybe t -> Maybe t
maybe_join = maybe_join' id

-- | Apply predicate inside 'Maybe'.
--
-- > maybe_predicate even (Just 3) == Nothing
maybe_predicate :: (a -> Bool) -> Maybe a -> Maybe a
maybe_predicate f i =
    case i of
      Nothing -> Nothing
      Just j -> if f j then Just j else Nothing

-- | 'map' of 'maybe_predicate'.
--
-- > let r = [Nothing,Nothing,Nothing,Just 4]
-- > in maybe_filter even [Just 1,Nothing,Nothing,Just 4] == r
maybe_filter :: (a -> Bool) -> [Maybe a] -> [Maybe a]
maybe_filter = map . maybe_predicate

