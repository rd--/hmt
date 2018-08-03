-- | 'Ordering' functions
module Music.Theory.Ord where

-- | Minimum by /f/.
min_by :: Ord a => (t -> a) -> t -> t -> t
min_by f p q = if f p <= f q then p else q

-- | Specialised 'fromEnum'.
ord_to_int :: Ordering -> Int
ord_to_int = fromEnum

-- | Specialised 'toEnum'.
int_to_ord :: Int -> Ordering
int_to_ord = toEnum

-- | Invert 'Ordering'.
--
-- > map ord_invert [LT,EQ,GT] == [GT,EQ,LT]
ord_invert :: Ordering -> Ordering
ord_invert x =
    case x of
      LT -> GT
      EQ -> EQ
      GT -> LT

-- | Given 'Ordering', re-order pair,
order_pair :: Ordering -> (t,t) -> (t,t)
order_pair o (x,y) =
    case o of
      LT -> (x,y)
      EQ -> (x,y)
      GT -> (y,x)

-- | Sort a pair of equal type values using given comparison function.
--
-- > sort_pair compare ('b','a') == ('a','b')
sort_pair :: (t -> t -> Ordering) -> (t,t) -> (t,t)
sort_pair fn (x,y) = order_pair (fn x y) (x,y)

-- | Variant where the comparison function may not compute a value.
sort_pair_m :: (t -> t -> Maybe Ordering) -> (t,t) -> Maybe (t,t)
sort_pair_m fn (x,y) = fmap (`order_pair` (x,y)) (fn x y)
