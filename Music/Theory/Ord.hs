-- | 'Ordering' functions
module Music.Theory.Ord where

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

