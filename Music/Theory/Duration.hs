module Music.Theory.Duration where

data Duration = Duration { division :: Integer
                         , dots :: Integer
                         , multiplier :: Rational }
                  deriving (Eq, Show)

-- | Compare durations.
dur_cmp :: Duration -> Duration -> Ordering
dur_cmp y0 y1 =
    if y0 == y1
    then EQ
    else let (Duration x0 n0 m0) = y0
             (Duration x1 n1 m1) = y1
         in if m0 /= m1
            then error "dur_cmp: non-equal multipliers"
            else if x0 == x1
                 then compare n0 n1
                 else compare x1 x0

{-
zipWith dur_cmp [e,e,e,e'] [e,s,q,e]
-}

sort_pair :: (t -> t -> Ordering) -> (t, t) -> (t, t)
sort_pair fn (x,y) =
    case fn x y of
      LT -> (x,y)
      EQ -> (x,y)
      GT -> (y,x)

-- | True if neither duration is dotted.
no_dots :: (Duration, Duration) -> Bool
no_dots (x0,x1) = dots x0 == 0 && dots x1 == 0

-- | Sum undotted divisions, input is required to be sorted.
sum_dur_undotted :: (Integer, Integer) -> Maybe Duration
sum_dur_undotted (x0, x1)
    | x0 == x1 = Just (Duration (x0 `div` 2) 0 1)
    | x0 == x1 * 2 = Just (Duration x1 1 1)
    | otherwise = Nothing

-- | Sum dotted divisions, input is required to be sorted.
sum_dur_dotted :: (Integer,Integer,Integer,Integer) -> Maybe Duration
sum_dur_dotted (x0, n0, x1, n1)
    | x0 == x1 &&
      n0 == 1 &&
      n1 == 1 = Just (Duration (x1 `div` 2) 1 1)
    | x0 == x1 * 2 &&
      n0 == 0 &&
      n1 == 1 = Just (Duration (x1 `div` 2) 0 1)
    | otherwise = Nothing

-- | Sum durations.  Not all durations can be summed, and the present
--   algorithm is not exhaustive.
sum_dur :: Duration -> Duration -> Maybe Duration
sum_dur y0 y1 =
    let (x0,x1) = sort_pair dur_cmp (y0,y1)
    in if no_dots (x0,x1)
       then sum_dur_undotted (division x0, division x1)
       else sum_dur_dotted (division x0, dots x0
                           ,division x1, dots x1)

sum_dur' :: Duration -> Duration -> Duration
sum_dur' y0 y1 =
    let y2 = sum_dur y0 y1
        err = error ("sum_dur': " ++ show (y0,y1))
    in maybe err id y2

{-
zipWith sum_dur [e,q,q'] [e,e,e]
-}
