-- | Enumeration functions.
module Music.Theory.Enum where

import Data.List {- base -}

-- | Generic variant of 'fromEnum' (p.263).
genericFromEnum :: (Integral i,Enum e) => e -> i
genericFromEnum = fromIntegral . fromEnum

-- | Generic variant of 'toEnum' (p.263).
genericToEnum :: (Integral i,Enum e) => i -> e
genericToEnum = toEnum . fromIntegral

-- | Variant of 'enumFromTo' that, if /p/ is after /q/, cycles from
-- 'maxBound' to 'minBound'.
--
-- > import Data.Word
-- > enum_from_to_cyclic (254 :: Word8) 1 == [254,255,0,1]
enum_from_to_cyclic :: (Bounded a, Enum a) => a -> a -> [a]
enum_from_to_cyclic p q =
    if fromEnum p > fromEnum q
    then [p .. maxBound] ++ [minBound .. q]
    else [p .. q]

-- | Variant of 'enumFromTo' that, if /p/ is after /q/, enumerates
-- from /q/ to /p/.
--
-- > enum_from_to_reverse 5 1 == [5,4,3,2,1]
-- > enum_from_to_reverse 1 5 == enumFromTo 1 5
enum_from_to_reverse :: Enum a => a -> a -> [a]
enum_from_to_reverse p q =
    if fromEnum p > fromEnum q
    then reverse [q .. p]
    else [p .. q]

-- | All elements in sequence.
--
-- > (enum_univ :: [Data.Word.Word8]) == [0 .. 255]
enum_univ :: (Bounded t,Enum t) => [t]
enum_univ = [minBound .. maxBound]

-- | List of 'Enum' values not in sorted input list.
--
-- > enum_list_gaps "abdh" == "cefg"
enum_list_gaps :: (Enum t,Eq t) => [t] -> [t]
enum_list_gaps l =
  let e0 = head l
      eN = last l
      f x = x `notElem` l
  in filter f [e0 .. eN]

-- | 'enum_list_gaps' of 'sort'
enum_set_gaps :: (Enum t,Eq t,Ord t) => [t] -> [t]
enum_set_gaps = enum_list_gaps . sort
