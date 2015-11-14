-- | Enumeration functions.
module Music.Theory.Enum where

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
