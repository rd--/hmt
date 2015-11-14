-- | Enumeration functions.
module Music.Theory.Enum where

-- | Variant of 'enumFromTo' that, if /p/ is after /q/, cycles from
-- 'maxBound' to 'minBound'.
--
-- > import Data.Word
-- > enum_from_to_cyclic (254 :: Word8) 1 == [254,255,0,1]
enum_from_to_cyclic :: (Bounded a, Enum a) => a -> a -> [a]
enum_from_to_cyclic p q =
    let p' = fromEnum p
        q' = fromEnum q
    in if p' > q'
       then [p .. maxBound] ++ [minBound .. q]
       else [p .. q]
