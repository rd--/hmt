-- | Read functions.
module Music.Theory.Read where

import Data.Char {- base -}

-- | 'reads' variant that requires using all the input to produce a
-- single token.  The only exception is a singular trailing white
-- space character.
read_maybe :: Read a => String -> Maybe a
read_maybe s =
    case reads s of
      [(x,[])] -> Just x
      [(x,[c])] -> if isSpace c then Just x else Nothing
      _ -> Nothing

-- | Type specialised variant.
--
-- > map read_maybe_int ["2","2:","2\n"] == [Just 2,Nothing,Just 2]
read_maybe_int :: String -> Maybe Int
read_maybe_int = read_maybe

-- | Type specialised variant.
read_maybe_double :: String -> Maybe Double
read_maybe_double = read_maybe

-- | Variant of 'read_maybe' with default value.
--
-- > map (read_def 0) ["2","2:","2\n"] == [2,0,2]
read_def :: Read a => a -> String -> a
read_def x s = maybe x id (read_maybe s)
