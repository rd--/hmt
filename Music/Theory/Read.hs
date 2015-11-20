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

-- | Variant of 'read_maybe' with default value.
--
-- > map (read_def 0) ["2","2:","2\n"] == [2,0,2]
read_def :: Read a => a -> String -> a
read_def x s = maybe x id (read_maybe s)

-- | Variant of 'read_maybe' that errors on 'Nothing'.
read_err :: Read a => String -> a
read_err = maybe (error "read_err") id . read_maybe

-- * Plain type specialisations

-- | Type specialised variant.
--
-- > map read_maybe_int ["2","2:","2\n"] == [Just 2,Nothing,Just 2]
read_maybe_int :: String -> Maybe Int
read_maybe_int = read_maybe

-- | Type specialised variant.
read_int :: String -> Int
read_int = read

-- | Type specialised variant.
read_maybe_double :: String -> Maybe Double
read_maybe_double = read_maybe

-- | Type specialised variant.
read_double :: String -> Double
read_double = read

-- | Type specialised variant.
--
-- > map read_maybe_rational ["1","1%2","1/2"] == [Nothing,Just (1/2),Nothing]
read_maybe_rational :: String -> Maybe Rational
read_maybe_rational = read_maybe

-- | Type specialised variant.
--
-- > read_rational "1%4"
read_rational :: String -> Rational
read_rational = read
