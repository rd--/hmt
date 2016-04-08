-- | Read functions.
module Music.Theory.Read where

import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}
import Numeric {- base -}

-- | Transform 'ReadS' function into precise 'Read' function.
-- Requires using all the input to produce a single token.  The only
-- exception is a singular trailing white space character.
reads_to_read_precise :: ReadS t -> (String -> Maybe t)
reads_to_read_precise f s =
    case f s of
      [(r,[])] -> Just r
      [(r,[c])] -> if isSpace c then Just r else Nothing
      _ -> Nothing

-- | Error variant of 'reads_to_read_precise'.
reads_to_read_precise_err :: String -> ReadS t -> String -> t
reads_to_read_precise_err err f =
    fromMaybe (error ("reads_to_read_precise_err:" ++ err)) .
    reads_to_read_precise f

-- | 'reads_to_read_precise' of 'reads'.
-- space character.
read_maybe :: Read a => String -> Maybe a
read_maybe = reads_to_read_precise reads

-- | Variant of 'read_maybe' with default value.
--
-- > map (read_def 0) ["2","2:","2\n"] == [2,0,2]
read_def :: Read a => a -> String -> a
read_def x s = maybe x id (read_maybe s)

-- | Variant of 'read_maybe' that errors on 'Nothing'.
read_err :: Read a => String -> a
read_err s = maybe (error ("read_err: " ++ s)) id (read_maybe s)

-- * Type specific variants

-- | Allow commas as thousand separators.
--
-- > let r = [Just 123456,Just 123456,Nothing,Just 123456789]
-- > in map read_integral_allow_commas_maybe ["123456","123,456","1234,56","123,456,789"]
read_integral_allow_commas_maybe :: (Integral i,Read i) => String -> Maybe i
read_integral_allow_commas_maybe s =
    let c = filter ((== ',') . fst) (zip (reverse s) [0..])
    in if null c
       then read_maybe s
       else if map snd c `isPrefixOf` [3::Int,7..]
            then read_maybe (filter (not . (== ',')) s)
            else Nothing

read_integral_allow_commas_err :: (Integral i,Read i) => String -> i
read_integral_allow_commas_err s =
    let err = error ("read_integral_allow_commas: misplaced commas: " ++ s)
    in fromMaybe err (read_integral_allow_commas_maybe s)

read_int_allow_commas :: String -> Int
read_int_allow_commas = read_integral_allow_commas_err

-- | Read a ratio where the division is given by @/@ instead of @%@
-- and the integers allow commas.
--
-- > map read_ratio_with_div_err ["123,456/7","123,456,789"] == [123456/7,123456789]
read_ratio_with_div_err :: (Integral i, Read i) => String -> Ratio i
read_ratio_with_div_err s =
    let f = read_integral_allow_commas_err
    in case break (== '/') s of
         (n,'/':d) -> f n % f d
         _ -> read_integral_allow_commas_err s % 1

-- | Read 'Ratio', allow commas for thousand separators.
--
-- > read_ratio_allow_commas_err "327,680" "177,147" == 327680 / 177147
read_ratio_allow_commas_err :: (Integral i,Read i) => String -> String -> Ratio i
read_ratio_allow_commas_err n d = let f = read_integral_allow_commas_err in f n % f d

-- | Delete trailing @.@, 'read' fails for @700.@.
delete_trailing_point :: String -> String
delete_trailing_point s =
    case reverse s of
      '.':s' -> reverse s'
      _ -> s

-- | 'read_err' disallows trailing decimal points.
--
-- > map read_fractional_allow_trailing_point_err ["123.","123.4"] == [123.0,123.4]
read_fractional_allow_trailing_point_err :: (Fractional n,Read n) => String -> n
read_fractional_allow_trailing_point_err = read_err . delete_trailing_point

-- * Plain type specialisations

-- | Type specialised 'read_maybe'.
--
-- > map read_maybe_int ["2","2:","2\n"] == [Just 2,Nothing,Just 2]
read_maybe_int :: String -> Maybe Int
read_maybe_int = read_maybe

-- | Type specialised 'read_err'.
read_int :: String -> Int
read_int = read_err

-- | Type specialised 'read_maybe'.
read_maybe_double :: String -> Maybe Double
read_maybe_double = read_maybe

-- | Type specialised 'read_err'.
read_double :: String -> Double
read_double = read_err

-- | Type specialised 'read_maybe'.
--
-- > map read_maybe_rational ["1","1%2","1/2"] == [Nothing,Just (1/2),Nothing]
read_maybe_rational :: String -> Maybe Rational
read_maybe_rational = read_maybe

-- | Type specialised 'read_err'.
--
-- > read_rational "1%4"
read_rational :: String -> Rational
read_rational = read_err

-- * Numeric variants

-- | Error variant of 'readHex'.
--
-- > read_hex_err "F0B0" == 61616
read_hex_err :: (Eq n,Num n) => String -> n
read_hex_err = reads_to_read_precise_err "readHex" readHex
