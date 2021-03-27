-- | Read functions.
module Music.Theory.Read where

import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}
import Data.Word {- base -}
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
--
-- > read_maybe "1.0" :: Maybe Int
-- > read_maybe "1.0" :: Maybe Float
read_maybe :: Read a => String -> Maybe a
read_maybe = reads_to_read_precise reads

-- | Variant of 'read_maybe' with default value.
--
-- > map (read_def 0) ["2","2:","2\n"] == [2,0,2]
read_def :: Read a => a -> String -> a
read_def x s = fromMaybe x (read_maybe s)

-- | Variant of 'read_maybe' that errors on 'Nothing', printing message.
read_err_msg :: Read a => String -> String -> a
read_err_msg msg s = fromMaybe (error ("read_err: " ++ msg ++ ": " ++ s)) (read_maybe s)

-- | Default message.
read_err :: Read a => String -> a
read_err = read_err_msg "read_maybe failed"

-- | Variant of 'reads' requiring exact match, no trailing white space.
--
-- > map reads_exact ["1.5","2,5"] == [Just 1.5,Nothing]
reads_exact :: Read a => String -> Maybe a
reads_exact s =
    case reads s of
      [(r,"")] -> Just r
      _ -> Nothing

-- | Variant of 'reads_exact' that errors on failure.
reads_exact_err :: Read a => String -> String -> a
reads_exact_err err_txt str =
    let err = error ("reads: " ++ err_txt ++ ": " ++ str)
    in fromMaybe err (reads_exact str)

-- * Type specific variants

-- | Allow commas as thousand separators.
--
-- > let r = [Just 123456,Just 123456,Nothing,Just 123456789]
-- > map read_integral_allow_commas_maybe ["123456","123,456","1234,56","123,456,789"] == r
read_integral_allow_commas_maybe :: Read i => String -> Maybe i
read_integral_allow_commas_maybe s =
    let c = filter ((== ',') . fst) (zip (reverse s) [0..])
    in if null c
       then read_maybe s
       else if map snd c `isPrefixOf` [3::Int,7..]
            then read_maybe (filter (/= ',') s)
            else Nothing

read_integral_allow_commas_err :: (Integral i,Read i) => String -> i
read_integral_allow_commas_err s =
    let err = error ("read_integral_allow_commas: misplaced commas: " ++ s)
    in fromMaybe err (read_integral_allow_commas_maybe s)

-- | Type specialised.
--
-- > map read_int_allow_commas ["123456","123,456","123,456,789"] == [123456,123456,123456789]
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
read_fractional_allow_trailing_point_err :: Read n => String -> n
read_fractional_allow_trailing_point_err = read_err . delete_trailing_point

-- * Plain type specialisations

-- | Type specialised 'read_maybe'.
--
-- > map read_maybe_int ["2","2:","2\n","x"] == [Just 2,Nothing,Just 2,Nothing]
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

-- | Read binary integer.
--
-- > mapMaybe read_bin (words "000 001 010 011 100 101 110 111") == [0 .. 7]
read_bin :: Integral a => String -> Maybe a
read_bin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

-- | Erroring variant.
read_bin_err :: Integral a => String -> a
read_bin_err = fromMaybe (error "read_bin") . read_bin

-- * HEX

-- | Error variant of 'readHex'.
--
-- > read_hex_err "F0B0" == 61616
read_hex_err :: (Eq n,Num n) => String -> n
read_hex_err = reads_to_read_precise_err "readHex" readHex

-- | Read hex value from string of at most /k/ places.
read_hex_sz :: (Eq n, Num n) => Int -> String -> n
read_hex_sz k str =
  if length str > k
  then error "read_hex_sz? = > K"
  else case readHex str of
         [(r,[])] -> r
         _ -> error "read_hex_sz? = PARSE"

-- | Read hexadecimal representation of 32-bit unsigned word.
--
-- > map read_hex_word32 ["00000000","12345678","FFFFFFFF"] == [minBound,305419896,maxBound]
read_hex_word32 :: String -> Word32
read_hex_word32 = read_hex_sz 8

-- * RATIONAL

-- | Parser for 'rational_pp'.
--
-- > map rational_parse ["1","3/2","5/4","2"] == [1,3/2,5/4,2]
-- > rational_parse "" == undefined
rational_parse :: (Read t,Integral t) => String -> Ratio t
rational_parse s =
  case break (== '/') s of
    ([],_) -> error "rational_parse"
    (n,[]) -> read n % 1
    (n,_:d) -> read n % read d

