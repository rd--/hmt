module Music.Theory.Parse where

import Data.Maybe {- base -}

import qualified Text.Parsec as P {- parsec -}
import qualified Text.Parsec.String as P {- parsec -}

-- | A 'Char' parser.
type P a = P.GenParser Char () a

-- | Boolean 'P' for given 'Char'.
is_char :: Char -> P Bool
is_char = fmap isJust . P.optionMaybe . P.char

-- | Parse 'Integral'.
parse_int :: Integral i => P i
parse_int = fmap (fromInteger . read) (P.many1 P.digit)
