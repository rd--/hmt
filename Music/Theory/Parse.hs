-- | Parsing utilities
module Music.Theory.Parse where

import Data.Maybe {- base -}

import qualified Text.Parsec as P {- parsec -}
import qualified Text.Parsec.String as String {- parsec -}

-- | A 'Char' parser with no user state.
type P a = String.GenParser Char () a

-- | Boolean 'P' for given 'Char'.
is_char :: Char -> P Bool
is_char = fmap isJust . P.optionMaybe . P.char

-- | Parse 'Integral'.
parse_int :: Integral i => P i
parse_int = fmap (fromInteger . read) (P.many1 P.digit)

run_parser :: P t -> String -> Either P.ParseError t
run_parser p = P.runParser p () ""

run_parser_maybe :: P t -> String -> Maybe t
run_parser_maybe p = either (const Nothing) Just . run_parser p

run_parser_error :: P c -> String -> c
run_parser_error p = either (error . show) id . run_parser p
