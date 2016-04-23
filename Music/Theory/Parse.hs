module Music.Theory.Parse where

import Control.Monad {- base -}

import Text.ParserCombinators.Parsec {- parsec -}

-- | A 'Char' parser.
type P a = GenParser Char () a

-- | Boolean 'P' for given 'Char'.
is_char :: Char -> P Bool
is_char c =
    let f '_' = False
        f _ = True
    in liftM f (option '_' (char c))

-- | Parse 'Integral'.
get_int :: Integral i => P i
get_int = liftM (fromInteger . read) (many1 digit)
