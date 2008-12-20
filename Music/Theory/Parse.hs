module Music.Theory.Parse (rnrtnmi) where

import Control.Monad
import Music.Theory.Pitch
import Text.ParserCombinators.Parsec

type P a = GenParser Char () a

is_char :: Char -> P Bool
is_char c = liftM f (option '_' (char c))
  where f '_' = False
        f _ = True

get_int :: P Int
get_int = liftM read (many1 digit)

rnrtnmi :: String -> SRO Int
rnrtnmi s = either 
            (\e -> error ("rnRTnMI parse failed\n" ++ show e)) 
            id 
            (parse p "" s)
  where p = do { r <- rot
               ; r' <- is_char 'R'
               ; char 'T'
               ; t <- get_int
               ; m <- is_char 'M'
               ; i <- is_char 'I'
               ; eof
               ; return (SRO r r' t m i) }
        rot = option 0 (char 'r' >> get_int)
