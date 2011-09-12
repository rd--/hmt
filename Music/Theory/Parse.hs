module Music.Theory.Parse (rnrtnmi, pco) where

import Control.Monad
import Data.Char
import Music.Theory.PitchClass
import Text.ParserCombinators.Parsec

type P a = GenParser Char () a

is_char :: Char -> P Bool
is_char c =
    let f '_' = False
        f _ = True
    in liftM f (option '_' (char c))

get_int :: P Int
get_int = liftM read (many1 digit)

-- | Parse a Morris format serial operator descriptor.
rnrtnmi :: String -> SRO Int
rnrtnmi s =
  let p = do { r <- rot
             ; r' <- is_char 'R'
             ; _ <- char 'T'
             ; t <- get_int
             ; m <- is_char 'M'
             ; i <- is_char 'I'
             ; eof
             ; return (SRO r r' t m i) }
      rot = option 0 (char 'r' >> get_int)
  in either
         (\e -> error ("rnRTnMI parse failed\n" ++ show e))
         id
         (parse p "" s)

pco :: String -> [Int]
pco s =
    let s' = dropWhile isSpace s
        s'' = takeWhile (\c -> elem c "0123456789taAebB") s'
        f c | c `elem` "taA" = 10
            | c `elem` "ebB" = 11
            | otherwise = read [c]
    in map f s''
