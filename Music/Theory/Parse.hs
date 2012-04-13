-- | Parsers for pitch class sets and sequences, and for 'SRO's.
module Music.Theory.Parse (rnrtnmi,pco) where

import Control.Monad
import Data.Char
import Music.Theory.SRO
import Music.Theory.Z12
import Text.ParserCombinators.Parsec

-- | A 'Char' parser.
type P a = GenParser Char () a

-- | Boolean 'P' for given 'Char'.
is_char :: Char -> P Bool
is_char c =
    let f '_' = False
        f _ = True
    in liftM f (option '_' (char c))

-- | Parse 'Int'.
get_int :: P Z12
get_int = liftM (fromInteger . read) (many1 digit)

-- | Parse a Morris format serial operator descriptor.
--
-- > rnrtnmi "r2RT3MI" == SRO 2 True 3 True True
rnrtnmi :: String -> SRO
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

-- | Parse a /pitch class object/ string.  Each 'Char' is either a
-- number, a space which is ignored, or a letter name for the numbers
-- 10 ('t' or 'a' or 'A') or 11 ('e' or 'B' or 'b').
--
-- > pco "13te" == [1,3,10,11]
-- > pco "13te" == pco "13ab"
pco :: String -> [Z12]
pco s =
    let s' = dropWhile isSpace s
        s'' = takeWhile (`elem` "0123456789taAebB") s'
        f c | c `elem` "taA" = 10
            | c `elem` "ebB" = 11
            | otherwise = fromInteger (read [c])
    in map f s''
