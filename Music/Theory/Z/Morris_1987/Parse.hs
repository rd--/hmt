-- | Parsers for pitch class sets and sequences, and for 'SRO's.
module Music.Theory.Z.Morris_1987.Parse where

import Data.Char {- base -}

-- | Parse a /pitch class object/ string.  Each 'Char' is either a
-- number, a space which is ignored, or a letter name for the numbers
-- 10 ('t' or 'a' or 'A') or 11 ('e' or 'B' or 'b').
--
-- > pco "13te" == [1,3,10,11]
-- > pco "13te" == pco "13ab"
pco :: Num n => String -> [n]
pco s =
    let s' = dropWhile isSpace s
        s'' = takeWhile (`elem` "0123456789taAebB") s'
        f c | c `elem` "taA" = 10
            | c `elem` "ebB" = 11
            | otherwise = fromInteger (read [c])
    in map f s''
