-- | String functions.
module Music.Theory.String where

import Data.Char {- base -}

-- | Remove @\r@.
filter_cr :: String -> String
filter_cr = filter (not . (==) '\r')

-- | Delete trailing 'Char' where 'isSpace' holds.
--
-- > delete_trailing_whitespace "   str   " == "   str"
delete_trailing_whitespace :: String -> String
delete_trailing_whitespace = reverse . dropWhile isSpace . reverse

