-- | Show functions.
module Music.Theory.Show where

import Data.Char {- base -}
import Data.Maybe {- base -}
import Numeric {- base -}

-- | Given /n/ in (0,255) make two character hex string.
--
-- > mapMaybe byte_hex_pp [0x0F,0xF0,0xF0F] == ["0F","F0"]
byte_hex_pp :: (Integral i, Show i) => i -> Maybe String
byte_hex_pp n =
    case showHex n "" of
      [c] -> Just ['0',toUpper c]
      [c,d] -> Just (map toUpper [c,d])
      _ -> Nothing

-- | Erroring variant.
byte_hex_pp_err :: (Integral i, Show i) => i -> String
byte_hex_pp_err = fromMaybe (error "byte_hex_pp") . byte_hex_pp

-- | 'unwords' of 'map' of 'byte_hex_pp_err'.
--
-- > byte_seq_hex_pp [0x0F,0xF0] == "0F F0"
byte_seq_hex_pp :: (Integral i, Show i) => [i] -> String
byte_seq_hex_pp = unwords . map byte_hex_pp_err
