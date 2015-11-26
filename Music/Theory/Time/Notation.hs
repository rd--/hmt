module Music.Theory.Time.Notation where

import Data.List.Split {- split -}
import Text.Printf {- base -}

-- | Fractional seconds.
type FSEC = Double

-- | Minutes, seconds as @(min,sec)@
type MinSec n = (n,n)

-- | Type specialised.
type MINSEC = (Int,Int)

-- | Minutes, seconds, centi-seconds as @(min,sec,csec)@
type MinCsec n = (n,n,n)

-- | Type specialised.
type MINCSEC = (Int,Int,Int)

-- | 'divMod' by @60@.
--
-- > sec_minsec 123 == (2,3)
sec_minsec :: Integral n => n -> MinSec n
sec_minsec = flip divMod 60

-- | Inverse of 'sec_minsec'.
--
-- > minsec_sec (2,3) == 123
minsec_sec :: Num n => MinSec n -> n
minsec_sec (m,s) = m * 60 + s

-- | Difference, assumes /p/ precedes /q/.
--
-- > minsec_diff (1,59) (2,35) == (0,36)
minsec_diff :: Integral n => MinSec n -> MinSec n -> MinSec n
minsec_diff p q = sec_minsec (minsec_sec q - minsec_sec p)

-- | Fractional seconds to @(min,sec)@.
--
-- > map fsec_minsec [59.49,60,60.51] == [(0,59),(1,0),(1,1)]
fsec_minsec :: FSEC -> MINSEC
fsec_minsec = sec_minsec . round

-- | 'MINSEC' pretty printer.
--
-- > map (minsec_pp . fsec_minsec) [59,61] == ["00:59","01:01"]
minsec_pp :: MINSEC -> String
minsec_pp (m,s) = printf "%02d:%02d" m s

-- * 'MinSec' parser.
minsec_parse :: (Num n,Read n) => String -> MinSec n
minsec_parse x =
    case splitOn ":" x of
      [m,s] -> (read m,read s)
      _ -> error "parse_minsec"

-- | Fractional seconds to @(min,sec,csec)@.
--
-- > map fsec_mincsec [1,1.5,4/3] == [(0,1,0),(0,1,50),(0,1,33)]
fsec_mincsec :: FSEC -> MINCSEC
fsec_mincsec tm =
    let tm' = floor tm
        (m,s) = sec_minsec tm'
        cs = round ((tm - fromIntegral tm') * 100)
    in (m,s,cs)

-- | Inverse of 'fsec_mincsec'.
mincsec_fsec :: Real n => MinCsec n -> FSEC
mincsec_fsec (m,s,cs) = realToFrac m * 60 + realToFrac s + (realToFrac cs / 100)

-- | 'MINCSEC' pretty printer.
--
-- > map (mincsec_pp . fsec_mincsec) [1,4/3] == ["00:01.00","00:01.33"]
mincsec_pp :: MINCSEC -> String
mincsec_pp (m,s,cs) = printf "%02d:%02d.%02d" m s cs

-- | Given printer, pretty print time span.
span_pp :: (t -> String) -> (t,t) -> String
span_pp f (t1,t2) = concat [f t1," - ",f t2]
