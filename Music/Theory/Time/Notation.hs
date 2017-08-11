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
-- > sec_to_minsec 123 == (2,3)
sec_to_minsec :: Integral n => n -> MinSec n
sec_to_minsec = flip divMod 60

-- | Inverse of 'sec_minsec'.
--
-- > minsec_to_sec (2,3) == 123
minsec_to_sec :: Num n => MinSec n -> n
minsec_to_sec (m,s) = m * 60 + s

minsec_binop :: Integral t => (t -> t -> t) -> MinSec t -> MinSec t -> MinSec t
minsec_binop f p q = sec_to_minsec (f (minsec_to_sec p) (minsec_to_sec q))

-- | 'minsec_binop' '-', assumes /q/ precedes /p/.
--
-- > minsec_sub (2,35) (1,59) == (0,36)
minsec_sub :: Integral n => MinSec n -> MinSec n -> MinSec n
minsec_sub = minsec_binop (-)

-- | 'minsec_binop' 'subtract', assumes /p/ precedes /q/.
--
-- > minsec_diff (1,59) (2,35) == (0,36)
minsec_diff :: Integral n => MinSec n -> MinSec n -> MinSec n
minsec_diff = minsec_binop subtract

-- | 'minsec_binop' '+'.
--
-- > minsec_add (1,59) (2,35) == (4,34)
minsec_add :: Integral n => MinSec n -> MinSec n -> MinSec n
minsec_add = minsec_binop (+)

-- | 'foldl' of 'minsec_add'
--
-- > minsec_sum [(1,59),(2,35),(4,34)] == (9,08)
minsec_sum :: Integral n => [MinSec n] -> MinSec n
minsec_sum = foldl minsec_add (0,0)

-- | Fractional seconds to @(min,sec)@.
--
-- > map fsec_to_minsec [59.49,60,60.51] == [(0,59),(1,0),(1,1)]
fsec_to_minsec :: FSEC -> MINSEC
fsec_to_minsec = sec_to_minsec . round

-- | 'MINSEC' pretty printer.
--
-- > map (minsec_pp . fsec_to_minsec) [59,61] == ["00:59","01:01"]
minsec_pp :: MINSEC -> String
minsec_pp (m,s) = printf "%02d:%02d" m s

-- * 'MinSec' parser.
minsec_parse :: (Num n,Read n) => String -> MinSec n
minsec_parse x =
    case splitOn ":" x of
      [m,s] -> (read m,read s)
      _ -> error "parse_minsec"

-- | Fractional seconds to @(min,sec,csec)@, csec value is 'round'ed.
--
-- > map fsec_to_mincsec [1,1.5,4/3] == [(0,1,0),(0,1,50),(0,1,33)]
fsec_to_mincsec :: FSEC -> MINCSEC
fsec_to_mincsec tm =
    let tm' = floor tm
        (m,s) = sec_to_minsec tm'
        cs = round ((tm - fromIntegral tm') * 100)
    in (m,s,cs)

-- | Inverse of 'fsec_mincsec'.
mincsec_to_fsec :: Real n => MinCsec n -> FSEC
mincsec_to_fsec (m,s,cs) = realToFrac m * 60 + realToFrac s + (realToFrac cs / 100)

-- > map (mincsec_to_csec . fsec_to_mincsec) [1,6+2/3,123.45] == [100,667,12345]
mincsec_to_csec :: Num n => MinCsec n -> n
mincsec_to_csec (m,s,cs) = m * 60 * 100 + s * 100 + cs

-- | Centi-seconds to 'MinCsec'.
--
-- > map csec_to_mincsec [123,12345] == [(0,1,23),(2,3,45)]
csec_to_mincsec :: Integral n => n -> MinCsec n
csec_to_mincsec csec =
    let (m,cs) = csec `divMod` 6000
        (s,cs') = cs `divMod` 100
    in (m,s,cs')

-- | 'MINCSEC' pretty printer.
--
-- > map (mincsec_pp . fsec_to_mincsec) [1,6+2/3,123.45] == ["00:01.00","00:06.67","02:03.45"]
mincsec_pp :: MINCSEC -> String
mincsec_pp (m,s,cs) = printf "%02d:%02d.%02d" m s cs

mincsec_binop :: Integral t => (t -> t -> t) -> MinCsec t -> MinCsec t -> MinCsec t
mincsec_binop f p q = csec_to_mincsec (f (mincsec_to_csec p) (mincsec_to_csec q))

-- | Given printer, pretty print time span.
span_pp :: (t -> String) -> (t,t) -> String
span_pp f (t1,t2) = concat [f t1," - ",f t2]
