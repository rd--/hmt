module Music.Theory.Time.Notation where

import Text.Printf {- base -}

-- | Fractional seconds.
type FSEC = Double

-- | Minutes, seconds as @(min,sec)@
type MINSEC = (Int,Int)

-- | Minutes, seconds, centi-seconds as @(min,sec,csec)@
type MINCSEC = (Int,Int,Int)

-- | Fractional seconds to @(min,sec)@.
--
-- > map fsec_to_minsec [59.49,60,60.51] == [(0,59),(1,0),(1,1)]
fsec_to_minsec :: FSEC -> MINSEC
fsec_to_minsec tm = round tm `divMod` 60

-- | 'MINSEC' pretty printer.
--
-- > map (minsec_pp . fsec_to_minsec) [59,61] == ["00:59","01:01"]
minsec_pp :: MINSEC -> String
minsec_pp (m,s) = printf "%02d:%02d" m s

-- | Fractional seconds to @(min,sec,csec)@.
--
-- > map fsec_to_mincsec [1,1.5,4/3] == [(0,1,0),(0,1,50),(0,1,33)]
fsec_to_mincsec :: FSEC -> MINCSEC
fsec_to_mincsec tm =
    let tm' = floor tm
        (m,s) = tm' `divMod` 60
        cs = round ((tm - fromIntegral tm') * 100)
    in (m,s,cs)

-- | 'MINCSEC' pretty printer.
--
-- > map (mincsec_pp . fsec_to_mincsec) [1,4/3] == ["00:01.00","00:01.33"]
mincsec_pp :: MINCSEC -> String
mincsec_pp (m,s,cs) = printf "%02d:%02d.%02d" m s cs

span_pp :: (t -> String) -> (t,t) -> String
span_pp f (t1,t2) = concat [f t1," - ",f t2]
