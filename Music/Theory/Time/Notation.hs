module Music.Theory.Time.Notation where

import Data.List.Split {- split -}
import qualified Data.Time as T {- time -}
import Text.Printf {- base -}

-- * TYPES

type BinOp t = t -> t -> t

type WEEK = Int -- (1-52)
type DAY = Int
type HOUR = Int -- (0-23)
type MIN = Int -- (0-59)
type SEC = Int -- (0-59)
type CSEC = Int -- (0-99)

-- | Fractional days.
type FDAY = Double

-- | Fractional hour (1.50 is one and a half hours).
type FHOUR = Double

-- | Fractional seconds.
type FSEC = Double

-- | Minutes, seconds as @(min,sec)@
type MinSec n = (n,n)

-- | Type specialised.
type MINSEC = (MIN,SEC)

-- | Fractional minutes and seconds (mm.ss, ie. 01.45 is 1 minute and 45 seconds).
type FMINSEC = Double

-- | Minutes, seconds, centi-seconds as @(min,sec,csec)@
type MinCsec n = (n,n,n)

-- | Type specialised.
type MINCSEC = (MIN,SEC,CSEC)

-- | (Hours,Minutes,Seconds)
type HMS = (HOUR,MIN,SEC)

-- | (Days,Hours,Minutes,Seconds)
type DHMS = (DAY,HOUR,MIN,SEC)

-- * T.UTCTime format strings.

parse_time_str :: String -> String -> T.UTCTime
parse_time_str = T.parseTimeOrError True T.defaultTimeLocale

format_time_str :: String -> T.UTCTime -> String
format_time_str = T.formatTime T.defaultTimeLocale

-- * ISO-8601

-- | Parse date in ISO-8601 (@Y-m-d@) form.
--
-- > T.toGregorian (T.utctDay (parse_iso8601_date "2011-10-09")) == (2011,10,09)
parse_iso8601_date :: String -> T.UTCTime
parse_iso8601_date = parse_time_str "%F"

-- | Format date in ISO-8601 (@Y-m-d@) form.
--
-- > format_iso8601_date (parse_iso8601_date "2011-10-09") == "2011-10-09"
format_iso8601_date :: T.UTCTime -> String
format_iso8601_date = format_time_str "%F"

{- | Format date in ISO-8601 (@Y-W@) form.

> r = ["2016-W52","2011-W40"]
> map (format_iso8601_week . parse_iso8601_date) ["2017-01-01","2011-10-09"] == r

-}
format_iso8601_week :: T.UTCTime -> String
format_iso8601_week = format_time_str "%G-W%V"

-- | Parse ISO-8601 @H:M:S@ time.
--
-- > format_iso8601_time (parse_iso8601_time "21:44:00") == "21:44:00"
parse_iso8601_time :: String -> T.UTCTime
parse_iso8601_time = parse_time_str "%H:%M:%S"

-- | Format time in @H:M:S@ form.
--
-- > format_iso8601_time (parse_iso8601_date_time "2011-10-09T21:44:00") == "21:44:00"
format_iso8601_time :: T.UTCTime -> String
format_iso8601_time = format_time_str "%H:%M:%S"

-- | Parse date in @Y-m-d@ and time in @H:M:%S@ forms.
--
-- > T.utctDayTime (parse_iso8601_date_time "2011-10-09T21:44:00") == T.secondsToDiffTime 78240
parse_iso8601_date_time :: String -> T.UTCTime
parse_iso8601_date_time = parse_time_str "%FT%H:%M:%S"

{- | Format date in @Y-m-d@ and time in @H:M:S@ forms.

> t = parse_iso8601_date_time "2011-10-09T21:44:00"
> format_iso8601_date_time t == "2011-10-09T21:44:00"

-}
format_iso8601_date_time :: T.UTCTime -> String
format_iso8601_date_time = format_time_str "%FT%H:%M:%S"

-- * FSEC

-- > fsec_to_picoseconds 78240.05
fsec_to_picoseconds :: FSEC -> Integer
fsec_to_picoseconds s = floor (s * (10 ** 12))

fsec_to_difftime :: FSEC -> T.DiffTime
fsec_to_difftime = T.picosecondsToDiffTime . fsec_to_picoseconds

-- * FMINSEC

-- > map fminsec_to_fsec [0.45,15.355] == [45,935.5]
fminsec_to_fsec :: FMINSEC -> FSEC
fminsec_to_fsec n =
    let m = ffloor n
        s = (n - m) * 100
    in (m * 60) + s

fminsec_to_sec_generic :: (RealFrac f,Integral i) => f -> i
fminsec_to_sec_generic n =
    let m = floor n
        s = round ((n - fromIntegral m) * 100)
    in (m * 60) + s

-- | Fractional minutes are mm.ss, so that 15.35 is 15 minutes and 35 seconds.
--
-- > map fminsec_to_sec [0.45,15.35] == [45,935]
fminsec_to_sec :: FMINSEC -> SEC
fminsec_to_sec = fminsec_to_sec_generic

-- > fsec_to_fminsec 935.5 == 15.355
fsec_to_fminsec :: FSEC -> FMINSEC
fsec_to_fminsec n =
    let m = ffloor (n / 60)
        s = n - (m * 60)
    in m + (s / 100)

-- > sec_to_fminsec 935 == 15.35
sec_to_fminsec :: SEC -> FMINSEC
sec_to_fminsec n =
    let m = ffloor (fromIntegral n / 60)
        s = fromIntegral n - (m * 60)
    in m + (s / 100)

-- > fminsec_add 1.30 0.45 == 2.15
-- > fminsec_add 1.30 0.45 == 2.15
fminsec_add :: BinOp FMINSEC
fminsec_add p q = fsec_to_fminsec (fminsec_to_fsec p + fminsec_to_fsec q)

fminsec_sub :: BinOp FMINSEC
fminsec_sub p q = fsec_to_fminsec (fminsec_to_fsec p - fminsec_to_fsec q)

-- > fminsec_mul 0.45 2 == 1.30
fminsec_mul :: BinOp FMINSEC
fminsec_mul t n = fsec_to_fminsec (fminsec_to_fsec t * n)

-- * FHOUR

ffloor :: Double -> Double
ffloor = fromInteger . floor

-- | Fractional hour to (hours,minutes,seconds).
--
-- > fhour_to_hms 21.75 == (21,45,0)
fhour_to_hms :: FHOUR -> HMS
fhour_to_hms h =
    let m = (h - ffloor h) * 60
        s = (m - ffloor m) * 60
    in (floor h,floor m,round s)

-- > hms_to_fhour (21,45,0) == 21.75
hms_to_fhour :: HMS -> FHOUR
hms_to_fhour (h,m,s) = fromIntegral h + (fromIntegral m / 60) + (fromIntegral s / (60 * 60))

-- | Fractional hour to seconds.
--
-- > fhour_to_fsec 21.75 == 78300.0
fhour_to_fsec :: FHOUR -> FSEC
fhour_to_fsec = (*) (60 * 60)

fhour_to_difftime :: FHOUR -> T.DiffTime
fhour_to_difftime = fsec_to_difftime . fhour_to_fsec

hms_to_difftime :: HMS -> T.DiffTime
hms_to_difftime = fhour_to_difftime . hms_to_fhour

-- * FDAY

-- | Time in fractional days.
--
-- > round (utctime_to_fday (parse_iso8601_date_time "2011-10-09T09:00:00")) == 55843
-- > round (utctime_to_fday (parse_iso8601_date_time "2011-10-09T21:00:00")) == 55844
utctime_to_fday :: T.UTCTime -> FDAY
utctime_to_fday t =
    let d = T.utctDay t
        d' = fromIntegral (T.toModifiedJulianDay d)
        s = T.utctDayTime t
        s_max = 86401
    in d' + (fromRational (toRational s) / s_max)

-- * DiffTime

-- | 'T.DiffTime' in fractional seconds.
--
-- > difftime_to_fsec (hms_to_difftime (21,44,30)) == 78270
difftime_to_fsec :: T.DiffTime -> FSEC
difftime_to_fsec = fromRational . toRational

-- | 'T.DiffTime' in fractional minutes.
--
-- > difftime_to_fmin (hms_to_difftime (21,44,30)) == 1304.5
difftime_to_fmin :: T.DiffTime -> Double
difftime_to_fmin = (/ 60) . difftime_to_fsec

-- | 'T.DiffTime' in fractional hours.
--
-- > difftime_to_fhour (hms_to_difftime (21,45,00)) == 21.75
difftime_to_fhour :: T.DiffTime -> FHOUR
difftime_to_fhour = (/ 60) . difftime_to_fmin

-- * MINSEC

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

-- | Convert /p/ and /q/ to seconds, apply /f/, and convert back to 'MinSec'.
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

-- | 'round' fractional seconds to @(min,sec)@.
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

-- * MINCSEC

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

-- | 'MINCSEC' pretty printer, concise mode omits centiseconds when zero.
--
-- > map (mincsec_pp_opt True . fsec_to_mincsec) [1,60.5] == ["00:01","01:00.50"]
mincsec_pp_opt :: Bool -> MINCSEC -> String
mincsec_pp_opt concise (m,s,cs) =
  if concise && cs == 0
  then printf "%02d:%02d" m s
  else printf "%02d:%02d.%02d" m s cs

-- | 'MINCSEC' pretty printer.
--
-- > let r = ["00:01.00","00:06.67","02:03.45"]
-- > map (mincsec_pp . fsec_to_mincsec) [1,6+2/3,123.45] == r
mincsec_pp :: MINCSEC -> String
mincsec_pp = mincsec_pp_opt False

mincsec_binop :: Integral t => (t -> t -> t) -> MinCsec t -> MinCsec t -> MinCsec t
mincsec_binop f p q = csec_to_mincsec (f (mincsec_to_csec p) (mincsec_to_csec q))

-- * DHMS

sec_to_dhms_generic :: Integral n => n -> (n,n,n,n)
sec_to_dhms_generic n =
    let (d,h') = n `divMod` (24 * 60 * 60)
        (h,m') = h' `divMod` (60 * 60)
        (m,s) = m' `divMod` 60
    in (d,h,m,s)

-- | Convert seconds into (days,hours,minutes,seconds).
--
-- > sec_to_dhms 1475469 == (17,1,51,9)
sec_to_dhms :: SEC -> DHMS
sec_to_dhms = sec_to_dhms_generic

-- | Inverse of 'seconds_to_dhms'.
--
-- > dhms_to_sec (17,1,51,9) == 1475469
dhms_to_sec :: Num n => (n,n,n,n) -> n
dhms_to_sec (d,h,m,s) = sum [d * 24 * 60 * 60,h * 60 * 60,m * 60,s]

parse_dhms_generic :: (Integral n,Read n) => String -> (n,n,n,n)
parse_dhms_generic =
    let sep_elem = split . keepDelimsR . oneOf
        sep_last x = let e:x' = reverse x in (reverse x',e)
        p x = case sep_last x of
                (n,'d') -> read n * 24 * 60 * 60
                (n,'h') -> read n * 60 * 60
                (n,'m') -> read n * 60
                (n,'s') -> read n
                _ -> error "parse_dhms"
    in sec_to_dhms_generic . sum . map p . filter (not . null) . sep_elem "dhms"

-- | Parse DHMS text.  All parts are optional, order is not
-- significant, multiple entries are allowed.
--
-- > parse_dhms "17d1h51m9s" == (17,1,51,9)
-- > parse_dhms "1s3d" == (3,0,0,1)
-- > parse_dhms "1h1h" == (0,2,0,0)
parse_dhms :: String -> DHMS
parse_dhms = parse_dhms_generic

-- * WEEK

-- | Week that /t/ lies in.
--
-- > map (time_to_week . parse_iso8601_date) ["2017-01-01","2011-10-09"] == [52,40]
time_to_week :: T.UTCTime -> WEEK
time_to_week = read . format_time_str "%V"

-- * UTIL

-- | Given printer, pretty print time span.
span_pp :: (t -> String) -> (t,t) -> String
span_pp f (t1,t2) = concat [f t1," - ",f t2]
