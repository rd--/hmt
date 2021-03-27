-- | Anamark tuning (TUN) files
--
-- <https://www.mark-henning.de/files/am/Tuning_File_V2_Doc.pdf>
module Music.Theory.Tuning.Anamark where

import Text.Printf {- base -}

import qualified Music.Theory.List as T

-- | Format section string
tun_sec :: String -> String
tun_sec = printf "[%s]"

-- | Format 'String' (text) attribute
tun_attr_txt :: (String,String) -> String
tun_attr_txt (k,v) = printf "%s = \"%s\"" k v

-- | Format 'Int' attribute
tun_attr_int :: (String,Int) -> String
tun_attr_int (k,v) = printf "%s = %d" k v

-- | Format 'Double' attribute
tun_attr_real :: (String,Double) -> String
tun_attr_real (k,v) = printf "%s = %f" k v

-- | TUN V.200 /Scale Begin/ (header) section.
tun_begin :: [String]
tun_begin =
  [tun_sec "Scale Begin"
  ,tun_attr_txt ("Format","AnaMark-TUN")
  ,tun_attr_int ("FormatVersion",200)
  ,tun_attr_txt ("FormatSpecs","http://www.mark-henning.de/eternity/tuningspecs.html")]

-- | Format /Info/ section given Name and ID (the only required fields).
--
-- > tun_info ("name","id")
tun_info :: (String,String) -> [String]
tun_info (nm,k) =
  [tun_sec "Info"
  ,tun_attr_txt ("Name",nm)
  ,tun_attr_txt ("ID",k)]

-- | Format /Tuning/ section given sequence of 128 integral cents values.
--
-- > tun_tuning [0,100.. 12700]
tun_tuning :: [Int] -> [String]
tun_tuning =
  let f k c = printf "note %d = %d" k c
  in (:) (tun_sec "Tuning") . zipWith f [0::Int .. 127]

-- | The default base frequency for /Exact Tuning/ (A4=440)
tun_f0_default :: Double
tun_f0_default = 8.1757989156437073336

-- | Format /Exact Tuning/ section given base frequency and sequence of 128 real cents values.
--
-- > tun_exact_tuning tun_f0_default [0,100.. 12700]
tun_exact_tuning :: Double -> [Double] -> [String]
tun_exact_tuning f0 =
  let f k c = printf "note %d = %f" k c
      hdr = [tun_sec "Exact Tuning"
            ,tun_attr_real ("BaseFreq",f0)]
  in (++) hdr  . zipWith f [0::Int .. 127]

{- | Format /Functional Tuning/ section given base frequency and sequence of 128 real cents values.

This simply sets note zero to /f0/ and increments each note by the difference from the previous note.

> tun_functional_tuning tun_f0_default [0,100.. 12700]
-}
tun_functional_tuning :: Double -> [Double] -> [String]
tun_functional_tuning f0 =
  let f k c = printf "note %d = \"#x=%d %% %f\"" k (k - 1) c
      hdr = [tun_sec "Functional Tuning"
            ,printf "note 0 = \"# %f\"" f0]
  in (++) hdr  . zipWith f [1::Int .. 127] . T.d_dx

-- | Format /Scale End/ section header.
tun_end :: [String]
tun_end =
  [tun_sec "Scale End"]

-- | Synonym for a list of strings.
type TUN = [String]

-- | Version 1 has just the /Tuning/ and /Exact Tuning/.
tun_from_cents_version_one :: (Double, [Double]) -> TUN
tun_from_cents_version_one (f0,c) =
  concat [tun_tuning (map round c)
         ,tun_exact_tuning f0 c]

-- | Version 2 files have, in addition, /Begin/, /Info/, /Functional Tuning/ and /End/ sections.
tun_from_cents_version_two :: (String,String) -> (Double, [Double]) -> TUN
tun_from_cents_version_two (nm,k) (f0,c) =
  concat [tun_begin
         ,tun_info (nm,k)
         ,tun_tuning (map round c)
         ,tun_exact_tuning f0 c
         ,tun_functional_tuning f0 c
         ,tun_end]

-- > t = tun_from_cents_version_one (tun_f0_default,[0,100 .. 12700])
-- > t = tun_from_cents_version_two ("equal-temperament-12","et12") (tun_f0_default,[0,100 .. 12700])
-- > tun_store "/home/rohan/et12.tun" t
tun_store :: FilePath -> TUN -> IO ()
tun_store fn = writeFile fn . unlines
