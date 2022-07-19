{- | Scala "keyboard mapping" files (.kbm) and related data structure.

<http://www.huygens-fokker.org/scala/help.htm#mappings>
-}
module Music.Theory.Tuning.Scala.Kbm where

import Data.List {- base -}
import Data.Maybe {- base -}
import System.FilePath {- filepath -}
import Text.Printf {- base -}

import qualified Music.Theory.Directory as Directory {- hmt -}
import qualified Music.Theory.List as List {- hmt -}
import qualified Music.Theory.Pitch as Pitch {- hmt -}
import qualified Music.Theory.Tuning as Tuning {- hmt -}
import qualified Music.Theory.Tuning.Scala as Scala {- hmt -}

{- | Scala keyboard mapping

(sz,(m0,mN),mC,(mF,f),o,m)

- sz      = size of map, the pattern repeats every so many keys
- (m0,mN) = the first and last midi note numbers to retune
- mC      = the middle note where the first entry of the mapping is mapped to
- (mF,f)  = the reference midi-note for which a frequency is given, ie. (69,440)
- o       = scale degree to consider as formal octave
- m       = mapping, numbers represent scale degrees mapped to keys, Nothing indicates no mapping

-}
type Kbm = (Int,(Int,Int),Int,(Int,Double),Int,[Maybe Int])

-- | Pretty-printer for scala .kbm file.
kbm_pp :: Kbm -> String
kbm_pp (sz,(m0,mN),mC,(mF,f),o,m) =
  unlines
  [printf "size = %d" sz
  ,printf "note-range = (%d,%d)" m0 mN
  ,printf "note-center = %d" mC
  ,printf "note-reference = (%d,%f)" mF f
  ,printf "formal-octave = %d" o
  ,printf "map = [%s] #%d" (intercalate "," (map (maybe "x" show) m)) (length m)]

-- | Is /mnn/ in range?
kbm_in_rng :: Kbm -> Int -> Bool
kbm_in_rng (_,(m0,mN),_,_,_,_) mnn = mnn >= m0 && mnn <= mN

-- | Is /kbm/ linear?, ie. is size zero? (formal-octave may or may not be zero)
kbm_is_linear :: Kbm -> Bool
kbm_is_linear (sz,_,_,_,_o,_) = sz == 0 -- && o == 0

{- | Given kbm and midi-note-number lookup (octave,scale-degree).

> k <- kbm_load_dist "example.kbm" -- 12-tone scale
> k <- kbm_load_dist "a440.kbm" -- linear
> k <- kbm_load_dist "white.kbm" -- 7-tone scale on white notes
> k <- kbm_load_dist "black.kbm" -- 5-tone scale on black notes
> k <- kbm_load_dist "128.kbm"

> map (kbm_lookup k) [48 .. 72]

-}
kbm_lookup :: Kbm -> Int -> Maybe (Int,Int)
kbm_lookup kbm mnn =
  if not (kbm_in_rng kbm mnn)
  then Nothing
  else if kbm_is_linear kbm
       then Just (0,mnn)
       else let (sz,(_m0,_mN),mC,(_mF,_f),_o,m) = kbm
                (oct,ix) = ((mnn - mC) `divMod` sz)
            in fmap (\dgr -> (oct,dgr)) (m !! ix)

-- | Return the triple (mF,kbm_lookup k mF,f).  The lookup for mF is not-nil by definition.
--
-- > kbm_lookup_mF k
kbm_lookup_mF :: Kbm -> (Int,(Int,Int),Double)
kbm_lookup_mF k@(_,_,_,(mF,f),_,_) =
  case kbm_lookup k mF of
    Nothing -> error "kbm_lookup_mF?"
    Just r -> (mF,r,f)

-- | Parser for scala .kbm file.
kbm_parse :: String -> Kbm
kbm_parse s =
  let f x = case x of
              "x" -> Nothing
              _ -> Just (read x)
      to_m sz = List.pad_right_no_truncate Nothing sz . map f -- _err -- some scala .kbm have |m| > sz?
  in case Scala.filter_comments (lines s) of
       i1:i2:i3:i4:i5:d1:i6:m ->
         let sz = read i1
         in (sz,(read i2,read i3),read i4,(read i5,read d1),read i6,to_m sz m)
       _ -> error "kbm_parse?"

-- | 'kbm_parse' of 'readFile'
kbm_load_file :: FilePath -> IO Kbm
kbm_load_file = fmap kbm_parse . readFile

{- | 'kbm_parse' of 'Scala.load_dist_file'

> pp nm = kbm_load_dist nm >>= \x -> putStrLn (kbm_pp x)
> pp "example"
> pp "bp"
> pp "7" -- error -- 12/#13
> pp "8" -- error -- 12/#13
> pp "white" -- error -- 12/#13
> pp "black" -- error -- 12/#13
> pp "128"
> pp "a440"
> pp "61"
-}
kbm_load_dist :: String -> IO Kbm
kbm_load_dist nm = fmap kbm_parse (Scala.load_dist_file (nm <.> "kbm"))

-- | If /nm/ is a file name (has a .kbm) extension run 'kbm_load_file' else run 'kbm_load_dist'.
kbm_load :: String -> IO Kbm
kbm_load nm = if hasExtension nm then kbm_load_file nm else kbm_load_dist nm

-- | Load all .kbm files at directory.
kbm_load_dir_fn :: FilePath -> IO [(FilePath, Kbm)]
kbm_load_dir_fn d = do
  fn <- Directory.dir_subset [".kbm"] d
  kbm <- mapM kbm_load fn
  return (zip fn kbm)

{- | Load all .kbm files at scala dist dir.

> db <- kbm_load_dist_dir_fn
> length db == 41
> x = map (\(fn,(sz,_,_,_,o,m)) -> (System.FilePath.takeFileName fn,sz,length m,o)) db
> filter (\(_,i,j,_) -> i < j) x -- size < map-length
> filter (\(_,i,_,k) -> i == 0 && k == 0) x -- size and formal octave both zero

> map (\(fn,k) -> (System.FilePath.takeFileName fn,kbm_lookup_mF k)) db
-}
kbm_load_dist_dir_fn :: IO [(FilePath, Kbm)]
kbm_load_dist_dir_fn = Scala.dist_get_dir >>= kbm_load_dir_fn

{- | Pretty-printer for scala .kbm file.

> m <- kbm_load_dist "7.kbm"
> kbm_parse (kbm_format m) == m
> putStrLn $ kbm_pp m
-}
kbm_format :: Kbm -> String
kbm_format (i1,(i2,i3),i4,(i5,d1),i6,m) =
  let from_m = map (maybe "x" show)
  in unlines ([show i1,show i2,show i3,show i4,show i5,show d1,show i6] ++ from_m m)

-- | 'writeFile' of 'kbm_format'
kbm_wr :: FilePath -> Kbm -> IO ()
kbm_wr fn = writeFile fn . kbm_format

{- | Standard 12-tone mapping with A=440hz (ie. example.kbm)

> fmap (== kbm_d12_a440) (kbm_load_dist "example.kbm")
> putStrLn $ kbm_pp kbm_d12_a440
-}
kbm_d12_a440 :: Kbm
kbm_d12_a440 = (12,(0,127),60,(69,440.0),12,map Just [0 .. 11])

kbm_d12_c256 :: Kbm
kbm_d12_c256 = (12,(0,127),60,(60,256.0),12,map Just [0 .. 11])

-- | Given size and note-center calculate relative octave and key
--   number (not scale degree) of the zero entry.
--
-- > map (kbm_k0 12) [59,60,61] == [(-4,1),(-5,0),(-5,11)]
kbm_k0 :: Int -> Int -> (Int,Int)
kbm_k0 sz mC = let (o,r) = mC `quotRem` sz in (negate o,negate r `mod` sz)

-- | Given size and note-center calculate complete octave and key
-- number sequence (ie. for entries 0 - 127).
--
-- > map (zip [0..] . kbm_oct_key_seq 12) [59,60,61]
kbm_oct_key_seq :: Kbm -> [(Int,(Int,Int))]
kbm_oct_key_seq (sz,(m0,mN),mC,(_mF,_f),_o,_m) =
  let (o0,k0) = kbm_k0 sz mC
      dgr = map (`mod` sz) (take 128 [k0 ..])
      upd o j = if j == 0 then (o + 1,(o + 1,j)) else (o,(o,j))
      key_seq = snd (mapAccumL upd (o0 - 1) dgr)
  in zip [m0 .. ] (take (mN - m0 + 1) (drop m0 key_seq))

-- | Given Kbm and SCL calculate frequency of note-center.
kbm_mC_freq :: Kbm -> Scala.Scale -> Double
kbm_mC_freq (sz,(_m0,_mN),mC,(mF,f),_o,m) scl =
  let dist_k = (mF - mC) `mod` sz
      dgr = fromMaybe (error "kbm_mC_freq") (m !! dist_k)
      c = Scala.scale_cents scl !! dgr
  in Tuning.cps_shift_cents f (- c)

-- | Given Kbm and SCL calculate fractional midi note-numbers for each key.
kbm_fmidi_tbl :: Kbm -> Scala.Scale -> [(Int, Double)]
kbm_fmidi_tbl kbm scl =
  let (_sz,(_m0,_mN),_mC,(_mF,_f),o,m) = kbm
      mC_freq = kbm_mC_freq kbm scl
      mC_fmidi = Pitch.cps_to_fmidi mC_freq
      key_seq = kbm_oct_key_seq kbm
      c = Scala.scale_cents scl
      oct_cents = c !! o
      oct_key_to_cents (oct,key) = maybe 0 (c !!) (m !! key) + (fromIntegral oct * oct_cents)
  in map (\(mnn,oct_key) -> (mnn,mC_fmidi + (oct_key_to_cents oct_key / 100.0))) key_seq

-- | Given Kbm and SCL calculate frequencies for each key.
kbm_cps_tbl :: Kbm -> Scala.Scale -> [(Int, Double)]
kbm_cps_tbl kbm = let f (k,n) = (k,Tuning.fmidi_to_cps n) in map f . kbm_fmidi_tbl kbm

{-

scl <- Scala.scl_load "young-lm_piano"
scl <- Scala.scl_load "meanquar"
scl <- Scala.scl_load "et12"
kbm <- kbm_load "example" -- d12_a440 -- kbm_d12_a440 kbm_d12_c256

kbm_fmidi_tbl kbm scl
kbm_cps_tbl kbm scl

-}
