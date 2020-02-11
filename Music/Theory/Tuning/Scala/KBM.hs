-- | Scala "keyboard mapping" files (.kbm) and related data structure.
--
-- <http://www.huygens-fokker.org/scala/help.htm#mappings>
module Music.Theory.Tuning.Scala.KBM where

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Tuning.Scala as T {- hmt -}

{- | Scala keyboard mapping

(sz,(m0,mN),mC,(mF,f),o,m)

- sz      = size of map, the pattern repeats every so many keys
- (m0,mN) = the first and last midi note numbers to retune
- mC      = the middle note where the first entry of the mapping is mapped to
- (mF,f)  = the reference midi-note for which a frequency is given, ie. (69,440)
- o       = scale degree to consider as formal octave
- m       = mapping, numbers represent scale degrees mapped to keys, nil indicates no mapping

-}
type KBM = (Int,(Int,Int),Int,(Int,Double),Int,[Maybe Int])

-- | Parser for scala .kbm file.
kbm_parse :: String -> KBM
kbm_parse s =
  let f x = case x of
              "x" -> Nothing
              _ -> Just (read x)
      to_m k = T.pad_right Nothing k . map f
  in case T.filter_comments (lines s) of
       i1:i2:i3:i4:i5:d1:i6:m ->
         let k = read i1
         in (k,(read i2,read i3),read i4,(read i5,read d1),read i6,to_m k m)
       _ -> error "kbm_parse?"

-- | 'kbm_parse' of 'readFile'
kbm_load :: FilePath -> IO KBM
kbm_load = fmap kbm_parse . readFile

{- | 'kbm_parse' of 'T.load_dist_file'

> kbm_load_dist "bp.kbm"
> kbm_load_dist "7.kbm"
> kbm_load_dist "white.kbm"
-}
kbm_load_dist :: FilePath -> IO KBM
kbm_load_dist = fmap kbm_parse . T.load_dist_file

{- | Pretty-printer for scala .kbm file.

> m <- kbm_load_dist "7.kbm"
> kbm_parse (kbm_pp m) == m
-}
kbm_pp :: KBM -> String
kbm_pp (i1,(i2,i3),i4,(i5,d1),i6,m) =
  let from_m = map (maybe "x" show)
  in unlines ([show i1,show i2,show i3,show i4,show i5,show d1,show i6] ++ from_m m)

-- | 'writeFile' of 'kbm_pp'
kbm_wr :: FilePath -> KBM -> IO ()
kbm_wr fn = writeFile fn . kbm_pp
