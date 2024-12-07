-- | Scala Db meta-data.
module Music.Theory.Tuning.Scala.Meta where

import qualified Music.Theory.Json as Json {- hmt-base -}

{- | Just-intonation (ie. all rational) scales, collected by author.

> db <- scl_ji_au "/home/rohan/sw/hmt/data/json/scala-meta-au.json"
> map fst db
-}
scl_ji_au :: FilePath -> IO [(String, [String])]
scl_ji_au fn = do
  o <- Json.readFile fn
  return (Json.value_to_assoc_list Json.value_to_string_list o)

{-
Archytas -- archchro archytas7 archytas12,archytas12sync Non-Ji
Erlich, Paul -- erlich1 erlich2 erlich3 erlich4 erlich5 erlich7 erlich8 erlich10a erlich12 erlich_bppe erlich_bppm erlichpump -- Non-Ji
Gann, Kyle -- gann_wolfe
Grady, Kraig -- grady_mirror-meta-pelog{7,9,20} grady_mirror-meta-slendro{12,17} -- Non-Ji
Hahn, Paul -- hahn_g mean14a
Harrison, Lou -- slendro_laras -- Non-Oct
Pythagoras -- pyth_31 pyth_sev pyth_third Not-Ji
Wilson, Erv -- eikohole2 eikohole4 eikohole5 eikohole6 stelhex3 stelhex4 wilson_gh1,wilson_gh2,wilson_gh11,wilson_gh50 -- Non-Ji
-}

{-

import Music.Theory.Tuning.Scala
db <- scl_load_db_dir
m <- scl_ji_au "/home/rohan/sw/hmt/data/json/scala-meta-au.json"
nm = nub (sort (concatMap snd m))
length nm = 515
scl = filter (\x -> scale_name x `elem` nm) db
length scl == 515
nm \\ map scale_name scl == []
non_ji = filter (not . scl_is_ji) scl
map scale_name non_ji

-}
