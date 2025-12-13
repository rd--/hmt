-- | Scala Db meta-data.
module Music.Theory.Tuning.Scala.Meta where

import qualified Music.Theory.Json as Json {- hmt-base -}

{- | Just-intonation (ie. all rational) scales, collected by author.

>>> db <- scl_ji_au "/home/rohan/sw/hmt/data/json/scala-meta-au.json"
>>> mapM_ (\x -> print (fst x, length (snd x))) db
("Alves, Bill",5)
("Archytas",18)
("Ayers, Lydia",7)
("Barlow, Clarence",7)
("Boethius",2)
("Bohlen, Heinz",23)
("Burt, Warren",24)
("Canright, David",1)
("Chalmers, John",23)
("Danielou, Alain",4)
("Didymus",7)
("Doty, David",3)
("Ellis, Alexander John",10)
("Eratosthenes",3)
("Erlich, Paul",58)
("Euler, Leonhard",67)
("Fokker, Adriaan",72)
("Gann, Kyle",16)
("Grady, Kraig",15)
("Hahn, Paul",5)
("Harrison, Lou",34)
("Johnston, Ben",12)
("Kepler, Johannes",3)
("Kirnberger, Johann",6)
("Novaro, Augusto",3)
("Partch, Harry",13)
("Ptolemy",30)
("Pythagoras",8)
("Riley, Terry",2)
("Sabat, Marc",1)
("Schulter, Margo",21)
("Smith, Gene Ward",69)
("Snyder, Jeff",1)
("Tenney, James",8)
("Wilson, Erv",113)
("Young, La Monte",2)
-}
scl_ji_au :: FilePath -> IO [(String, [String])]
scl_ji_au fn = do
  o <- Json.readFile fn
  return (Json.value_to_assoc_list Json.value_to_string_list o)

{-
Archytas -- archchro archytas7 archytas12,archytas12sync Non-Ji
Ellis, Alexander John -- duodene_w ellis ellis_eb ellis_mteb mean11 mean19 mean23 meaneb451
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
