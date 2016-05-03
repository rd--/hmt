import Data.Char {- base -}
import System.Environment {- base -}

import qualified Music.Theory.Z.SRO as Z

import Music.Theory.Z12
import Music.Theory.Z12.Drape_1999
import qualified Music.Theory.Z12.Forte_1973 as Z12

help :: [String]
help =
    ["pct ess pcset"
    ,"pct fl -c cset"
    ,"pct frg pcset"
    ,"pct si [pcset]"
    ,"pct spsc set-class..."
    ,"pct sra"
    ,"pct sro sro"
    ,"pct tmatrix pcseg"
    ,"pct trs [-m] pcseg"]

pco_parse :: String -> [Z12]
pco_parse = map char_to_z12

pco_pp :: [Z12] -> String
pco_pp = map z12_to_char

type CMD = String -> String

mk_cmd :: ([Z12] -> [Z12]) -> CMD
mk_cmd f = pco_pp . f . pco_parse

mk_cmd_many :: ([Z12] -> [[Z12]]) -> CMD
mk_cmd_many f = unlines . map pco_pp . f . pco_parse

-- > ess_cmd "0164325" "23A" == unlines ["923507A","2B013A9"]
ess_cmd :: String -> CMD
ess_cmd p = mk_cmd_many (ess (pco_parse p))

fl_c_cmd :: CMD
fl_c_cmd = unlines . map Z12.sc_name . concatMap Z12.scs_n . map digitToInt

frg_cmd :: CMD
frg_cmd p =
    let p' = pco_parse p
    in unlines [frg_pp p',ic_cycle_vector_pp (ic_cycle_vector p')]

si_cmd :: CMD
si_cmd = unlines . si . pco_parse

-- > spsc_cmd ["4-11","4-12"] == "5-26[02458]\n"
spsc_cmd :: [String] -> String
spsc_cmd = unlines . map Z12.sc_name_long . spsc . map Z12.sc

sra_cmd :: CMD
sra_cmd = mk_cmd_many sra

sro_cmd :: String -> CMD
sro_cmd o = mk_cmd (sro (Z.sro_parse o))

-- > tmatrix_cmd "1258"
tmatrix_cmd :: CMD
tmatrix_cmd = mk_cmd_many tmatrix

-- > trs_cmd trs "024579" "642"
trs_cmd :: ([Z12] -> [Z12] -> [[Z12]]) -> String -> CMD
trs_cmd f p = mk_cmd_many (f (pco_parse p))

interact_ln :: CMD -> IO ()
interact_ln f = interact (unlines . map f . lines)

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["ess",p] -> interact_ln (ess_cmd p)
    ["fl","-c",c] -> putStr (fl_c_cmd c)
    ["frg",p] -> putStr (frg_cmd p)
    ["si"] -> interact_ln si_cmd
    ["si",p] -> putStr (si_cmd p)
    "spsc":p -> putStr (spsc_cmd p)
    ["sra"] -> interact_ln sra_cmd
    ["sro",o] -> interact_ln (sro_cmd o)
    ["tmatrix",p] -> putStr (tmatrix_cmd p)
    ["trs",p] -> interact_ln (trs_cmd trs p)
    ["trs","-m",p] -> interact_ln (trs_cmd trs_m p)
    _ -> putStrLn (unlines help)
