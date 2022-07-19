import Data.Char {- base -}
import Data.Int {- base -}
import System.Environment {- base -}

import Music.Theory.Z {- hmt -}
import Music.Theory.Z.Drape_1999 {- hmt -}
import Music.Theory.Z.Forte_1973 {- hmt -}
import Music.Theory.Z.Sro {- hmt -}

type Z12 = Int8

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

z16_seq_parse :: String -> [Int]
z16_seq_parse = map digitToInt

pco_parse :: String -> [Z12]
pco_parse = map fromIntegral . z16_seq_parse

pco_pp :: [Z12] -> String
pco_pp = map (toUpper . integral_to_digit)

-- > cset_parse "34" == [3,4]
cset_parse :: String -> [Int]
cset_parse = map digitToInt

type CMD = String -> String

mk_cmd :: ([Z12] -> [Z12]) -> CMD
mk_cmd f = pco_pp . f . pco_parse

mk_cmd_many :: ([Z12] -> [[Z12]]) -> CMD
mk_cmd_many f = unlines . map pco_pp . f . pco_parse

-- > ess_cmd "0164325" "23A" == unlines ["923507A","2B013A9"]
ess_cmd :: String -> CMD
ess_cmd p = mk_cmd_many (ess z12 (pco_parse p))

z12_sc_name :: [Z12] -> SC_Name
z12_sc_name = sc_name

fl_c_cmd :: CMD
fl_c_cmd = unlines . map z12_sc_name . concatMap scs_n . cset_parse

frg_cmd :: CMD
frg_cmd p =
    let p' = pco_parse p
    in unlines [frg_pp p',ic_cycle_vector_pp (ic_cycle_vector p')]

pi_cmd :: String -> CMD
pi_cmd p = mk_cmd_many (pci z12 (z16_seq_parse p))

scc_cmd :: String -> CMD
scc_cmd p = mk_cmd_many (scc z12 (sc p))

si_cmd :: CMD
si_cmd = unlines . si . pco_parse

z12_sc_name_long :: [Z12] -> SC_Name
z12_sc_name_long = sc_name_long

-- > spsc_cmd ["4-11","4-12"] == "5-26[02458]\n"
spsc_cmd :: [String] -> String
spsc_cmd = unlines . map z12_sc_name_long . spsc z12 . map sc

sra_cmd :: CMD
sra_cmd = mk_cmd_many (sra z12)

sro_cmd :: String -> CMD
sro_cmd o = mk_cmd (sro z12 (sro_parse 5 o))

-- > tmatrix_cmd "1258"
tmatrix_cmd :: CMD
tmatrix_cmd = mk_cmd_many (tmatrix z12)

-- > trs_cmd (trs z12) "024579" "642"
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
    ["pi",p,q] -> putStr (pi_cmd q p)
    ["scc",p] -> interact_ln (scc_cmd p)
    ["scc",p,q] -> putStr (scc_cmd p q)
    ["si"] -> interact_ln si_cmd
    ["si",p] -> putStr (si_cmd p)
    "spsc":p -> putStr (spsc_cmd p)
    ["sra"] -> interact_ln sra_cmd
    ["sro",o] -> interact_ln (sro_cmd o)
    ["tmatrix",p] -> putStr (tmatrix_cmd p)
    ["trs",p] -> interact_ln (trs_cmd (trs z12) p)
    ["trs","-m",p] -> interact_ln (trs_cmd (trs_m z12) p)
    _ -> putStrLn (unlines help)
