import System.Environment {- base -}

import Music.Theory.Z12
import Music.Theory.Z12.Drape_1999

help :: [String]
help =
    ["pct ess pcset"
    ,"pct frg pcset"
    ,"pct si [pcset]"
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

frg_cmd :: CMD
frg_cmd p =
    let p' = pco_parse p
    in unlines [frg_pp p',ic_cycle_vector_pp (ic_cycle_vector p')]

si_cmd :: CMD
si_cmd = unlines . si . pco_parse

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
    ["frg",p] -> putStr (frg_cmd p)
    ["si"] -> interact_ln si_cmd
    ["si",p] -> putStr (si_cmd p)
    ["tmatrix",p] -> putStr (tmatrix_cmd p)
    ["trs",p] -> interact_ln (trs_cmd trs p)
    ["trs","-m",p] -> interact_ln (trs_cmd trs_m p)
    _ -> putStrLn (unlines help)
