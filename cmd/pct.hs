import System.Environment {- base -}

import Music.Theory.Z12
import Music.Theory.Z12.Drape_1999

help :: [String]
help =
    ["pct ess pcset"
    ,"pct frg pcset"
    ,"pct si [pcset]"]

pco_parse :: String -> [Z12]
pco_parse = map char_to_z12

pco_pp :: [Z12] -> String
pco_pp = map z12_to_char

type CMD = String -> String

ess_cmd :: String -> CMD
ess_cmd p q = unlines (map pco_pp (ess (pco_parse q) (pco_parse p)))

frg_cmd :: CMD
frg_cmd p =
    let p' = pco_parse p
    in unlines [frg_pp p',ic_cycle_vector_pp (ic_cycle_vector p')]

si_cmd :: CMD
si_cmd = unlines . si . pco_parse

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
    _ -> putStrLn (unlines help)
