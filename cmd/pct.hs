import System.Environment {- base -}

import Music.Theory.Z12
import Music.Theory.Z12.Drape_1999

help :: [String]
help = ["pct si pcset"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["si",p] -> putStrLn (unlines (si (map char_to_z12 p)))
    _ -> putStrLn (unlines help)
