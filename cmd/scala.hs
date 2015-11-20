import Data.Char {- base -}
import Data.List {- base -}
import System.Environment {- base -}

import qualified Music.Theory.Function as T {- hmt -}
import qualified Music.Theory.Tuning.Scala as T {- hmt -}

-- > search True ["xenakis"]
-- > search True ["lamonte","young"]
search :: Bool -> [String] -> IO ()
search ci txt = do
  db <- T.scl_load_db :: IO [T.Scale Integer]
  let modify = if ci then map toLower else id
      txt' = map modify txt
      db' = filter (T.predicate_all (map isInfixOf txt') . modify . T.scale_description) db
  mapM_ (putStrLn . unlines . T.scale_stat) db'

stat_all :: IO ()
stat_all = do
  db <- T.scl_load_db :: IO [T.Scale Integer]
  mapM_ (putStrLn . unlines . T.scale_stat) db

-- > stat_by_name "young-lm_piano"
stat_by_name :: FilePath -> IO ()
stat_by_name nm = do
  sc <- T.scl_load nm :: IO (T.Scale Integer)
  putStrLn (unlines (T.scale_stat sc))

help :: [String]
help =
    ["search ci|cs text:string..."
    ,"stat all"
    ,"stat scale-name:string|file-path"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    "search":ci:txt -> search (ci == "ci") txt
    ["stat","all"] -> stat_all
    ["stat",nm] -> stat_by_name nm
    _ -> putStrLn (unlines help)

