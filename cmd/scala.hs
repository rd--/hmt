import Data.Char {- base -}
import Data.List {- base -}
import System.Environment {- base -}

import qualified Music.Theory.Function as T {- hmt -}
import qualified Music.Theory.Read as T {- hmt -}
import qualified Music.Theory.Tuning.Scala as T {- hmt -}

db_stat :: IO ()
db_stat = do
  db <- T.scl_load_db :: IO [T.Scale Integer]
  let po = filter (== Just (Right 2)) (map T.scale_octave db)
      uf = filter T.is_scale_uniform db
      r = ["# entries        : " ++ show (length db)
          ,"# perfect-octave : " ++ show (length po)
          ,"# scale-uniform  : " ++ show (length uf)]
  putStrLn (unlines r)

env :: IO ()
env = do
  dir <- T.scl_get_dir
  putStrLn ("SCALA_SCL_DIR = " ++ if null dir then "NOT SET" else dir)

cut :: Maybe Int -> [a] -> [a]
cut lm s = maybe s (\n -> take n s) lm

-- > search (True,Nothing) ["xenakis"]
-- > search (True,Just 75) ["lamonte","young"]
search :: (Bool,Maybe Int) -> [String] -> IO ()
search (ci,lm) txt = do
  db <- T.scl_load_db :: IO [T.Scale Integer]
  let modify = if ci then map toLower else id
      txt' = map modify txt
      db' = filter (T.predicate_all (map isInfixOf txt') . modify . T.scale_description) db
  mapM_ (putStrLn . unlines . map (cut lm) . T.scale_stat) db'

stat_all :: Maybe Int -> IO ()
stat_all lm = do
  db <- T.scl_load_db :: IO [T.Scale Integer]
  mapM_ (putStrLn . unlines . map (cut lm) . T.scale_stat) db

-- > stat_by_name Nothing "young-lm_piano"
stat_by_name :: Maybe Int -> FilePath -> IO ()
stat_by_name lm nm = do
  sc <- T.scl_load nm :: IO (T.Scale Integer)
  putStrLn (unlines (map (cut lm) (T.scale_stat sc)))

help :: [String]
help =
    ["db-stat"
    ,"env"
    ,"search ci|cs lm|nil text:string..."
    ,"stat all lm|nil"
    ,"stat scale lm|nil name:string|file-path"]

nil_or_read :: Read a => String -> Maybe a
nil_or_read s = if s == "nil" then Nothing else Just (T.read_err s)

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["db-stat"] -> db_stat
    ["env"] -> env
    "search":ci:lm:txt -> search (ci == "ci",nil_or_read lm) txt
    ["stat","all",lm] -> stat_all (nil_or_read lm)
    ["stat","scale",lm,nm] -> stat_by_name (nil_or_read lm) nm
    _ -> putStrLn (unlines help)
