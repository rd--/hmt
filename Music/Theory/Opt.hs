{- | Very simple CLI option parser.

Only allows options of the form --key=value, with the form --key equal to --key=True.

A list of OptUsr describes the options and provides default values.

'get_opt_arg' merges user and default values into a table with values for all options.

To fetch options use 'opt_get' and 'opt_read'.

-}
module Music.Theory.Opt where

import Control.Monad {- base -}
import Data.Either {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import System.Environment {- base -}
import System.Exit {- base -}

import qualified Data.List.Split as Split {- split -}

import qualified Music.Theory.Read as T {- hmt -}

-- | (KEY,VALUE)
--   Key does not include leading '--'.
type Opt = (String,String)

-- | (KEY,DEFAULT-VALUE,TYPE,NOTE)
type OptUsr = (String,String,String,String)

-- | Re-write default values at OptUsr.
opt_usr_rw_def :: [Opt] -> [OptUsr] -> [OptUsr]
opt_usr_rw_def rw =
  let f (k,v,ty,dsc) = case lookup k rw of
                         Just v' -> (k,v',ty,dsc)
                         Nothing -> (k,v,ty,dsc)
  in map f

-- | OptUsr to Opt.
opt_plain :: OptUsr -> Opt
opt_plain (k,v,_,_) = (k,v)

-- | OptUsr to help string, indent is two spaces.
opt_usr_help :: OptUsr -> String
opt_usr_help (k,v,t,n) = concat ["  ",k,":",t," -- ",n,"; default=",if null v then "NIL" else v]

-- | 'unlines' of 'opt_usr_help'
opt_help :: [OptUsr] -> String
opt_help = unlines . map opt_usr_help

-- | Lookup KEY in Opt, error if non-existing.
opt_get :: [Opt] -> String -> String
opt_get o k = fromMaybe (error ("opt_get: " ++ k)) (lookup k o)

-- | Variant that returns Nothing if the result is the empty string, else Just the result.
opt_get_nil :: [Opt] -> String -> Maybe String
opt_get_nil o k = let r = opt_get o k in if null r then Nothing else Just r

-- | 'read' of 'get_opt'
opt_read :: Read t => [Opt] -> String -> t
opt_read o = T.read_err . opt_get o

-- | Parse k or k=v string, else error.
opt_param_parse :: String -> Opt
opt_param_parse p =
  case Split.splitOn "=" p of
    [lhs] -> (lhs,"True")
    [lhs,rhs] -> (lhs,rhs)
    _ -> error ("opt_param_parse: " ++ p)

-- | Parse option string of form "--opt" or "--key=value".
--
-- > opt_parse "--opt" == Just ("opt","True")
-- > opt_parse "--key=value" == Just ("key","value")
opt_parse :: String -> Maybe Opt
opt_parse s =
  case s of
    '-':'-':p -> Just (opt_param_parse p)
    _ -> Nothing

-- | Parse option sequence, collating options and non-options.
--
-- > opt_set_parse (words "--a --b=c d") == ([("a","True"),("b","c")],["d"])
opt_set_parse :: [String] -> ([Opt],[String])
opt_set_parse =
  let f s = maybe (Right s) Left (opt_parse s)
  in partitionEithers . map f

-- | Left-biased Opt merge.
opt_merge :: [Opt] -> [Opt] -> [Opt]
opt_merge p q =
  let x = map fst p
  in p ++ filter (\(k,_) -> k `notElem` x) q

-- | Process argument list.
opt_proc :: [OptUsr] -> [String] -> ([Opt], [String])
opt_proc def arg =
  let (o,a) = opt_set_parse arg
  in (opt_merge o (map opt_plain def),a)

-- | Usage text
type OptHelp = [String]

-- | Format usage pre-amble and 'opt_help'.
opt_help_pp :: OptHelp -> [OptUsr] -> String
opt_help_pp usg def = unlines (usg ++ ["",opt_help def])

-- | Print help and exit.
opt_usage :: OptHelp -> [OptUsr] -> IO ()
opt_usage usg def = putStrLn (opt_help_pp usg def)  >> exitSuccess

-- | Print help and error.
opt_error :: OptHelp -> [OptUsr] -> t
opt_error usg def = error (opt_help_pp usg def)

-- | Verify that all Opt have keys that are in OptUsr
opt_verify :: OptHelp -> [OptUsr] -> [Opt] -> IO ()
opt_verify usg def =
  let k_set = map (fst . opt_plain) def
      f (k,_) = if k `elem` k_set
                then return ()
                else putStrLn ("UNKNOWN KEY: " ++ k ++ "\n") >> opt_usage usg def
  in mapM_ f

-- | 'opt_set_parse' and maybe 'opt_verify' and 'opt_merge' of 'getArgs'.
--   If arguments include -h or --help run 'opt_usage'
opt_get_arg :: Bool -> OptHelp -> [OptUsr] -> IO ([Opt],[String])
opt_get_arg chk usg def = do
  a <- getArgs
  when ("-h" `elem` a || "--help" `elem` a) (opt_usage usg def)
  let (o,p) = opt_set_parse a
  when chk (opt_verify usg def o)
  return (opt_merge o (map opt_plain def),p)

-- | Parse param set, one parameter per line.
--
-- > opt_param_set_parse "a\nb=c" == [("a","True"),("b","c")]
opt_param_set_parse :: String -> [Opt]
opt_param_set_parse = map opt_param_parse . lines

-- | Simple scanner over argument list.
opt_scan :: [String] -> String -> Maybe String
opt_scan a k =
  let (o,_) = opt_set_parse a
  in fmap snd (find ((== k) . fst) o)

-- | Scanner with default value.
opt_scan_def :: [String] -> (String,String) -> String
opt_scan_def a (k,v) = fromMaybe v (opt_scan a k)

-- | Reading scanner with default value.
opt_scan_read :: Read t => [String] -> (String,t) -> t
opt_scan_read a (k,v) = maybe v read (opt_scan a k)
