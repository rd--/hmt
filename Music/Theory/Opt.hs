{- | Very simple CLI option parser.

A list of OPT_USR describes the options and provides default values.

'get_opt_arg' merges user and default values into a table with values for all options.

To fetch options use 'opt_get' and 'opt_read'.

-}
module Music.Theory.Opt where

import Data.Either {- base -}
import Data.Maybe {- base -}
import System.Environment {- base -}

import qualified Data.List.Split as Split {- split -}

-- | (KEY,VALUE)
type OPT = (String,String)

-- | (KEY,VALUE,TYPE,NOTE)
type OPT_USR = (String,String,String,String)

-- | OPT_USR to OPT.
opt_plain :: OPT_USR -> OPT
opt_plain (k,v,_,_) = (k,v)

-- | OPT_USR to help string.
opt_usr_help :: OPT_USR -> String
opt_usr_help (k,v,t,n) = concat [k,":",t," -- ",n,"; default=",v]

-- | Lookup KEY in OPT, error if non-existing.
opt_get :: [OPT] -> String -> String
opt_get o k = fromMaybe (error ("opt_get: " ++ k)) (lookup k o)

-- | 'read' of 'get_opt'
opt_read :: Read t => [OPT] -> String -> t
opt_read o = read . opt_get o

-- | Parse option string of form "--opt" or "--key=value".
--
-- > opt_parse "--opt" == Just ("opt","true")
-- > opt_parse "--key=value" == Just ("key","value")
opt_parse :: String -> Maybe OPT
opt_parse s =
  case s of
    '-':'-':o -> case Split.splitOn "=" o of
                   [lhs] -> Just (lhs,"true")
                   [lhs,rhs] -> Just (lhs,rhs)
                   _ -> error "opt_parse"
    _ -> Nothing

-- | Parse option sequence, collating options and non-options.
--
-- > opt_set_parse (words "--a --b=c d") == ([("a","true"),("b","c")],["d"])
opt_set_parse :: [String] -> ([OPT],[String])
opt_set_parse =
  let f s = maybe (Right s) Left (opt_parse s)
  in partitionEithers . map f

-- | Left-biased OPT merge.
opt_merge :: [OPT] -> [OPT] -> [OPT]
opt_merge p q =
  let x = map fst p
  in p ++ filter (\(k,_) -> k `notElem` x) q

-- | Process argument list.
opt_proc :: [OPT_USR] -> [String] -> ([OPT], [String])
opt_proc def arg =
  let (o,a) = opt_set_parse arg
  in (opt_merge o (map opt_plain def),a)

-- | 'opt_merge' and 'opt_set_parse' of 'getArgs'
get_opt_arg :: [OPT_USR] -> IO ([OPT],[String])
get_opt_arg def = fmap ((\(o,a) -> (opt_merge o (map opt_plain def),a)) . opt_set_parse) getArgs
