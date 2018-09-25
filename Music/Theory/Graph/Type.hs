module Music.Theory.Graph.Type where

import Data.List {- base -}

import qualified Music.Theory.List as T {- hmt -}

-- * GR = graph

type V = Int
type E = (V,V)

e_sort :: E -> E
e_sort (i,j) = (min i j,max i j)

-- | (vertices,edges)
type GR = ([V],[E])

-- | (|v|,|e|)
gr_degree :: GR -> (Int,Int)
gr_degree (v,e) = (length v,length e)

-- | List of E to GR, derives V from E.
eset_to_gr :: [E] -> GR
eset_to_gr e =
  let v = sort (nub (concatMap (\(i,j) -> [i,j]) e))
  in (v,e)

-- * EL = edge list (zero-indexed)

-- | ((|v|,|e|),[e])
type EL = ((Int,Int), [E])

-- | Requires V is (0 .. |v| - 1).
el_to_gr :: EL -> GR
el_to_gr ((nv,ne),e) =
  let v = [0 .. nv - 1]
  in if ne /= length e
     then error (show ("el_to_gr",nv,ne,length e))
     else (v,e)

el_parse_int_list :: String -> [Int]
el_parse_int_list = map read . words

el_parse_int_pairs :: String -> [(Int,Int)]
el_parse_int_pairs = T.adj2 2 . el_parse_int_list

el_parse_int_pair :: String -> (Int,Int)
el_parse_int_pair = T.unlist1_err . el_parse_int_pairs

-- | Parse EL as printed by nauty-listg.
el_parse :: [String] -> EL
el_parse ln =
  case ln of
    [m,e] -> (el_parse_int_pair m,el_parse_int_pairs e)
    _ -> error "parse_e"
