{- | Geometrical Drawings

A. Bernard Deacon and Camilla H. Wedgwood. “Geometrical Drawings
from Malekula and Other Islands of the New Hebrides”. The Journal
of the Royal Anthropological Institute of Great Britain and
Ireland, 64:129—175, 1934.
-}
module Music.Theory.Graph.Deacon_1934 where

import Data.Bifunctor {- base -}
import Data.List {- base -}

import qualified Music.Theory.Array.Cell_Ref as Cell_Ref {- hmt-base -}
import qualified Music.Theory.List as List {- hmt-base -}
import qualified Music.Theory.Tuple as Tuple {- hmt-base -}

import qualified Music.Theory.Array.Direction as Direction {- hmt -}
import qualified Music.Theory.Graph.Dot as Dot {- hmt -}
import qualified Music.Theory.Graph.Fgl as Fgl {- hmt -}

gen_graph :: Ord v => [Dot.Dot_Attr] -> Dot.Graph_Pp v e -> [Fgl.Edge_Lbl v e] -> [String]
gen_graph opt pp es = Fgl.fgl_to_udot opt pp (Fgl.g_from_edges_l es)

gen_graph_ul :: Ord v => [Dot.Dot_Attr] -> (v -> String) -> [Fgl.Edge v] -> [String]
gen_graph_ul opt pp es = Fgl.fgl_to_udot opt (Dot.gr_pp_label_v pp) (Fgl.g_from_edges es)

gen_digraph :: Ord v => [Dot.Dot_Attr] -> Dot.Graph_Pp v e -> [Fgl.Edge_Lbl v e] -> [String]
gen_digraph opt pp es = Fgl.fgl_to_dot Dot.Graph_Digraph opt pp (Fgl.g_from_edges_l es)

type G = ([Fgl.Edge String], [Dot.Dot_Attr], FilePath)

-- * E
g1 :: G
g1 =
  let c1 = words "A1 B2 A3 B4 C3 B2 C1 D2 C3 D4 D3 D2 D1 C2 D3 C4 B3 C2 B1 A2 B3 A4 A3 A2 A1"
      o1 = [("node:shape", "circle"), ("edge:len", "1.5"), ("edge:fontsize", "7")]
  in (List.adj2 1 c1, o1, "E")

-- * D
g2 :: G
g2 =
  let c2' = words "B3 C2 = C3 B2 A1 = A2 B1 C2 = C1 B2 A3 = A2 B3 C3 C2 B2 B3 ~ C3 ~ C2 C1 == C3 C2 C1 B1 B2 C2 ~ C1 ~ B1 A1 C1 B1 A1 A2 B2 B1 ~ A1 ~ A2 A3 == A1 A2 A3 B3 B2 A2 ~ A3 ~ B3 C3 C3 ~~ C1 ~~ A1 ~~ A3 A3 B3"
      c2 = filter Cell_Ref.is_cell_ref c2'
      o2 = [("node:shape", "circle"), ("edge:len", "3"), ("edge:fontsize", "7")]
  in (List.adj2 1 c2, o2, "D")

-- * A
g4 :: G
g4 =
  let c4' = words "B1 C2 C3 B3 B2 C2 ~~ C3 C2 ~~ C1 C2 C2 B3 A3 A2 B2 B3 ~~ A3 B3 ~~ C3 B3 B3 A2 A1 B1 B2 A2 ~~ A1 A2 ~~ A3 A2 A2 B1 C1 C2 B2 B1 ~~ C1 B1 ~~ A1 B1 B1"
      c4 = filter Cell_Ref.is_cell_ref c4'
      o4 = [("node:shape", "circle"), ("edge:len", "3"), ("edge:fontsize", "7")]
  in (List.adj2 1 c4, o4, "A")

g6 :: G
g6 =
  let c6' = words "B1 C2 B2 C1 B1 A2 B2 A1 B1 B2 B3 B3 B3 B3 B2 B1 B0 B0 B0 B0 B1 C1 ~~~ C2 B2 B2 B2 A2 ~~~ A1 B1 B1 B1"
      c6 = filter Cell_Ref.is_cell_ref c6'
      o6 = [("node:shape", "circle"), ("edge:len", "3"), ("edge:fontsize", "7")]
  in (List.adj2 1 c6, o6, "B")

g8 :: G
g8 =
  let c8' = words "C2 B1 B1 A2 ~ (04) B1 B2 B3 ~ (08) C2 B3 B3 A2 ~ (13) B3 B2 A2 (17) A3 A3 B2 C1 C1 C2 B2 B1 ~ (23) C2 B2 A2 A1 A1 B2 C3 C3 C2"
      c8 = filter Cell_Ref.is_cell_ref c8'
      o8 = [("node:shape", "circle"), ("edge:len", "3"), ("edge:fontsize", "7")]
  in (List.adj2 1 c8, o8, "C")

g9 :: G
g9 =
  let d9' = ("E6", words "U R D LL (03/D6) U R R U L D D LL (11/C6) U R R U U R D L L D D LL (22/B6) U R R U U R R U L D D L L D D LL (38/A6) U R R U U R R U U R D L L D D L L D D LUU (56/A4) R R U U R R U L D D L L D D L UU (71/A3) R R U U R D L L D D L UU (83/A2) R R U L D D L UU (91/A1) R D L")
      d9 = second (filter Direction.is_direction) d9'
      c9 = Direction.dir_seq_to_cell_seq d9
      o9 = [("node:shape", "circle"), ("edge:len", "1.5"), ("edge:fontsize", "7")]
  in (List.adj2 1 c9, o9, "F")

g10 :: G
g10 =
  let d10' = ("B7", words "U R LL (03/A6) R R U L D D LUU (10/A5) R R U L D D L UU (18/A4) R R U L D D L UU (26/A3) R R U L D D L UU (34/A2) R R U L D D L UU (41/A1) R D L")
      d10 = second (filter Direction.is_direction) d10'
      c10 = Direction.dir_seq_to_cell_seq d10
      e10 = List.adj2 1 c10
      o10 = [("node:shape", "circle"), ("edge:len", "1.5"), ("edge:fontsize", "7")]
  in (e10, o10, "G")

g11 :: G
g11 =
  let d11' = ("C3", words "DR DDL UUR U L (05/C3) DL DDR UUL U R (10/C3) D D U UL UUR DDL (16/B3) DL R U (18/B3) L DR R (21/C4) UR UUL DDR DR L (26/D4) U R DL L U (31/C3) U D (33/C3) R UUR DDDDD UUL L . (40/C4) L DDL UUUUU DDR R (44/C3)")
      d11 = second (filter Direction.is_direction) d11'
      c11 = Direction.dir_seq_to_cell_seq d11
      e11 = List.adj2 1 c11
      o11 = [("node:shape", "circle"), ("edge:len", "1.5"), ("edge:fontsize", "7")]
  in (e11, o11, "H")

g12 :: G
g12 =
  let d12' = ("C2", words "DR UR (02/E2) L DL UL L (06/A2) DR UR UR DR (10/E2) L UL DL L (14/A2) UR DR (16/C2)")
      d12 = second (filter Direction.is_direction) d12'
      c12 = Direction.dir_seq_to_cell_seq d12
      e12 = List.adj2 1 c12
      o12 = [("node:shape", "circle"), ("edge:len", "1.5"), ("edge:fontsize", "7")]
  in (e12, o12, "I")

g13 :: G
g13 =
  let d13' = ("B3", words "U D D U R DDL UUL R (07/C3) R UU DDL L UU DDR (11/C3)")
      d13 = second (filter Direction.is_direction) d13'
      c13 = Direction.dir_seq_to_cell_seq d13
      e13 = List.adj2 1 c13
      o13 = [("node:shape", "circle"), ("edge:len", "1.5"), ("edge:fontsize", "7")]
  in (e13, o13, "J")

g_all :: [G]
g_all = [g1, g2, g4, g6, g8, g9, g10, g11, g12, g13]

-- G = unlabeled, GL = labeled
-- GC = collated, GF = filtered (unique edges)
-- GD = directed
wr :: G -> IO ()
wr (e, o, nm) = do
  let mk_nm ty = "/home/rohan/sw/hmt/data/dot/deacon/" ++ nm ++ "_" ++ ty ++ ".dot"
      wr_f ty g = writeFile (mk_nm ty) (unlines g)
  wr_f "G" (gen_graph_ul o id e)
  wr_f "GL" (gen_graph o (Dot.gr_pp_label id show) (Fgl.e_label_seq e))
  wr_f "GC" (gen_graph o (Dot.gr_pp_label id Dot.br_csl_pp) (Fgl.e_collate_normalised_l (Fgl.e_label_seq e)))
  wr_f "GF" (gen_graph_ul o id (nub (map Tuple.t2_sort e)))
  wr_f "GD" (gen_digraph o (Dot.gr_pp_label id Dot.br_csl_pp) (Fgl.e_collate_normalised_l (Fgl.e_label_seq e)))

{-
  let o' = ("graph:layout","fdp") : o
  wr_f "GC_" (gen_graph o' List.gr_pp_id_br_csl (List.e_collate_normalised_l (List.e_label_seq e)))
-}

wr_all :: IO ()
wr_all = mapM_ wr g_all
