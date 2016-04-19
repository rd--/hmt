-- | Graph (fgl) functions.
module Music.Theory.Graph.FGL where

import qualified Data.Graph.Inductive.Graph as G {- fgl -}
import qualified Data.Graph.Inductive.Query as G {- fgl -}
import qualified Data.Graph.Inductive.PatriciaTree as G {- fgl -}

import qualified Control.Monad.Logic as L {- logict -}

-- | 'L.msum' '.' 'map' 'return'.
ml_from_list :: L.MonadLogic m => [a] -> m a
ml_from_list = L.msum . map return

-- | Synonym for 'G.noNodes'.
g_degree :: G.Gr v e -> Int
g_degree = G.noNodes

-- | 'G.subgraph' of each 'G.components'.
--
-- > g_partition gr
g_partition :: G.Gr v e -> [G.Gr v e]
g_partition gr = map (\n -> G.subgraph n gr) (G.components gr)

type G_NODE_SEL_F v e = G.Gr v e -> G.Node -> [G.Node]

-- | Use /sel_f/ is 'G.pre' for directed graphs and 'G.neighbors' for undirected.
g_hamiltononian_path_ml :: L.MonadLogic m => G_NODE_SEL_F v e -> G.Gr v e -> G.Node -> m [G.Node]
g_hamiltononian_path_ml sel_f gr =
    let n_deg = g_degree gr
        recur r c =
            if length r == n_deg - 1
            then return (c:r)
            else do i <- ml_from_list (sel_f gr c)
                    L.guard (i `notElem` r)
                    recur (c:r) i
    in recur []

-- > map (L.observeAll . ug_hamiltononian_path_ml_0) (g_partition gr)
ug_hamiltononian_path_ml_0 :: L.MonadLogic m => G.Gr v e -> m [G.Node]
ug_hamiltononian_path_ml_0 gr = g_hamiltononian_path_ml G.neighbors gr (G.nodes gr !! 0)

