{- | LGL = Large Graph Layout (NCOL, LGL)

<http://lgl.sourceforge.net/#FileFormat>
-}
module Music.Theory.Graph.LGL where

import qualified Music.Theory.Show as T {- hmt -}

-- | (edge,weight)
type NCOL_ENT t = ((t,t),Maybe Double)

-- | [ncol-entry]
type NCOL t = [NCOL_ENT t]

-- | Parse 'NCOL_ENT' from 'String'
ncol_parse :: Read t => String -> NCOL_ENT t
ncol_parse s =
  case words s of
    [i,j] -> ((read i,read j),Nothing)
    [i,j,k] -> ((read i,read j),read k)
    _ -> error "ncol_parse"

-- | Load 'NCOL' from .ncol file.
ncol_load :: Read t => FilePath -> IO (NCOL t)
ncol_load = fmap (map ncol_parse . lines) . readFile

-- | Type-specialised.
ncol_load_int :: FilePath -> IO (NCOL Int)
ncol_load_int = ncol_load

-- > ncol_ent_format 4 ((0,1),Nothing) == "0 1"
-- > ncol_ent_format 4 ((0,1),Just 2.0) == "0 1 2.0000"
ncol_ent_format :: Show t => Int -> NCOL_ENT t -> String
ncol_ent_format k ((i,j),w) = unwords (map show [i,j]) ++ maybe "" ((' ':) . T.double_pp k) w

-- | Store 'NCOL' of 'Int' to .ncol file
ncol_store :: Show t => Int -> FilePath -> NCOL t -> IO ()
ncol_store k fn dat = writeFile fn (unlines (map (ncol_ent_format k) dat))

-- | Type-specialised.
ncol_store_int :: Int -> FilePath -> NCOL Int -> IO ()
ncol_store_int k = ncol_store k

-- | Store edge set to .ncol file
ncol_store_eset :: Show t => FilePath -> [(t,t)] -> IO ()
ncol_store_eset fn dat = ncol_store 0 fn (map (\e -> (e,Nothing)) dat)
