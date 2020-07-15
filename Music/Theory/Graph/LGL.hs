{- | LGL = Large Graph Layout (NCOL, LGL)

<http://lgl.sourceforge.net/#FileFormat>
-}
module Music.Theory.Graph.LGL where

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

-- | Store 'NCOL' of 'Int' to .ncol file
ncol_store :: Show t => FilePath -> NCOL t -> IO ()
ncol_store fn dat = do
  let f ((i,j),w) = unwords (map show [i,j]) ++ maybe "" show w
  writeFile fn (unlines (map f dat))

-- | Type-specialised.
ncol_store_int :: FilePath -> NCOL Int -> IO ()
ncol_store_int = ncol_store

-- | Store edge set to .ncol file
ncol_store_eset :: Show t => FilePath -> [(t,t)] -> IO ()
ncol_store_eset fn dat = ncol_store fn (map (\e -> (e,Nothing)) dat)
