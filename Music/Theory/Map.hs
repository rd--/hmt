-- | Map functions.
module Music.Theory.Map where

import qualified Data.Map as M {- containers -}
import Data.Maybe {- base -}

-- | Erroring 'M.lookup'.
map_lookup_err :: Ord k => k -> M.Map k c -> c
map_lookup_err k = fromMaybe (error "M.lookup") . M.lookup k

-- | 'flip' of 'M.lookup'.
map_ix :: Ord k => M.Map k c -> k -> Maybe c
map_ix = flip M.lookup

-- | 'flip' of 'map_lookup_err'.
map_ix_err :: Ord k => M.Map k c -> k -> c
map_ix_err = flip map_lookup_err
