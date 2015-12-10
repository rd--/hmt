module Music.Theory.DB.Common where

import Data.List {- base -}
import Safe {- safe -}

import qualified Music.Theory.List as T {- base -}

type Entry k v = (k,v)
type Record k v = [Entry k v]
type DB k v = [Record k v]

record_key_seq :: Record k v -> [k]
record_key_seq = map fst

record_key_histogram :: Ord k => Record k v -> [(k,Int)]
record_key_histogram = T.histogram . record_key_seq

record_lookup_by :: (k -> k -> Bool) -> k -> Record k v -> [v]
record_lookup_by f c = map snd . filter (f c . fst)

-- | Lookup all values for key at 'Record'.
record_lookup :: Eq k => k -> Record k v -> [v]
record_lookup = record_lookup_by (==)

-- | /n/th element of 'record_lookup'.
record_lookup_at :: Eq k => (k,Int) -> Record k v -> Maybe v
record_lookup_at (c,n) = flip atMay n . record_lookup c

db_keys_set :: Ord k => DB k v -> [k]
db_keys_set = nub . sort . map fst . concat
