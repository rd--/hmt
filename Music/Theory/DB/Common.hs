module Music.Theory.DB.Common where

import Data.List {- base -}
import Safe {- safe -}

import qualified Music.Theory.List as T {- base -}
import qualified Music.Theory.Maybe as T {- base -}

type Entry k v = (k,v)
type Record k v = [Entry k v]
type DB k v = [Record k v]

type Key = String
type Value = String
type Entry' = Entry Key Value
type Record' = Record Key Value
type DB' = DB Key Value

-- | The sequence of keys at 'Record'.
record_key_seq :: Record k v -> [k]
record_key_seq = map fst

-- | 'T.histogram' of 'record_key_seq'.
record_key_histogram :: Ord k => Record k v -> [(k,Int)]
record_key_histogram = T.histogram . record_key_seq

-- | Duplicate keys predicate.
record_has_duplicate_keys :: Ord k => Record k v -> Bool
record_has_duplicate_keys = any (> 0) . map snd . record_key_histogram

-- | Find all associations for key using given equality function.
record_lookup_by :: (k -> k -> Bool) -> k -> Record k v -> [v]
record_lookup_by f c = map snd . filter (f c . fst)

-- | Lookup all values for key at 'Record' using '=='.
record_lookup :: Eq k => k -> Record k v -> [v]
record_lookup k = record_lookup_by (==) k

-- | /n/th element of 'record_lookup'.
record_lookup_at :: Eq k => (k,Int) -> Record k v -> Maybe v
record_lookup_at (c,n) = flip atMay n . record_lookup c

-- | Variant requiring unique key, error if multiples, 'Nothing' if none.
record_lookup_uniq :: Eq k => k -> Record k v -> Maybe v
record_lookup_uniq k r =
    case record_lookup k r of
      [] -> Nothing
      [v] -> Just v
      _ -> error "record_lookup_uniq: non uniq"

-- | Error variant.
record_lookup_uniq_err :: Eq k => k -> Record k c -> c
record_lookup_uniq_err k = T.from_just "record_lookup_uniq: none" . record_lookup_uniq k

-- | Preserves order of occurence.
db_key_set :: Ord k => DB k v -> [k]
db_key_set = nub . map fst . concat

db_lookup_by :: (k -> k -> Bool) -> (v -> v -> Bool) -> k -> v -> DB k v -> [Record k v]
db_lookup_by k_cmp v_cmp k v =
    let f = any (v_cmp v) . record_lookup_by k_cmp k
    in filter f

db_lookup :: (Eq k,Eq v) => k -> v -> DB k v -> [Record k v]
db_lookup = db_lookup_by (==) (==)

db_has_duplicate_keys :: Ord k => DB k v -> Bool
db_has_duplicate_keys = any id . map record_has_duplicate_keys

db_key_histogram :: Ord k => DB k v -> [(k,Int)]
db_key_histogram db =
    let h = concatMap record_key_histogram db
        f k = (k,maximum (record_lookup k h))
    in map f (db_key_set db)

db_to_table :: Ord k => (Maybe v -> e) -> DB k v -> ([k],[[e]])
db_to_table f db =
    let kh = db_key_histogram db
        hdr = concatMap (\(k,n) -> replicate n k) kh
        ix = concatMap (\(k,n) -> zip (repeat k) [0 .. n - 1]) kh
    in (hdr,map (\r -> map (\i -> f (record_lookup_at i r)) ix) db)

-- * Collating duplicate keys.

record_collate' :: Eq k => (k,[v]) -> Record k v -> Record k [v]
record_collate' (k,v) r =
    case r of
      [] -> [(k,reverse v)]
      (k',v'):r' ->
          if k == k'
          then record_collate' (k,v' : v) r'
          else (k,reverse v) : record_collate' (k',[v']) r'

-- | Collate adjacent entries of existing sequence with equal key.
record_collate :: Eq k => Record k v -> Record k [v]
record_collate r =
    case r of
      [] -> error "record_collate: nil"
      (k,v):r' -> record_collate' (k,[v]) r'

record_uncollate :: Eq k => Record k [v] -> Record k v
record_uncollate = concatMap (\(k,v) -> zip (repeat k) v)
