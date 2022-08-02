{- | A sequence structure, courtesy <https://github.com/nosuchtim/keykit>.

A /note/ has a time, a duration and a value (called an entry below).
A /phrase/ is a time-ascending sequence of notes and a /length/.
The length of a phrase is independent it's contents.
The sequence operator, /phrase_append/, sums phrase lengths.
The the parallel operator, /phrase_merge/, selects the longer length.

Operations are ordinarily on phrases, notes are operated on indirectly.
The phrase indexing operation, /phrase_at/ returns a phrase of degree one.
-}
module Music.Theory.Time.KeyKit where

import Data.List {- base -}

import qualified Data.List.Ordered as O {- data-ordlist -}

-- * Time

type Time = Rational
type Duration = Time
type Length = Time

-- * Note

data Note t =
  Note { note_time :: Time, note_duration :: Duration, note_entry :: t }
  deriving (Eq, Ord, Show)

note_shift_time :: Time -> Note t -> Note t
note_shift_time k (Note t d e) = Note (t + k) d e

note_scale_duration :: Time -> Note t -> Note t
note_scale_duration m (Note t d e) = Note t (d * m) e

note_start_in_region :: (Time, Time) -> Note t -> Bool
note_start_in_region (t1, t2) (Note t _ _) = t >= t1 && t < t2

note_entirely_in_region :: (Time, Time) -> Note t -> Bool
note_entirely_in_region (t1, t2) (Note t d _) = t >= t1 && (t + d) < t2

note_map :: (t -> u) -> Note t -> Note u
note_map f (Note t d e) = Note t d (f e)

-- * Phrase

-- | It is an un-checked invariant that the note list is in ascending order.
data Phrase t =
  Phrase { phrase_notes :: [Note t], phrase_length :: Length }
  deriving (Eq, Ord, Show)

phrase_entry_map :: (t -> u) -> Phrase t -> Phrase u
phrase_entry_map f (Phrase n l) = Phrase (map (note_map f) n) l

phrase_note_map :: (Note t -> Note u) -> Phrase t -> Phrase u
phrase_note_map f (Phrase n l) = Phrase (map f n) l

phrase_phrase_map :: Ord u => (Phrase t -> Phrase u) -> Phrase t -> Phrase u
phrase_phrase_map f (Phrase n l) =
  let g (Note t d e) = f (Phrase [Note t d e] (t + d))
  in Phrase (sort (concatMap phrase_notes (map g n))) l

phrase_map :: Ord u => (Note t -> Phrase u) -> Phrase t -> Phrase u
phrase_map f (Phrase n l) = Phrase (sort (concatMap phrase_notes (map f n))) l

phrase_set_length :: Phrase t -> Length -> Phrase t
phrase_set_length (Phrase n _) l = Phrase n l

phrase_degree :: Phrase t -> Int
phrase_degree (Phrase n _) = length n

phrase_start_time :: Phrase t -> Time
phrase_start_time (Phrase n _) =
  case n of
    [] -> 0
    n1 : _ -> note_time n1

phrase_end_time :: Phrase t -> Time
phrase_end_time (Phrase n _) =
  case n of
    [] -> 0
    _ -> note_time (last n)

phrase_duration :: Phrase t -> Duration
phrase_duration p = phrase_end_time p - phrase_start_time p

-- | Keykit sets the length to the duration, i.e. ('c,e,g'%2).length is 192.
phrase_at :: Phrase t -> Int -> Phrase t
phrase_at (Phrase n _) k =
  let nt = n !! (k - 1)
  in Phrase [nt] (note_time nt + note_duration nt)

phrase_time_at :: Phrase t -> Int -> Time
phrase_time_at (Phrase n _) k = note_time (n !! (k - 1))

phrase_delete_at :: Phrase t -> Int -> Phrase t
phrase_delete_at (Phrase n l) k =
  let remove_ix ix list = let (p,q) = splitAt ix list in p ++ tail q
  in Phrase (remove_ix (k - 1) n) l

phrase_at_put :: Ord t => Phrase t -> Int -> Phrase t -> Phrase t
phrase_at_put (Phrase n1 l1) k (Phrase n2 _) =
  let nt = n1 !! (k - 1)
      remove_ix ix list = let (p,q) = splitAt ix list in p ++ tail q
  in Phrase (O.merge (remove_ix (k - 1) n1) (map (note_shift_time (note_time nt)) n2)) l1

phrase_is_empty :: Phrase t -> Bool
phrase_is_empty (Phrase n _) = null n

phrase_append :: Ord t => Phrase t -> Phrase t -> Phrase t
phrase_append (Phrase n1 l1) (Phrase n2 l2) = Phrase (O.merge n1 (map (note_shift_time l1) n2)) (l1 + l2)

phrase_merge :: Ord t => Phrase t -> Phrase t -> Phrase t
phrase_merge (Phrase n1 l1) (Phrase n2 l2) = Phrase (O.merge n1 n2) (max l1 l2)

phrase_merge_list :: Ord t => [Phrase t] -> Phrase t
phrase_merge_list p =
  let l = maximum (map phrase_length p)
      n = sort (concatMap phrase_notes p)
  in Phrase n l

phrase_select :: Phrase t -> (Note t -> Bool) -> Phrase t
phrase_select (Phrase n l) f = Phrase (filter f n) l

phrase_partition :: Phrase t -> (Note t -> Bool) -> (Phrase t, Phrase t)
phrase_partition (Phrase n l) f =
  let (n1, n2) = partition f n
  in (Phrase n1 l, Phrase n2 l)

phrase_select_region :: Phrase t -> (Time, Time) -> Phrase t
phrase_select_region p r = phrase_select p (note_start_in_region r)

phrase_clear_region :: Phrase t -> (Time, Time) -> Phrase t
phrase_clear_region p r = phrase_select p (not . note_start_in_region r)

phrase_shift :: Phrase t -> Time -> Phrase t
phrase_shift (Phrase n l) t = Phrase (map (note_shift_time t) n) l

phrase_trim :: Phrase t -> Phrase t
phrase_trim p =
  let t = phrase_start_time p
      p' = phrase_shift p (0 - t)
  in phrase_set_length p' (phrase_duration p')

phrase_extract_region :: Phrase t -> (Time, Time) -> Phrase t
phrase_extract_region p (t1, t2) =
  let p' = phrase_select_region p (t1, t2)
  in phrase_set_length (phrase_shift p' (0 - t1)) (t2 - t1)

phrase_cut :: Phrase t -> Time -> (Phrase t, Phrase t)
phrase_cut p t =
  let (p1, p2) = phrase_partition p (note_start_in_region (0, t))
      p1' = phrase_set_length p1 t
      p2' = phrase_set_length (phrase_shift p2 (0 - t)) (phrase_length p - t)
  in (p1', p2')

phrase_reverse :: Phrase t -> Phrase t
phrase_reverse (Phrase n l) =
  let f (Note t d e) = Note (l - t - d) d e
  in Phrase (reverse (map f n)) l

phrase_shuffle :: Phrase t -> [Int] -> Phrase t
phrase_shuffle (Phrase n l) p =
  let f (Note t d _) i = Note t d (note_entry (n !! (i - 1)))
  in Phrase (zipWith f n p) l
