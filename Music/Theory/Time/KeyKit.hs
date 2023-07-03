{- | A sequence structure, courtesy <https://github.com/nosuchtim/keykit>.

A /note/ has a time, a duration and a value.
A /phrase/ is a time-ascending sequence of notes and a /length/.
The length of a phrase is independent of the contents.
The sequence operator, /phrase_append/, sums phrase lengths.
The parallel operator, /phrase_merge/, selects the longer length.

Operations are ordinarily on phrases, notes are operated on indirectly.
The phrase indexing operation, /phrase_at/ returns a phrase of degree one.
-}
module Music.Theory.Time.KeyKit where

import Data.List {- base -}

import qualified Data.List.Ordered {- data-ordlist -}

import Music.Theory.Time {- hmt -}
import qualified Music.Theory.Time.Seq as Seq {- hmt -}

-- * Time

type Length = Time

-- * Note

data Note t =
  Note { note_start_time :: Time, note_duration :: Duration, note_value :: t }
  deriving (Eq, Ord, Show)

note_end_time :: Note t -> Time
note_end_time n = note_start_time n + note_duration n

note_region :: Note t -> (Time, Time)
note_region n = (note_start_time n, note_end_time n)

note_shift_time :: Time -> Note t -> Note t
note_shift_time k (Note t d e) = Note (t + k) d e

note_scale_duration :: Time -> Note t -> Note t
note_scale_duration m (Note t d e) = Note t (d * m) e

note_scale_duration_and_time :: Time -> Note t -> Note t
note_scale_duration_and_time m (Note t d e) = Note (t * m) (d * m) e

note_is_start_in_region :: (Time, Time) -> Note t -> Bool
note_is_start_in_region (t1, t2) (Note t _ _) = t >= t1 && t < t2

note_is_entirely_in_region :: (Time, Time) -> Note t -> Bool
note_is_entirely_in_region (t1, t2) (Note t d _) = t >= t1 && (t + d) < t2

-- * Phrase

-- | It is an un-checked invariant that the note list is in ascending order.
data Phrase t =
  Phrase { phrase_notes :: [Note t], phrase_length :: Length }
  deriving (Eq, Ord, Show)

phrase_values :: Phrase t -> [t]
phrase_values = map note_value . phrase_notes

phrase_set_length :: Phrase t -> Length -> Phrase t
phrase_set_length (Phrase n _) l = Phrase n l

phrase_degree :: Phrase t -> Int
phrase_degree (Phrase n _) = length n

phrase_start_time :: Phrase t -> Time
phrase_start_time (Phrase n _) =
  case n of
    [] -> 0
    n1 : _ -> note_start_time n1

phrase_end_time :: Phrase t -> Time
phrase_end_time (Phrase n _) =
  case n of
    [] -> 0
    _ -> note_start_time (last n)

phrase_duration :: Phrase t -> Duration
phrase_duration p = phrase_end_time p - phrase_start_time p

phrase_maximum :: Ord t => Phrase t -> Note t
phrase_maximum (Phrase n _) = maximum n

phrase_minimum :: Ord t => Phrase t -> Note t
phrase_minimum (Phrase n _) = minimum n

-- | Keykit sets the length to the duration, i.e. ('c,e,g'%2).length is 192.
phrase_at :: Phrase t -> Int -> Phrase t
phrase_at (Phrase n _) k =
  let nt = n !! (k - 1)
  in Phrase [nt] (note_start_time nt + note_duration nt)

phrase_time_at :: Phrase t -> Int -> Time
phrase_time_at (Phrase n _) k = note_start_time (n !! (k - 1))

phrase_clear_at :: Phrase t -> Int -> Phrase t
phrase_clear_at (Phrase n l) k =
  let remove_ix ix list = let (p,q) = splitAt ix list in p ++ tail q
  in Phrase (remove_ix (k - 1) n) l

phrase_at_put :: Ord t => Phrase t -> Int -> Phrase t -> Phrase t
phrase_at_put (Phrase n1 l1) k (Phrase n2 _) =
  let nt = n1 !! (k - 1)
      remove_ix ix list = let (p,q) = splitAt ix list in p ++ tail q
  in Phrase (Data.List.Ordered.merge (remove_ix (k - 1) n1) (map (note_shift_time (note_start_time nt)) n2)) l1

phrase_is_empty :: Phrase t -> Bool
phrase_is_empty (Phrase n _) = null n

-- | KeyKits p+q
phrase_append :: Ord t => Phrase t -> Phrase t -> Phrase t
phrase_append (Phrase n1 l1) (Phrase n2 l2) = Phrase (Data.List.Ordered.merge n1 (map (note_shift_time l1) n2)) (l1 + l2)

phrase_append_list :: Ord t => [Phrase t] -> Phrase t
phrase_append_list = foldl1' phrase_append

-- | KeyKits p|q
phrase_merge :: Ord t => Phrase t -> Phrase t -> Phrase t
phrase_merge (Phrase n1 l1) (Phrase n2 l2) = Phrase (Data.List.Ordered.merge n1 n2) (max l1 l2)

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
phrase_select_region p r = phrase_select p (note_is_start_in_region r)

phrase_clear_region :: Phrase t -> (Time, Time) -> Phrase t
phrase_clear_region p r = phrase_select p (not . note_is_start_in_region r)

phrase_select_indices :: Phrase t -> (Int, Int) -> Phrase t
phrase_select_indices (Phrase n l) (i, j) = Phrase (take (j - i + 1) (drop (i - 1) n)) l

phrase_clear_indices :: Phrase t -> (Int, Int) -> Phrase t
phrase_clear_indices (Phrase n l) (i, j) = Phrase (take (i - 1) n ++ drop j n) l

phrase_extract_region :: Phrase t -> (Time, Time) -> Phrase t
phrase_extract_region p (t1, t2) =
  let p' = phrase_select_region p (t1, t2)
  in phrase_set_length (phrase_shift p' (0 - t1)) (t2 - t1)

phrase_delete_region :: Ord t => Phrase t -> (Time, Time) -> Phrase t
phrase_delete_region p (t1, t2) =
  phrase_append
  (phrase_extract_region p (0, t1))
  (phrase_extract_region p (t2, phrase_length p))

phrase_separate :: Phrase t -> Time -> (Phrase t, Phrase t)
phrase_separate p t =
  let (p1, p2) = phrase_partition p (note_is_start_in_region (0, t))
      p1' = phrase_set_length p1 t
      p2' = phrase_set_length (phrase_shift p2 (0 - t)) (phrase_length p - t)
  in (p1', p2')

phrase_reverse :: Phrase t -> Phrase t
phrase_reverse (Phrase n l) =
  let f (Note t d e) = Note (l - t - d) d e
  in Phrase (reverse (map f n)) l

phrase_reorder :: Phrase t -> [Int] -> Phrase t
phrase_reorder (Phrase n l) p =
  let f (Note t d _) i = Note t d (note_value (n !! (i - 1)))
  in Phrase (zipWith f n p) l

phrase_truncate :: Phrase t -> Phrase t
phrase_truncate p = phrase_set_length p (phrase_end_time p)

phrase_trim :: Phrase t -> Phrase t
phrase_trim p =
  let t = phrase_start_time p
  in phrase_truncate (phrase_shift p (0 - t))

-- * Functor

note_map :: (t -> u) -> Note t -> Note u
note_map f (Note t d e) = Note t d (f e)

phrase_value_map :: (t -> u) -> Phrase t -> Phrase u
phrase_value_map f (Phrase n l) = Phrase (map (note_map f) n) l

phrase_note_map :: (Note t -> Note u) -> Phrase t -> Phrase u
phrase_note_map f (Phrase n l) = Phrase (map f n) l

phrase_phrase_map :: Ord u => (Phrase t -> Phrase u) -> Phrase t -> Phrase u
phrase_phrase_map f (Phrase n l) =
  let g (Note t d e) = f (Phrase [Note t d e] (t + d))
  in Phrase (sort (concatMap phrase_notes (map g n))) l

phrase_map :: Ord u => (Note t -> Phrase u) -> Phrase t -> Phrase u
phrase_map f (Phrase n l) = Phrase (sort (concatMap phrase_notes (map f n))) l

phrase_shift :: Phrase t -> Time -> Phrase t
phrase_shift p t = phrase_note_map (note_shift_time t) p

phrase_scale_duration :: Phrase t -> Time -> Phrase t
phrase_scale_duration p m = phrase_note_map (note_scale_duration m) p

phrase_scale_duration_and_time :: Phrase t -> Time -> Phrase t
phrase_scale_duration_and_time p m = phrase_note_map (note_scale_duration_and_time m) p

phrase_scale_to_duration :: Phrase t -> Duration -> Phrase t
phrase_scale_to_duration p d = phrase_scale_duration_and_time p (d / phrase_length p)

phrase_scale_to_region :: Phrase t -> (Time, Duration) -> Phrase t
phrase_scale_to_region p (t1, t2) = phrase_shift (phrase_scale_to_duration p (t2 - t1)) t1

-- * Seq

phrase_to_wseq :: Phrase t -> Seq.Wseq Time t
phrase_to_wseq (Phrase n _) =
  let f (Note tm dur e) = ((tm, dur), e)
  in map f n

useq_to_phrase :: Seq.Useq Time t -> Phrase t
useq_to_phrase = dseq_to_phrase . Seq.useq_to_dseq

dseq_to_phrase :: Seq.Dseq Time t -> Phrase t
dseq_to_phrase = wseq_to_phrase . Seq.dseq_to_wseq 0

wseq_to_phrase :: Seq.Wseq Time t -> Phrase t
wseq_to_phrase sq =
  let f ((t, d), e) = Note t d e
  in Phrase (map f sq) (Seq.wseq_dur sq)
