-- | Translations of some functions from <https://github.com/nosuchtim/keykit/blob/master/lib/basic1.k>
module Music.Theory.Time.KeyKit.Basic where

import Data.List {- base -}

import qualified Music.Theory.List as List {- hmt-base -}

import Music.Theory.Time {- hmt -}
import Music.Theory.Time.KeyKit {- hmt -}

{- | Returns an arpeggiated version of the phrase.
One way of describing desc it is that all the notes have been separated and then put back together, back-to-back.

> phrase_arpeggio (wseq_to_phrase (zip (repeat (0,1)) [60, 64, 67]))
-}
phrase_arpeggio :: Phrase t -> Phrase t
phrase_arpeggio (Phrase n l) =
  case n of
    [] -> Phrase n l
    n1 : _ ->
      let t_seq = scanl (+) (note_start_time n1) (map note_duration n)
          n' = zipWith (\t (Note _ d e) -> Note t d e) t_seq n
          l' = note_end_time (last n)
      in Phrase n' l'

-- | Return phrase ph echoed num times, with rtime delay between each echo.
phrase_echo :: Ord t => Phrase t -> Int -> Time -> Phrase t
phrase_echo p n t = phrase_merge_list (map (\i -> phrase_shift p (fromIntegral i * t)) [0 .. n - 1])

{- | Convert a phrase to be in step time, ie. all notes with the same spacing and duration.
Overlapped notes (no matter how small the overlap) are played at the same time.

> phrase_step (wseq_to_phrase [((0, 1), 60), ((5, 2), 64), ((23, 3), 67)]) 1
-}
phrase_step :: Phrase t -> Duration -> Phrase t
phrase_step (Phrase n _) d =
  let g = groupBy (\i j -> note_start_time i == note_start_time j) n
      f l t = map (\(Note _ _ e) -> Note t d e) l
      n' = concat (zipWith f g [0, d ..])
  in Phrase n' (note_end_time (last n'))

{- | This function takes a phrase, splits in in 2 halves (along time) and shuffles the result
(ie. first a note from the first half, then a note from the second half, etc.).
The timing of the original phrase is applied to the result.

> phrase_to_wseq (phrase_shuffle (useq_to_phrase (1,[1..9])))
-}
phrase_shuffle :: Phrase t -> Phrase t
phrase_shuffle (Phrase n l) =
  let (lhs, rhs) = List.split_into_halves (map note_value n)
      f (Note t d _) e = Note t d e
      n' = zipWith f n (concat (transpose [lhs, rhs]))
  in Phrase n' l
