module Music.Theory.Pitch.Spelling.Key where

import qualified Music.Theory.Key as Key {- hmt -}
import qualified Music.Theory.Pitch as Pitch {- hmt -}
import qualified Music.Theory.Pitch.Note as Pitch.Note {- hmt -}
import qualified Music.Theory.Pitch.Spelling.Table as Pitch.Spelling.Table {- hmt -}

pcset_spell_implied_key_f :: Integral i => [i] -> Maybe (Pitch.Spelling i)
pcset_spell_implied_key_f x =
    case Key.implied_fifths Key.Major_Mode x of
      Nothing -> Nothing
      Just n -> if n == 0
                then Just Pitch.Spelling.Table.pc_spell_natural
                else if n < 0
                     then Just Pitch.Spelling.Table.pc_spell_flat
                     else Just Pitch.Spelling.Table.pc_spell_sharp

{- | Implied key

>>> map pcset_spell_implied_key [[0,1],[4,10],[3,9],[3,11]]
[Just [(C,Natural),(D,Flat)],Just [(E,Natural),(B,Flat)],Just [(E,Flat),(A,Natural)],Just [(D,Sharp),(B,Natural)]]
-}
pcset_spell_implied_key :: Integral i => [i] -> Maybe [(Pitch.Note.Note, Pitch.Note.Alteration)]
pcset_spell_implied_key x =
    case pcset_spell_implied_key_f x of
      Just f -> Just (map f x)
      Nothing -> Nothing

{- | Implied key from octave pitch classes

>>> map (fmap (map Pitch.pitch_pp_iso) . octpc_spell_implied_key) [[(3,11),(4,1)],[(3,11),(4,10)]]
[Just ["B3","C#4"],Just ["B3","A#4"]]
-}
octpc_spell_implied_key :: [Pitch.OctPc] -> Maybe [Pitch.Pitch]
octpc_spell_implied_key x =
    let f o (n,a) = Pitch.Pitch n a o
    in fmap (zipWith f (map fst x)) (pcset_spell_implied_key (map snd x))

{- | Implied key from midi note numbers

>>> map (fmap (map Pitch.pitch_pp_iso) . midi_spell_implied_key) [[59,61],[59,70]]
[Just ["B3","C#4"],Just ["B3","A#4"]]
-}
midi_spell_implied_key :: [Pitch.Midi] -> Maybe [Pitch.Pitch]
midi_spell_implied_key = octpc_spell_implied_key . map Pitch.midi_to_octpc
