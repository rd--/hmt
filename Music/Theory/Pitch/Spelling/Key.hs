module Music.Theory.Pitch.Spelling.Key where

import qualified Music.Theory.Key as T {- hmt -}
import qualified Music.Theory.Pitch as T {- hmt -}
import qualified Music.Theory.Pitch.Note as T {- hmt -}
import qualified Music.Theory.Pitch.Spelling.Table as T {- hmt -}

pcset_spell_implied_key_f :: Integral i => [i] -> Maybe (T.Spelling i)
pcset_spell_implied_key_f x =
    case T.implied_fifths T.Major_Mode x of
      Nothing -> Nothing
      Just n -> if n == 0
                then Just T.pc_spell_natural
                else if n < 0
                     then Just T.pc_spell_flat
                     else Just T.pc_spell_sharp

-- > map pcset_spell_implied_key [[0,1],[4,10],[3,9],[3,11]]
pcset_spell_implied_key :: Integral i => [i] -> Maybe [(T.Note, T.Alteration)]
pcset_spell_implied_key x =
    case pcset_spell_implied_key_f x of
      Just f -> Just (map f x)
      Nothing -> Nothing

-- > map octpc_spell_implied_key [[(3,11),(4,1)],[(3,11),(4,10)]]
octpc_spell_implied_key :: [T.OctPc] -> Maybe [T.Pitch]
octpc_spell_implied_key x =
    let f o (n,a) = T.Pitch n a o
    in fmap (zipWith f (map fst x)) (pcset_spell_implied_key (map snd x))

-- > map (fmap (map T.pitch_pp_iso) . midi_spell_implied_key) [[59,61],[59,70]]
midi_spell_implied_key :: [T.Midi] -> Maybe [T.Pitch]
midi_spell_implied_key = octpc_spell_implied_key . map T.midi_to_octpc
