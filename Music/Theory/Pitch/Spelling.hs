module Music.Theory.Pitch.Spelling where

import Music.Theory.Pitch

pc_spell_natural :: Spelling
pc_spell_natural pc =
    case pc of
      0 -> (C,Natural)
      2 -> (D,Natural)
      4 -> (E,Natural)
      5 -> (F,Natural)
      7 -> (G,Natural)
      9 -> (A,Natural)
      11 -> (B,Natural)
      _ -> error ("pc_spell_natural: " ++ show pc)

-- use spelling from simplest key-signature
-- ambiguous for 8 (G#/Ab)
pc_spell_ks :: Spelling
pc_spell_ks pc =
    case pc of
      1 -> (C,Sharp) -- 2#
      3 -> (E,Flat) -- 3b
      6 -> (F,Sharp) -- 1#
      8 -> (A,Flat) -- 3b/3#
      10 -> (B,Flat) -- 1b
      _ -> pc_spell_natural pc

pc_spell_sharp :: Spelling
pc_spell_sharp pc =
    case pc of
      1 -> (C,Sharp)
      3 -> (D,Sharp)
      6 -> (F,Sharp)
      8 -> (G,Sharp)
      10 -> (A,Sharp)
      _ -> pc_spell_natural pc

pc_spell_flat :: Spelling
pc_spell_flat pc =
    case pc of
      1 -> (D,Flat)
      3 -> (E,Flat)
      6 -> (G,Flat)
      8 -> (A,Flat)
      10 -> (B,Flat)
      _ -> pc_spell_natural pc

{-
map pc_spell_ks [0..11]
-}
