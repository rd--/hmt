-- | Simple table based spelling rules for common music notation.
module Music.Theory.Pitch.Spelling.Table where

import Music.Theory.Pitch.Note {- hmt -}

type Spelling_Table i = [(i,(Note,Alteration))]

-- | Spelling table for natural (♮) notes only.
pc_spell_natural_tbl :: Integral i => Spelling_Table i
pc_spell_natural_tbl =
    [(0,(C,Natural))
    ,(2,(D,Natural))
    ,(4,(E,Natural))
    ,(5,(F,Natural))
    ,(7,(G,Natural))
    ,(9,(A,Natural))
    ,(11,(B,Natural))]

-- | Spelling table for sharp (♯) notes only.
pc_spell_sharp_tbl :: Integral i => Spelling_Table i
pc_spell_sharp_tbl =
    [(1,(C,Sharp))
    ,(3,(D,Sharp))
    ,(6,(F,Sharp))
    ,(8,(G,Sharp))
    ,(10,(A,Sharp))]

-- | Spelling table for flat (♭) notes only.
pc_spell_flat_tbl :: Integral i => Spelling_Table i
pc_spell_flat_tbl =
    [(1,(D,Flat))
    ,(3,(E,Flat))
    ,(6,(G,Flat))
    ,(8,(A,Flat))
    ,(10,(B,Flat))]

-- | Spelling table from simplest key-signature.  Note that this is
-- ambiguous for @8@, which could be either G Sharp (♯) in /A Major/
-- or A Flat (♭) in /E Flat (♭) Major/.
pc_spell_ks_tbl :: Integral i => Spelling_Table i
pc_spell_ks_tbl =
      [(1,(C,Sharp)) -- 2♯
      ,(3,(E,Flat)) -- 3♭
      ,(6,(F,Sharp)) -- 1♯
      ,(8,(A,Flat)) -- 3♭/3♯
      ,(10,(B,Flat))] -- 1♭
