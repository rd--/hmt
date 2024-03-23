-- | Z12 set class database.
module Music.Theory.Z.Literature where

{- | Set class database with descriptors for historically and
theoretically significant set classes, indexed by Forte name.

>>> lookup "6-Z17" sc_db
Just "All-Trichord Hexachord"

>>> lookup "7-35" sc_db
Just "diatonic collection (d)"
-}
sc_db :: [(String, String)]
sc_db =
  [ ("4-Z15", "All-Interval Tetrachord (see also 4-Z29)")
  , ("4-Z29", "All-Interval Tetrachord (see also 4-Z15)")
  , ("6-Z17", "All-Trichord Hexachord")
  , ("8-Z15", "All-Tetrachord Octochord (see also 8-Z29)")
  , ("8-Z29", "All-Tetrachord Octochord (see also 8-Z15)")
  , ("6-1", "A-Type All-Combinatorial Hexachord")
  , ("6-8", "B-Type All-Combinatorial Hexachord")
  , ("6-32", "C-Type All-Combinatorial Hexachord")
  , ("6-7", "D-Type All-Combinatorial Hexachord")
  , ("6-20", "E-Type All-Combinatorial Hexachord")
  , ("6-35", "F-Type All-Combinatorial Hexachord")
  , ("7-35", "diatonic collection (d)")
  , ("7-34", "ascending melodic minor collection")
  , ("8-28", "octotonic collection (Messiaen Mode II)")
  , ("6-35", "wholetone collection")
  , ("3-10", "diminished triad")
  , ("3-11", "major/minor triad")
  , ("3-12", "augmented triad")
  , ("4-19", "minor major-seventh chord")
  , ("4-20", "major-seventh chord")
  , ("4-25", "french augmented sixth chord")
  , ("4-28", "dimished-seventh chord")
  , ("4-26", "minor-seventh chord")
  , ("4-27", "half-dimished seventh(P)/dominant-seventh(I) chord")
  , ("6-30", "Petrushka Chord {0476a1},3-11 at T6")
  , ("6-34", "Mystic Chord {06a492}")
  , ("6-Z44", "Schoenberg Signature Set,3-3 at T5 or T7")
  , ("6-Z19", "complement of 6-Z44,3-11 at T1 or TB")
  , ("9-12", "Messiaen Mode III (nontonic collection)")
  , ("8-9", "Messian Mode IV")
  , ("7-31", "The only seven-element subset of 8-28. ")
  , ("5-31", "The only five-element superset of 4-28.")
  , ("5-33", "The only five-element subset of 6-35.")
  , ("7-33", "The only seven-element superset of 6-35.")
  , ("5-21", "The only five-element subset of 6-20.")
  , ("7-21", "The only seven-element superset of 6-20.")
  , ("5-25", "The only five-element subset of both 7-35 and 8-28.")
  , ("6-14", "Any non-intersecting union of 3-6 and 3-12.")
  ]
