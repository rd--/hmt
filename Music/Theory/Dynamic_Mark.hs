-- | Common music notation dynamic marks.
module Music.Theory.Dynamic_Mark where

data Dynamic_Mark_T = PPPPP | PPPP | PPP | PP | P | MP
                    | MF | F | FF | FFF | FFFF | FFFFF
                    | FP | SF | SFP | SFPP | SFZ | SFFZ
                      deriving (Eq,Ord,Enum,Bounded,Show)
