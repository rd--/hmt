-- | Common music notation dynamic marks.
module Music.Theory.Dynamic_Mark where

import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Text.Read {- base -}

import qualified Music.Theory.List as List {- hmt-base -}

-- | Enumeration of dynamic mark symbols.
data Dynamic_Mark
  = Niente
  | Ppppp
  | Pppp
  | Ppp
  | Pp
  | P
  | Mp
  | Mf
  | F
  | Ff
  | Fff
  | Ffff
  | Fffff
  | Fp
  | Sf
  | Sfp
  | Sfpp
  | Sfz
  | Sffz
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

{- | Case insensitive reader for 'Dynamic_Mark'.

>>> map dynamic_mark_t_parse_ci (words "pP p Mp F")
[Just Pp,Just P,Just Mp,Just F]
-}
dynamic_mark_t_parse_ci :: String -> Maybe Dynamic_Mark
dynamic_mark_t_parse_ci =
  let capitalise x = toUpper (List.head_err x) : map toLower (List.tail_err x)
  in readMaybe . capitalise

{- | Lookup Midi velocity for 'Dynamic_Mark'.  The range is linear in @0-127@.

>>> mapMaybe dynamic_mark_midi [Niente .. Fffff]
[0,6,17,28,39,50,61,72,83,94,105,116,127]

>>> mapMaybe dynamic_mark_midi [Pp .. Ff]
[39,50,61,72,83,94]

>>> map dynamic_mark_midi [Fp,Sf,Sfp,Sfpp,Sfz,Sffz] == replicate 6 Nothing
True
-}
dynamic_mark_midi :: (Num n, Enum n) => Dynamic_Mark -> Maybe n
dynamic_mark_midi m =
  let r = zip [0 ..] (0 : reverse [127, 127 - 11 .. 0])
  in lookup (fromEnum m) r

-- | Error variant.
dynamic_mark_midi_err :: Integral n => Dynamic_Mark -> n
dynamic_mark_midi_err = fromMaybe (error "dynamic_mark_midi") . dynamic_mark_midi

{- | Map midi velocity (0-127) to dynamic mark.

>>> List.histogram (mapMaybe midi_dynamic_mark [0 .. 127])
[(Niente,1),(Ppppp,12),(Pppp,12),(Ppp,12),(Pp,12),(P,12),(Mp,12),(Mf,12),(F,12),(Ff,12),(Fff,12),(Ffff,7)]
-}
midi_dynamic_mark :: (Ord n, Num n, Enum n) => n -> Maybe Dynamic_Mark
midi_dynamic_mark m =
  let r = zip (0 : [12, 24 .. 132]) [0 ..]
  in fmap (toEnum . snd) (find ((>= m) . fst) r)

{- | Translate /fixed/ 'Dynamic_Mark's to /db/ amplitude over given /range/.

>>> mapMaybe (dynamic_mark_db 120) [Niente,P,F,Fffff]
[-120.0,-70.0,-40.0,0.0]

>>> mapMaybe (dynamic_mark_db 60) [Niente,P,F,Fffff]
[-60.0,-35.0,-20.0,0.0]
-}
dynamic_mark_db :: Fractional n => n -> Dynamic_Mark -> Maybe n
dynamic_mark_db r m =
  let u = [Niente .. Fffff]
      n = length u - 1
      k = r / fromIntegral n
      f i = negate r + (fromIntegral i * k)
  in fmap f (elemIndex m u)

-- | Enumeration of hairpin indicators.
data Hairpin = Crescendo | Diminuendo | End_Hairpin
  deriving (Eq, Ord, Enum, Bounded, Show)

{- | The 'Hairpin' implied by a ordered pair of 'Dynamic_Mark's.

>>> map (implied_hairpin Mf) [Mp,F]
[Just Diminuendo,Just Crescendo]
-}
implied_hairpin :: Dynamic_Mark -> Dynamic_Mark -> Maybe Hairpin
implied_hairpin p q =
  case compare p q of
    LT -> Just Crescendo
    EQ -> Nothing
    GT -> Just Diminuendo

-- | A node in a dynamic sequence.
type Dynamic_Node = (Maybe Dynamic_Mark, Maybe Hairpin)

-- | The empty 'Dynamic_Node'.
empty_dynamic_node :: Dynamic_Node
empty_dynamic_node = (Nothing, Nothing)

{- | Calculate a 'Dynamic_Node' sequence from a sequence of 'Dynamic_Mark's.

>>> dynamic_sequence [Pp,Mp,Mp,Pp]
[(Just Pp,Just Crescendo),(Just Mp,Just End_Hairpin),(Nothing,Just Diminuendo),(Just Pp,Just End_Hairpin)]
-}
dynamic_sequence :: [Dynamic_Mark] -> [Dynamic_Node]
dynamic_sequence d =
  let h = zipWith implied_hairpin d (List.tail_err d) ++ [Nothing]
      e = Just End_Hairpin
      rec i p =
        case p of
          [] -> []
          [(j, _)] -> if i then [(j, e)] else [(j, Nothing)]
          (j, k) : p' -> case k of
            Nothing ->
              if i
                then (j, e) : rec False p'
                else (j, k) : rec False p'
            Just _ -> (j, k) : rec True p'
  in rec False (zip (List.indicate_repetitions d) h)

{- | Delete redundant (unaltered) dynamic marks.

>>> delete_redundant_marks [Just P,Nothing,Just P,Just P,Just F]
[Just P,Nothing,Nothing,Nothing,Just F]
-}
delete_redundant_marks :: [Maybe Dynamic_Mark] -> [Maybe Dynamic_Mark]
delete_redundant_marks =
  let f i j = case (i, j) of
        (Just a, Just b) -> if a == b then (j, Nothing) else (j, j)
        (Just _, Nothing) -> (i, Nothing)
        (Nothing, _) -> (j, j)
  in snd . mapAccumL f Nothing

{- | Variant of 'dynamic_sequence' for sequences of 'Dynamic_Mark' with holes (ie. rests).
Runs 'delete_redundant_marks'.

>>> dynamic_sequence_sets [Just P,Just F,Nothing,Just P]
[Just (Just P,Just Crescendo),Just (Just F,Just End_Hairpin),Nothing,Just (Just P,Nothing)]

>>> dynamic_sequence_sets [Just P,Nothing,Just P]
[Just (Just P,Nothing),Nothing,Nothing]
-}
dynamic_sequence_sets :: [Maybe Dynamic_Mark] -> [Maybe Dynamic_Node]
dynamic_sequence_sets =
  let f l = case l of
        Nothing : _ -> map (const Nothing) l
        _ -> map Just (dynamic_sequence (catMaybes l))
  in concatMap f . List.group_just . delete_redundant_marks

{- | Apply 'Hairpin' and 'Dynamic_Mark' functions in that order as required by 'Dynamic_Node'.

>>> let f _ x = show x
>>> apply_dynamic_node f f (Nothing,Just Crescendo) undefined
"Crescendo"
-}
apply_dynamic_node :: (a -> Dynamic_Mark -> a) -> (a -> Hairpin -> a) -> Dynamic_Node -> a -> a
apply_dynamic_node f g (i, j) m =
  let n = maybe m (g m) j
  in maybe n (f n) i

-- * Ascii

-- | Ascii pretty printer for 'Dynamic_Mark'.
dynamic_mark_ascii :: Dynamic_Mark -> String
dynamic_mark_ascii = map toLower . show

-- | Ascii pretty printer for 'Hairpin'.
hairpin_ascii :: Hairpin -> String
hairpin_ascii hp =
  case hp of
    Crescendo -> "<"
    Diminuendo -> ">"
    End_Hairpin -> ""

-- | Ascii pretty printer for 'Dynamic_Node'.
dynamic_node_ascii :: Dynamic_Node -> String
dynamic_node_ascii (mk, hp) =
  let mk' = maybe "" dynamic_mark_ascii mk
      hp' = maybe "" hairpin_ascii hp
  in case (mk', hp') of
      ([], []) -> []
      ([], _) -> hp'
      (_, []) -> mk'
      _ -> mk' ++ " " ++ hp'

-- | Ascii pretty printer for 'Dynamic_Node' sequence.
dynamic_sequence_ascii :: [Dynamic_Node] -> String
dynamic_sequence_ascii =
  unwords
    . filter (not . null)
    . map dynamic_node_ascii
