-- | Common music notation dynamic marks.
module Music.Theory.Dynamic_Mark where

import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.List as T

-- | Enumeration of dynamic mark symbols.
data Dynamic_Mark_T = Niente
                    | PPPPP | PPPP | PPP | PP | P | MP
                    | MF | F | FF | FFF | FFFF | FFFFF
                    | FP | SF | SFP | SFPP | SFZ | SFFZ
                      deriving (Eq,Ord,Enum,Bounded,Show)

-- | Lookup MIDI velocity for 'Dynamic_Mark_T'.  The range is linear
-- in @0-127@.
--
-- > let r = [0,6,17,28,39,50,61,72,83,94,105,116,127]
-- > in mapMaybe dynamic_mark_midi [Niente .. FFFFF] == r
--
-- > map dynamic_mark_midi [FP,SF,SFP,SFPP,SFZ,SFFZ] == replicate 6 Nothing
dynamic_mark_midi :: (Num n,Enum n) => Dynamic_Mark_T -> Maybe n
dynamic_mark_midi m =
    let r = zip [0..] (0 : reverse [127, 127-11 .. 0])
    in lookup (fromEnum m) r

-- | Map midi velocity (0-127) to dynamic mark.
--
-- > histogram (mapMaybe midi_dynamic_mark [0 .. 127])
midi_dynamic_mark :: (Ord n,Eq n,Num n,Enum n) => n -> Maybe Dynamic_Mark_T
midi_dynamic_mark m =
    let r = zip (0 : [12,24 .. 132]) [0..]
    in fmap (toEnum . snd) (find ((>= m) . fst) r)

-- | Translate /fixed/ 'Dynamic_Mark_T's to /db/ amplitude over given
-- /range/.
--
-- > mapMaybe (dynamic_mark_db 120) [Niente,P,F,FFFFF] == [-120,-70,-40,0]
-- > mapMaybe (dynamic_mark_db 60) [Niente,P,F,FFFFF] == [-60,-35,-20,0]
dynamic_mark_db :: Fractional n => n -> Dynamic_Mark_T -> Maybe n
dynamic_mark_db r m =
    let u = [Niente .. FFFFF]
        n = length u - 1
        k = r / fromIntegral n
        f i = negate r + (fromIntegral i * k)
    in fmap f (elemIndex m u)

-- | <http://www.csounds.com/manual/html/ampmidid.html>
--
-- > import Sound.SC3.Plot
-- > plotTable [map (ampmidid 20) [0 .. 127],map (ampmidid 60) [0 .. 127]]
ampmidid :: Floating a => a -> a -> a
ampmidid db v =
    let r = 10 ** (db / 20)
        b = 127 / (126 * sqrt r) - 1 / 126
        m = (1 - b) / 127
    in (m * v + b) ** 2

-- | JMcC (SC3) equation.
--
-- > plotTable1 (map amp_db [0,0.005 .. 1])
amp_db :: Floating a => a -> a
amp_db a = logBase 10 a * 20

-- | JMcC (SC3) equation.
--
-- > plotTable1 (map db_amp [-60,-59 .. 0])
db_amp :: Floating a => a -> a
db_amp a = 10 ** (a * 0.05)

-- | Enumeration of hairpin indicators.
data Hairpin_T = Crescendo | Diminuendo | End_Hairpin
                 deriving (Eq,Ord,Enum,Bounded,Show)

-- | The 'Hairpin_T' implied by a ordered pair of 'Dynamic_Mark_T's.
--
-- > map (implied_hairpin MF) [MP,F] == [Just Diminuendo,Just Crescendo]
implied_hairpin :: Dynamic_Mark_T -> Dynamic_Mark_T -> Maybe Hairpin_T
implied_hairpin p q =
    case compare p q of
      LT -> Just Crescendo
      EQ -> Nothing
      GT -> Just Diminuendo

-- | A node in a dynamic sequence.
type Dynamic_Node = (Maybe Dynamic_Mark_T,Maybe Hairpin_T)

-- | The empty 'Dynamic_Node'.
empty_dynamic_node :: Dynamic_Node
empty_dynamic_node = (Nothing,Nothing)

-- | Calculate a 'Dynamic_Node' sequence from a sequence of
-- 'Dynamic_Mark_T's.
--
-- > dynamic_sequence [PP,MP,MP,PP] == [(Just PP,Just Crescendo)
-- >                                   ,(Just MP,Just End_Hairpin)
-- >                                   ,(Nothing,Just Diminuendo)
-- >                                   ,(Just PP,Just End_Hairpin)]
dynamic_sequence :: [Dynamic_Mark_T] -> [Dynamic_Node]
dynamic_sequence d =
    let h = zipWith implied_hairpin d (tail d) ++ [Nothing]
        e = Just End_Hairpin
        rec i p =
            case p of
              [] -> []
              [(j,_)] -> if i then [(j,e)] else [(j,Nothing)]
              (j,k):p' -> case k of
                            Nothing -> if i
                                       then (j,e) : rec False p'
                                       else (j,k) : rec False p'
                            Just _ -> (j,k) : rec True p'
    in rec False (zip (T.indicate_repetitions d) h)

-- | Delete redundant (unaltered) dynamic marks.
--
-- > let s = [Just P,Nothing,Just P,Just P,Just F]
-- > in delete_redundant_marks s == [Just P,Nothing,Nothing,Nothing,Just F]
delete_redundant_marks :: [Maybe Dynamic_Mark_T] -> [Maybe Dynamic_Mark_T]
delete_redundant_marks =
    let f i j = case (i,j) of
                  (Just a,Just b) -> if a == b then (j,Nothing) else (j,j)
                  (Just _,Nothing) -> (i,Nothing)
                  (Nothing,_) -> (j,j)
    in snd . mapAccumL f Nothing

-- | Variant of 'dynamic_sequence' for sequences of 'Dynamic_Mark_T'
-- with holes (ie. rests).  Runs 'delete_redundant_marks'.
--
-- > let r = [Just (Just P,Just Crescendo),Just (Just F,Just End_Hairpin)
-- >         ,Nothing,Just (Just P,Nothing)]
-- > in dynamic_sequence_sets [Just P,Just F,Nothing,Just P] == r
--
-- > let s = [Just P,Nothing,Just P]
-- > in dynamic_sequence_sets s = [Just (Just P,Nothing),Nothing,Nothing]
dynamic_sequence_sets :: [Maybe Dynamic_Mark_T] -> [Maybe Dynamic_Node]
dynamic_sequence_sets =
    let f l = case l of
                Nothing:_ -> map (const Nothing) l
                _ -> map Just (dynamic_sequence (catMaybes l))
    in concatMap f . T.group_just . delete_redundant_marks

-- | Apply 'Hairpin_T' and 'Dynamic_Mark_T' functions in that order as
-- required by 'Dynamic_Node'.
--
-- > let f _ x = show x
-- > in apply_dynamic_node f f (Nothing,Just Crescendo) undefined
apply_dynamic_node :: (a -> Dynamic_Mark_T -> a) -> (a -> Hairpin_T -> a)
                   -> Dynamic_Node -> a -> a
apply_dynamic_node f g (i,j) m =
    let n = maybe m (g m) j
    in maybe n (f n) i

-- * ASCII

-- | ASCII pretty printer for 'Dynamic_Mark_T'.
dynamic_mark_ascii :: Dynamic_Mark_T -> String
dynamic_mark_ascii = map toLower . show

-- | ASCII pretty printer for 'Hairpin_T'.
hairpin_ascii :: Hairpin_T -> String
hairpin_ascii hp =
    case hp of
      Crescendo -> "<"
      Diminuendo -> ">"
      End_Hairpin -> ""

-- | ASCII pretty printer for 'Dynamic_Node'.
dynamic_node_ascii :: Dynamic_Node -> String
dynamic_node_ascii (mk,hp) =
    let mk' = maybe "" dynamic_mark_ascii mk
        hp' = maybe "" hairpin_ascii hp
    in case (mk',hp') of
         ([],[]) -> []
         ([],_) -> hp'
         (_,[]) -> mk'
         _ -> mk' ++ " " ++ hp'

-- | ASCII pretty printer for 'Dynamic_Node' sequence.
dynamic_sequence_ascii :: [Dynamic_Node] -> String
dynamic_sequence_ascii =
    intercalate " " .
    filter (not . null) .
    map dynamic_node_ascii
