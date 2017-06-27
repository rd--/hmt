-- | Duration annotations.
module Music.Theory.Duration.Annotation where

import Data.Maybe {- base -}
import Data.Ratio {- base -}
import Data.Tree {- containers -}

import Music.Theory.Duration
import Music.Theory.Duration.RQ
import qualified Music.Theory.List as L {- hmt -}

-- | Standard music notation durational model annotations
data D_Annotation = Tie_Right
                  | Tie_Left
                  | Begin_Tuplet (Integer,Integer,Duration)
                  | End_Tuplet
                    deriving (Eq,Show)

-- | Annotated 'Duration'.
type Duration_A = (Duration,[D_Annotation])

begin_tuplet :: D_Annotation -> Maybe (Integer,Integer,Duration)
begin_tuplet a =
    case a of
      Begin_Tuplet t -> Just t
      _ -> Nothing

da_begin_tuplet :: Duration_A -> Maybe (Integer,Integer,Duration)
da_begin_tuplet (_,a) =
    case mapMaybe begin_tuplet a of
      [t] -> Just t
      _ -> Nothing

begins_tuplet :: D_Annotation -> Bool
begins_tuplet a =
    case a of
      Begin_Tuplet _ -> True
      _ -> False

-- | Does 'Duration_A' begin a tuplet?
da_begins_tuplet :: Duration_A -> Bool
da_begins_tuplet (_,a) = any begins_tuplet a

-- | Does 'Duration_A' end a tuplet?
da_ends_tuplet :: Duration_A -> Bool
da_ends_tuplet (_,a) = End_Tuplet `elem` a

-- | Is 'Duration_A' tied to the the right?
da_tied_right :: Duration_A -> Bool
da_tied_right = elem Tie_Right . snd

-- | Annotate a sequence of 'Duration_A' as a tuplet.
--
-- > import Music.Theory.Duration.Name
-- > da_tuplet (3,2) [(quarter_note,[Tie_Left]),(eighth_note,[Tie_Right])]
da_tuplet :: (Integer,Integer) -> [Duration_A] -> [Duration_A]
da_tuplet (d,n) x =
    let fn (p,q) = (p {multiplier = n%d},q)
        k = sum (map (duration_to_rq . fst) x) / (d%1)
        ty = rq_to_duration_err (show ("da_tuplet",d,n,x,k)) k
        t0 = [Begin_Tuplet (d,n,ty)]
        ts = [t0] ++ replicate (length x - 2) [] ++ [[End_Tuplet]]
        jn (p,q) z = (p,q++z)
    in zipWith jn (map fn x) ts

-- | Group tuplets into a 'Tree'.  Branch nodes have label 'Nothing',
-- leaf nodes label 'Just' 'Duration_A'.
--
-- > import Music.Theory.Duration.Name.Abbreviation
--
-- > let d = [(q,[])
-- >         ,(e,[Begin_Tuplet (3,2,e)])
-- >         ,(s,[Begin_Tuplet (3,2,s)]),(s,[]),(s,[End_Tuplet])
-- >         ,(e,[End_Tuplet])
-- >         ,(q,[])]
-- > in catMaybes (flatten (da_group_tuplets d)) == d
da_group_tuplets :: [Duration_A] -> Tree (Maybe Duration_A)
da_group_tuplets = L.group_tree (da_begins_tuplet,da_ends_tuplet)

-- | Variant of 'break' that places separator at left.
--
-- > break_left (== 3) [1..6] == ([1..3],[4..6])
-- > break_left (== 3) [1..3] == ([1..3],[])
break_left :: (a -> Bool) -> [a] -> ([a], [a])
break_left f x =
    let (p,q) = break f x
    in case q of
         [] -> (p,q)
         i:j -> (p++[i],j)

-- | Variant of 'break_left' that balances begin & end predicates.
--
-- > break_left (== ')') "test (sep) _) balanced"
-- > sep_balanced True (== '(') (== ')') "test (sep) _) balanced"
-- > sep_balanced False (== '(') (== ')') "(test (sep) _) balanced"
sep_balanced :: Bool -> (a -> Bool) -> (a -> Bool) -> [a] -> ([a], [a])
sep_balanced u f g =
    let go n x =
            case x of
              [] -> ([],[])
              p:q -> let n' = if f p then n + 1 else n
                         r = g p
                         n'' = if r then n' - 1 else n'
                     in if r && n'' == 0
                        then ([p],q)
                        else let (i,j) = go n'' q in (p:i,j)
    in go (fromEnum u)

-- | Group non-nested tuplets, ie. groups nested tuplets at one level.
da_group_tuplets_nn :: [Duration_A] -> [Either Duration_A [Duration_A]]
da_group_tuplets_nn x =
    case x of
      [] -> []
      d:x' -> if da_begins_tuplet d
              then let f = sep_balanced True da_begins_tuplet da_ends_tuplet
                       (t,x'') = f x'
                   in Right (d : t) : da_group_tuplets_nn x''
              else Left d : da_group_tuplets_nn x'

-- | Keep right variant of 'zip', unused rhs values are returned.
--
-- > zip_kr [1..4] ['a'..'f'] == ([(1,'a'),(2,'b'),(3,'c'),(4,'d')],"ef")
zip_kr :: [a] -> [b] -> ([(a,b)],[b])
zip_kr = L.zip_with_kr (,)

-- | 'zipWith' variant that adopts the shape of the lhs.
--
-- > let {p = [Left 1,Right [2,3],Left 4]
-- >     ;q = "abcd"}
-- > in nn_reshape (,) p q == [Left (1,'a'),Right [(2,'b'),(3,'c')],Left (4,'d')]
nn_reshape :: (a -> b -> c) -> [Either a [a]] -> [b] -> [Either c [c]]
nn_reshape f p q =
    case (p,q) of
      (e:p',i:q') -> case e of
                       Left j -> Left (f j i) : nn_reshape f p' q'
                       Right j -> let (j',q'') = L.zip_with_kr f j q
                                  in Right j' : nn_reshape f p' q''
      _ -> []

-- | Does /a/ have 'Tie_Left' and 'Tie_Right'?
d_annotated_tied_lr :: [D_Annotation] -> (Bool,Bool)
d_annotated_tied_lr a = (Tie_Left `elem` a,Tie_Right `elem` a)

-- | Does /d/ have 'Tie_Left' and 'Tie_Right'?
duration_a_tied_lr :: Duration_A -> (Bool,Bool)
duration_a_tied_lr (_,a) = d_annotated_tied_lr a
