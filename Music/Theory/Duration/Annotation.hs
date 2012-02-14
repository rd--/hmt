-- | Duration annotations.
module Music.Theory.Duration.Annotation where

import Data.Maybe
import Data.Ratio
import Data.Tree
import Music.Theory.Duration
import Music.Theory.Duration.RQ

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
    case catMaybes (map begin_tuplet a) of
      [t] -> Just t
      _ -> Nothing

begins_tuplet :: D_Annotation -> Bool
begins_tuplet a =
    case a of
      Begin_Tuplet _ -> True
      _ -> False

-- | Does 'Duration_A' being a tuplet?
da_begins_tuplet :: Duration_A -> Bool
da_begins_tuplet (_,a) = any begins_tuplet a

-- | Does 'Duration_A' end a tuplet?
da_ends_tuplet :: Duration_A -> Bool
da_ends_tuplet (_,a) = any (== End_Tuplet) a

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

-- | Transform predicates into 'Ordering' predicate such that if /f/
-- holds then 'LT', if /g/ holds then 'GT' else 'EQ'.
--
-- > map (begin_end_cmp (== '{') (== '}')) "{a}" == [LT,EQ,GT]
begin_end_cmp :: (t -> Bool) -> (t -> Bool) -> t -> Ordering
begin_end_cmp f g x = if f x then LT else if g x then GT else EQ

-- | Variant of 'begin_end_cmp', predicates are constructed by '=='.
--
-- > map (begin_end_cmp_eq '{' '}') "{a}" == [LT,EQ,GT]
begin_end_cmp_eq :: Eq t => t -> t -> t -> Ordering
begin_end_cmp_eq p q = begin_end_cmp (== p) (== q)

-- | Given an 'Ordering' predicate where 'LT' opens a group, 'GT'
-- closes a group, and 'EQ' continues current group, construct tree
-- from list.
--
-- > let {l = "a {b {c d} e f} g h i"
-- >     ;t = group_tree (begin_end_cmp_eq '{' '}') l}
-- > in catMaybes (concatMap flatten t) == l
--
-- > let d = putStrLn . drawForest . map (fmap show)
-- > in d (group_tree (begin_end_cmp_eq '(' ')') "a(b(cd)ef)ghi")
group_tree :: (a -> Ordering) -> [a] -> Forest (Maybe a)
group_tree f =
    let unit e = Node (Just e) []
        nil = Node Nothing []
        insert_e (Node t l) e = Node t (e:l)
        reverse_n (Node t l) = Node t (reverse l)
        push (r,z) e = case z of
                         h:z' -> (r,insert_e h (unit e) : z')
                         [] -> (unit e : r,[])
        open (r,z) = (r,nil:z)
        close (r,z) = case z of
                        h0:h1:z' -> (r,insert_e h1 (reverse_n h0) : z')
                        h:z' -> (reverse_n h : r,z')
                        [] -> (r,z)
        go st x =
            case x of
              [] -> reverse (fst st)
              e:x' -> case f e of
                        LT -> go (push (open st) e) x'
                        EQ -> go (push st e) x'
                        GT -> go (close (push st e)) x'
    in go ([],[])

-- | Group tuplets into 'Tree'.
--
-- > import Music.Theory.Duration.Name.Abbreviation
--
-- > let d = [(q,[])
-- >         ,(e,[Begin_Tuplet (3,2,e)])
-- >         ,(s,[Begin_Tuplet (3,2,s)]),(s,[]),(s,[End_Tuplet])
-- >         ,(e,[End_Tuplet])
-- >         ,(q,[])]
-- > in catMaybes (concatMap flatten (da_group_tuplets d)) == d
da_group_tuplets :: [Duration_A] -> Forest (Maybe Duration_A)
da_group_tuplets =
    let f = begin_end_cmp da_begins_tuplet da_ends_tuplet
    in group_tree f

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

-- | Keep right variant of 'zipWith', unused rhs values are returned.
--
-- > zip_with_kr (,) [1..3] ['a'..'e'] == ([(1,'a'),(2,'b'),(3,'c')],"de")
zip_with_kr :: (a -> b -> c) -> [a] -> [b] -> ([c],[b])
zip_with_kr f =
    let go r p q =
            case (p,q) of
              (i:p',j:q') -> go (f i j : r) p' q'
              _ -> (reverse r,q)
    in go []

-- | Keep right variant of 'zip', unused rhs values are returned.
--
-- > zip_kr [1..4] ['a'..'f'] == ([(1,'a'),(2,'b'),(3,'c'),(4,'d')],"ef")
zip_kr :: [a] -> [b] -> ([(a,b)],[b])
zip_kr p = zip_with_kr (,) p

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
                       Right j -> let (j',q'') = zip_with_kr f j q
                                  in Right j' : nn_reshape f p' q''
      _ -> []
