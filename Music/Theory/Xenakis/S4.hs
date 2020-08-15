-- | Symetric Group S4 as related to the composition \"Nomos Alpha\"
-- by Iannis Xenakis.  In particular in relation to the discussion in
-- \"Towards a Philosophy of Music\", /Formalized Music/ pp. 219 -- 221
module Music.Theory.Xenakis.S4 where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.List as T
import qualified Music.Theory.Permutations as T

-- * S4 notation

-- | 'Label's for elements of the symmetric group P4.
data Label = A|B|C|D|D2|E|E2|G|G2|I|L|L2
           | Q1|Q2|Q3|Q4|Q5|Q6|Q7|Q8|Q9|Q10|Q11|Q12
             deriving (Eq,Ord,Enum,Bounded,Show)

-- | Initial half of 'Seq' (ie. #4).  The complete 'Seq' is formed by
-- appending the 'complement' of the 'Half_Seq'.
type Half_Seq = [Int]

-- | Complete sequence (ie. #8).
type Seq = [Int]

-- | Complement of a 'Half_Seq'.
--
-- > map complement [[4,1,3,2],[6,7,8,5]] == [[8,5,7,6],[2,3,4,1]]
complement :: Half_Seq -> Half_Seq
complement x =
    case sort x of
      [1,2,3,4] -> map (+ 4) x
      [5,6,7,8] -> map (+ (-4)) x
      _ -> error "complement"

-- | Form 'Seq' from 'Half_Seq'.
--
-- > full_seq [3,2,4,1] == [3,2,4,1,7,6,8,5]
-- > label_of (full_seq [3,2,4,1]) == G2
-- > label_of (full_seq [1,4,2,3]) == L
full_seq :: Half_Seq -> Seq
full_seq x = x ++ complement x

-- | Lower 'Half_Seq', ie. 'complement' or 'id'.
--
-- > map lower [[4,1,3,2],[6,7,8,5]] == [[4,1,3,2],[2,3,4,1]]
lower :: Half_Seq -> Half_Seq
lower x =
    case sort x of
      [1,2,3,4] -> x
      [5,6,7,8] -> complement x
      _ -> error (show ("lower",x))

-- | Application of 'Label' /p/ on /q/.
--
-- > l_on Q1 I == Q1
-- > l_on D Q12 == Q4
-- > [l_on L L,l_on E D,l_on D E] == [L2,C,B]
l_on :: Label -> Label -> Label
l_on p q =
    let p' = seq_of p
        q' = seq_of q
        r = map (\i -> q' !! (i - 1)) p'
    in label_of r

{- | Generalisation of Fibonnaci process, /f/ is the binary operator
giving the next element, /p/ and /q/ are the initial elements.

See discussion in: Carlos Agon, Moreno Andreatta, Gérard Assayag, and
Stéphan Schaub. _Formal Aspects of Iannis Xenakis' "Symbolic Music": A
Computer-Aided Exploration of Compositional Processes_. Journal of New
Music Research, 33(2):145-159, 2004.

Note that the article has an error, printing Q4 for Q11 in the sequence below.

> import qualified Music.Theory.List as T

> let r = [D,Q12,Q4, E,Q8,Q2, E2,Q7,Q4, D2,Q3,Q11, L2,Q7,Q2, L,Q8,Q11]
> (take 18 (fib_proc l_on D Q12) == r,T.duplicates r == [Q2,Q4,Q7,Q8,Q11])

Beginning E then G2 no Q nodes are visited.

> let r = [E,G2,L2,C,G,D,E,B,D2,L,G,C,L2,E2,D2,B]
> (take 16 (fib_proc l_on E G2) == r,T.duplicates r == [B,C,D2,E,G,L2])

> let [a,b] = take 2 (T.segments 18 18 (fib_proc l_on D Q12)) in a == b

The prime numbers that are not factors of 18 are {1,5,7,11,13,17}.
They form a closed group under modulo 18 multiplication.

> let n = [5,7,11,13,17]
> let r0 = [(5,7,17),(5,11,1),(5,13,11),(5,17,13)]
> let r1 = [(7,11,5),(7,13,1),(7,17,11)]
> let r2 = [(11,13,17),(11,17,7)]
> let r3 = [(13,17,5)]
> [(p,q,(p * q) `mod` 18) | p <- n, q <- n, p < q] == concat [r0,r1,r2,r3]

The article also omits the 5 after 5,1 in the sequence below.

> let r = [11,13,17,5,13,11,17,7,11,5,1,5,5,7,17,11,7,5,17,13,5,11,1,11]
> take 24 (fib_proc (\p q -> (p * q) `mod` 18) 11 13) == r

-}
fib_proc :: (a -> a -> a) -> a -> a -> [a]
fib_proc f p q = let r = f p q in p : fib_proc f q r

-- | 'Seq' of 'Label', inverse of 'label_of'.
--
-- > seq_of Q1 == [8,7,5,6,4,3,1,2]
seq_of :: Label -> Seq
seq_of i = fromMaybe (error "seq_of") (lookup i viii_6b)

-- | 'Half_Seq' of 'Label', ie. 'half_seq' '.' 'seq_of'.
--
-- > half_seq_of Q1 == [8,7,5,6]
half_seq_of :: Label -> Seq
half_seq_of = half_seq . seq_of

-- | 'Half_Seq' of 'Seq', ie. 'take' @4@.
--
-- > complement (half_seq (seq_of Q7)) == [3,4,2,1]
half_seq :: Seq -> Half_Seq
half_seq = take 4

-- | 'Label' of 'Seq', inverse of 'seq_of'.
--
-- > label_of [8,7,5,6,4,3,1,2] == Q1
-- > label_of (seq_of Q4) == Q4
label_of :: Seq -> Label
label_of i =
    let err = error ("label_of: " ++ show i)
    in fromMaybe err (T.reverse_lookup i viii_6b)

-- | 'True' if two 'Half_Seq's are complementary, ie. form a 'Seq'.
--
-- > complementary [4,2,1,3] [8,6,5,7] == True
complementary :: Half_Seq -> Half_Seq -> Bool
complementary p q =
    let c = concat (sort [sort p,sort q])
    in c == [1..8]

-- * Rel

-- | Relation between to 'Half_Seq' values as a
-- /(complementary,permutation)/ pair.
type Rel = (Bool,T.Permutation)

-- | Determine 'Rel' of 'Half_Seq's.
--
-- > relate [1,4,2,3] [1,3,4,2] == (False,[0,3,1,2])
-- > relate [1,4,2,3] [8,5,6,7] == (True,[1,0,2,3])
relate :: Half_Seq -> Half_Seq -> Rel
relate p q =
    if complementary p q
    then (True,T.permutation (complement p) q)
    else (False,T.permutation p q)

-- | 'Rel' from 'Label' /p/ to /q/.
--
-- > relate_l L L2 == (False,[0,3,1,2])
relate_l :: Label -> Label -> Rel
relate_l p q = relate (half_seq_of p) (half_seq_of q)

-- | 'relate' adjacent 'Half_Seq', see also 'relations_l'.
relations :: [Half_Seq] -> [Rel]
relations p = zipWith relate p (tail p)

-- | 'relate' adjacent 'Label's.
--
-- > relations_l [L2,L,A] == [(False,[0,2,3,1]),(False,[2,0,1,3])]
relations_l :: [Label] -> [Rel]
relations_l p = zipWith relate_l p (tail p)

-- | Apply 'Rel' to 'Half_Seq'.
--
-- > apply_relation (False,[0,3,1,2]) [1,4,2,3] == [1,3,4,2]
apply_relation :: Rel -> Half_Seq -> Half_Seq
apply_relation (c,p) i =
    let j = T.apply_permutation p i
    in if c then complement j else j

-- | Apply sequence of 'Rel' to initial 'Half_Seq'.
apply_relations :: [Rel] -> Half_Seq -> [Half_Seq]
apply_relations rs i =
    case rs of
      [] -> [i]
      (r:rs') -> let i' = apply_relation r i
                 in i : apply_relations rs' i'

-- | Variant of 'apply_relations'.
--
-- > apply_relations_l (relations_l [L2,L,A,Q1]) L2 == [L2,L,A,Q1]
apply_relations_l :: [Rel] -> Label -> [Label]
apply_relations_l rs = map (label_of . full_seq) .
                       apply_relations rs .
                       half_seq_of

-- * Face

-- | Enumeration of set of /faces/ of a cube.
data Face = F_Back | F_Front | F_Right | F_Left | F_Bottom | F_Top
          deriving (Eq,Enum,Bounded,Ord,Show)

-- | Table indicating set of faces of cubes as drawn in Fig. VIII-6 (p.220).
--
-- > lookup [1,4,6,7] faces == Just F_Left
-- > T.reverse_lookup F_Right faces == Just [2,3,5,8]
faces :: [([Int],Face)]
faces =
    [([1,3,6,8],F_Back) -- (I in viii-6)
    ,([2,4,5,7],F_Front)
    ,([2,3,5,8],F_Right)
    ,([1,4,6,7],F_Left)
    ,([3,4,5,6],F_Bottom)
    ,([1,2,7,8],F_Top)]

-- * Figures

-- | Label sequence of Fig. VIII-6. Hexahedral (Octahedral) Group (p. 220)
--
-- > let r = [I,A,B,C,D,D2,E,E2,G,G2,L,L2,Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12]
-- > in viii_6_lseq == r
viii_6_lseq :: [Label]
viii_6_lseq =
    [L2,L,A,Q1,Q7,Q3,Q9
    ,G2,G,C,Q8,Q5,Q10,Q2
    ,E,E2,B,Q4,Q11,Q12,Q6
    ,D,D2,I]

-- | Label sequence of Fig. VIII-7 (p.221)
--
-- > let r = [I,A,B,C,D,D2,E,E2,G,G2,L,L2,Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12]
-- > in viii_7_lseq == r
viii_7_lseq :: [Label]
viii_7_lseq =
    [I,A,B,C
    ,D,D2,E,E2
    ,G,G2,L,L2
    ,Q1,Q2,Q3,Q4
    ,Q5,Q6,Q7,Q8
    ,Q9,Q10,Q11,Q12]

-- | Fig. VIII-7 (p.221)
--
-- > map (take 4) (take 4 viii_7) == [[I,A,B,C]
-- >                                 ,[A,I,C,B]
-- >                                 ,[B,C,I,A]
-- >                                 ,[C,B,A,I]]
--
-- > import Music.Theory.Array.MD
--
-- > let t = md_matrix_opt show (\x -> "_" ++ x ++ "_") (head viii_7,head viii_7) viii_7
-- > putStrLn $ unlines $ md_table' t
viii_7 :: [[Label]]
viii_7 = map (\i -> map (`l_on` i) viii_7_lseq) viii_7_lseq

-- | Label sequence of Fig. VIII-6/b (p.221)
--
-- > length viii_6b_l == length viii_6_l
-- > take 8 viii_6b_l == [I,A,B,C,D2,D,E2,E]
viii_6b_lseq :: [Label]
viii_6b_lseq =
    [I,A,B,C
    ,D2,D,E2,E
    ,G2,G,L2,L
    ,Q7,Q2,Q3,Q11
    ,Q8,Q6,Q1,Q5
    ,Q9,Q10,Q4,Q12]

-- | Fig. VIII-6/b 'Half_Seq'.
--
-- > viii_6b_p' == map half_seq_of viii_6b_l
-- > nub (map (length . nub) viii_6b_p') == [4]
viii_6b_p' :: [Half_Seq]
viii_6b_p' =
    [[1,2,3,4]
    ,[2,1,4,3]
    ,[3,4,1,2]
    ,[4,3,2,1]
    ,[2,3,1,4]
    ,[3,1,2,4]
    ,[2,4,3,1]
    ,[4,1,3,2]

    ,[3,2,4,1]
    ,[4,2,1,3]
    ,[1,3,4,2]
    ,[1,4,2,3]
    ,[7,8,6,5]
    ,[7,6,5,8]
    ,[8,6,7,5]
    ,[6,7,8,5]

    ,[6,8,5,7]
    ,[6,5,7,8]
    ,[8,7,5,6]
    ,[7,5,8,6]
    ,[5,8,7,6]
    ,[5,7,6,8]
    ,[8,5,6,7]
    ,[5,6,8,7]]

-- | Variant of 'viii_6b' with 'Half_Seq'.
viii_6b' :: [(Label,Half_Seq)]
viii_6b' = zip viii_6b_lseq viii_6b_p'

-- | Fig. VIII-6/b.
--
-- > map (viii_6b !!) [0,8,16] == [(I,[1,2,3,4,5,6,7,8])
-- >                              ,(G2,[3,2,4,1,7,6,8,5])
-- >                              ,(Q8,[6,8,5,7,2,4,1,3])]
viii_6b :: [(Label,Seq)]
viii_6b = zip viii_6b_lseq (map full_seq viii_6b_p')

-- | The sequence of 'Rel' to give 'viii_6_l' from 'L2'.
--
-- > apply_relations_l viii_6_relations L2 == viii_6_l
-- > length (nub viii_6_relations) == 14
viii_6_relations :: [Rel]
viii_6_relations = relations (map half_seq_of viii_6_lseq)

-- | The sequence of 'Rel' to give 'viii_6b_l' from 'I'.
--
-- > apply_relations_l viii_6b_relations I == viii_6b_l
-- > length (nub viii_6b_relations) == 10
viii_6b_relations :: [Rel]
viii_6b_relations = relations (map half_seq_of viii_6b_lseq)

