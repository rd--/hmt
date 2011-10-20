-- | Symetric Group S4 as related to the composition \"Nomos Alpha\"
-- by Iannis Xenakis.  In particular in relation to the discussion in
-- \"Towards a Philosophy of Music\", /Formalized Musi/ pp. 219 -- 221
module Music.Theory.Xenakis.S4 where

import Data.List
import qualified Data.Permute as P
import Music.Theory.Permutations

-- | 'Label's for elements of the symmetric group P4.
data Label = A|B|C|D|D2|E|E2|G|G2|I|L|Ln
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
      _ -> error "lower"

-- | Fig. VIII-6 (p. 221)
--
-- > length viii_6_l == 24
viii_6_l :: [Label]
viii_6_l =
    [Ln,L,A,Q1,Q7,Q3,Q9
    ,G2,G,C,Q8,Q5,Q10,Q2
    ,E,E2,B,Q4,Q11,Q12,Q6
    ,D,D2,I]

-- | Application of 'Label' /p/ on /q/.
--
-- > l_on Q1 I == Q1
-- > l_on D A == G
-- > [l_on L L,l_on E D,l_on D E] == [Ln,C,B]
l_on :: Label -> Label -> Label
l_on p q =
    let p' = seq_of p
        q' = seq_of q
        r = map (\i -> q' !! (i - 1)) p'
    in label_of r

-- | Fig. VIII-7 (p.221)
viii_7 :: [[Label]]
viii_7 =
    let o = [I,A,B,C
            ,D,D2,E,E2
            ,G,G2,L,Ln
            ,Q1,Q2,Q3,Q4
            ,Q5,Q6,Q7,Q8
            ,Q9,Q10,Q11,Q12]
    in map (\i -> map (\j -> l_on j i) o) o

-- > length viii_8_l == 24
viii_8_l :: [Label]
viii_8_l =
    [I,A,B,C,D2,D,E2,E
    ,G2,G,Ln,L,Q7,Q2,Q3,Q11
    ,Q8,Q6,Q1,Q5,Q9,Q10,Q4,Q12]

-- > length viii_8_p' == 24
-- > nub (map (length . nub) viii_8_p') == [4]
viii_8_p' :: [Half_Seq]
viii_8_p' =
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

viii_8' :: [(Label,Half_Seq)]
viii_8' = zip viii_8_l viii_8_p'

viii_8 :: [(Label,Seq)]
viii_8 = zip viii_8_l (map full_seq viii_8_p')

-- | 'Seq' of 'Label', inverse of 'label_of'.
--
-- > seq_of Q1 == [8,7,5,6,4,3,1,2]
seq_of :: Label -> Seq
seq_of i = maybe (error "seq_of") id (lookup i viii_8)

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

-- | Reverse table 'lookup'.
--
-- > reverse_lookup 'b' (zip [1..] ['a'..]) == Just 2
-- > lookup 2 (zip [1..] ['a'..]) == Just 'b'
reverse_lookup :: (Eq a) => a -> [(b,a)] -> Maybe b
reverse_lookup i =
    let f (p,q) = (q,p)
    in lookup i . map f

-- | 'Label' of 'Seq', inverse of 'seq_of'.
--
-- > label_of [8,7,5,6,4,3,1,2] == Q1
-- > label_of (seq_of Q4) == Q4
label_of :: Seq -> Label
label_of i =
    let err = error ("label_of: " ++ show i)
    in maybe err id (reverse_lookup i (viii_8))

-- | 'True' if two 'Half_Seq's are complementary, ie. form a 'Seq'.
--
-- > complementary [4,2,1,3] [8,6,5,7] == True
complementary :: Half_Seq -> Half_Seq -> Bool
complementary p q =
    let c = concat (sort [sort p,sort q])
    in c == [1..8]

-- | Relation between to 'Half_Seq' values as a
-- /(complementary,permutation)/ pair.
type Rel = (Bool,P.Permute)

-- | Determine 'Rel' of 'Half_Seq's.
--
-- > relate [1,4,2,3] [1,3,4,2] == (False,P.listPermute 4 [0,3,1,2])
-- > relate [1,4,2,3] [8,5,6,7] == (True,P.listPermute 4 [1,0,2,3])
relate :: Half_Seq -> Half_Seq -> Rel
relate p q =
    case complementary p q of
      True -> (True,permutation (complement p) q)
      False -> (False,permutation p q)

-- | Apply 'Rel' to 'Half_Seq'.
--
-- > apply_relation (False,P.listPermute 4 [0,3,1,2]) [1,4,2,3] == [1,3,4,2]
apply_relation :: Rel -> Half_Seq -> Half_Seq
apply_relation (c,p) i =
    let j = apply_permutation p i
    in if c then complement j else j

relations :: [Half_Seq] -> [Rel]
relations p = zipWith relate p (tail p)

-- > length (nub viii_6_relations) == 14
viii_6_relations :: [Rel]
viii_6_relations = relations (map (half_seq . seq_of) viii_6_l)

-- > length (nub viii_8_relations) == 10
viii_8_relations :: [Rel]
viii_8_relations = relations (map (half_seq . seq_of) viii_8_l)

-- | 'Rel' from 'Label' /p/ to /q/.
--
-- > relate_l L Ln == (False,listPermute 4 [0,3,1,2])
relate_l :: Label -> Label -> Rel
relate_l p q = relate (half_seq (seq_of p)) (half_seq (seq_of q))

-- | Enumeration of set of /faces/ of a cube.
data Face = F_Back | F_Front | F_Right | F_Left | F_Bottom | F_Top
          deriving (Eq,Enum,Bounded,Ord,Show)

-- | Table indicating set of faces of cubes as drawn in Fig. VIII-5
-- (p.220).
--
-- > lookup [1,4,6,7] faces == Just F_Left
-- > reverse_lookup F_Right faces == Just [2,3,5,8]
faces :: [([Int],Face)]
faces =
    [([1,3,6,8],F_Back) -- (I in viii-5)
    ,([2,4,5,7],F_Front)
    ,([2,3,5,8],F_Right)
    ,([1,4,6,7],F_Left)
    ,([3,4,5,6],F_Bottom)
    ,([1,2,7,8],F_Top)]
