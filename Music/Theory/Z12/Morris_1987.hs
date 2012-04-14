-- | Robert Morris. /Composition with Pitch-Classes: A Theory of
-- Compositional Design/. Yale University Press, New Haven, 1987.
module Music.Theory.Z12.Morris_1987 where

import Data.List
import Music.Theory.List
import Music.Theory.Z12
import Music.Theory.Z12.SRO

-- | @INT@ operator.
--
-- > int [0,1,3,6,10] == [1,2,3,4]
int :: [Z12] -> [Z12]
int = d_dx

-- * Serial operations

-- | Serial Operator,of the form rRTMI.
data SRO = SRO Z12 Bool Z12 Bool Bool
           deriving (Eq,Show)

-- | Serial operation.
--
-- >>> sro T4 156
-- 59A
--
-- > sro (rnrtnmi "T4") (pco "156") == [5,9,10]
--
-- >>> echo 024579 | sro RT4I
-- 79B024
--
-- > sro (SRO 0 True 4 False True) [0,2,4,5,7,9] == [7,9,11,0,2,4]
--
-- >>> sro T4I 156
-- 3BA
--
-- > sro (rnrtnmi "T4I") (pco "156") == [3,11,10]
-- > sro (SRO 0 False 4 False True) [1,5,6] == [3,11,10]
--
-- >>> echo 156 | sro T4  | sro T0I
-- 732
--
-- > (sro (rnrtnmi "T0I") . sro (rnrtnmi "T4")) (pco "156") == [7,3,2]
--
-- >>> echo 024579 | sro RT4I
-- 79B024
--
-- > sro (rnrtnmi "RT4I") (pco "024579") == [7,9,11,0,2,4]
--
-- > sro (SRO 1 True 1 True False) [0,1,2,3] == [11,6,1,4]
-- > sro (SRO 1 False 4 True True) [0,1,2,3] == [11,6,1,4]
sro :: SRO -> [Z12] -> [Z12]
sro (SRO r r' t m i) x =
    let x1 = if i then invert 0 x else x
        x2 = if m then m5 x1 else x1
        x3 = tn t x2
        x4 = if r' then reverse x3 else x3
    in genericRotate_left r x4

-- | The total set of serial operations.
sros :: [Z12] -> [(SRO,[Z12])]
sros x = [let o = (SRO r r' t m i) in (o,sro o x) |
          r <- [0 .. genericLength x - 1],
          r' <- [False,True],
          t <- [0 .. 11],
          m <- [False,True],
          i <- [False,True]]

-- | The set of transposition 'SRO's.
sro_Tn ::[SRO]
sro_Tn = [SRO 0 False n False False | n <- [0..11]]

-- | The set of transposition and inversion 'SRO's.
sro_TnI ::[SRO]
sro_TnI = [SRO 0 False n False i |
           n <- [0..11],
           i <- [False,True]]

-- | The set of retrograde and transposition and inversion 'SRO's.
sro_RTnI ::[SRO]
sro_RTnI = [SRO 0 r n False i |
            r <- [True,False],
            n <- [0..11],
            i <- [False,True]]

-- | The set of transposition,@M5@ and inversion 'SRO's.
sro_TnMI ::[SRO]
sro_TnMI = [SRO 0 False n m i |
            n <- [0..11],
            m <- [True,False],
            i <- [True,False]]

-- | The set of retrograde,transposition,@M5@ and inversion 'SRO's.
sro_RTnMI ::[SRO]
sro_RTnMI = [SRO 0 r n m i |
             r <- [True,False],
             n <- [0..11],
             m <- [True,False],
             i <- [True,False]]
