data Exp = Num Int
        | Add Exp Exp
        | Sub Exp Exp
        deriving Show
 
 
-- Questão 1
e0 = Num 1
e1 = Add (Num 1) (Sub (Num 10) (Num 20))
e2 = Add (Sub (Num 10) (Num 20)) (Num 1)
e3 = Sub (Add (Num 5) (Num 5)) (Sub (Num 5) (Num 5))
-- Questão 2
{- e0 = 1 + 2
  e1 = ((1 + 2) + 3) + 4
  e2 = 1 + ( 2 + (3 + 4))
  e3 = ((1-2)-3) - 4
  e4 = 1 - (2 - (3 - 4))
  e5 = ((1-2) - (3 - 4))
-}
 
 
-- Questão 3
avalia :: Exp -> Int
avalia (Num x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2)
-- Questão 4 
avalia2 ::(Exp -> Int) -> Exp ->  Exp
avalia2 _ (Num x) = (Num x)
avalia2 f exp1 = (avalia2 f ( Num (f exp1)))

e4 = Add (Add (Add (Num 1) (Num 2)) (Num 3)) (Num 4)
main = print (avalia2 avalia e3)
