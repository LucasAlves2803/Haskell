data Exp = Log Bool
           | And Exp Exp
		   | Or  Exp Exp
		   | Not Exp
		  deriving Show
		   
c1 = Not (Log True)		   
		   
avaliaExp :: Exp -> Bool
avaliaExp (Log x) = x
avaliaExp (And exp1 exp2) = (avaliaExp exp1) && (avaliaExp exp2)
avaliaExp (Or exp1 exp2)  = (avaliaExp exp1) || (avaliaExp exp2)
avaliaExp (Not x) = not (avaliaExp x)

avalia' :: Exp -> Exp
avalia' exp = Log (avaliaExp exp)
main = print (avalia' c1)