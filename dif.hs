data Exp = Num Int
           | Add Exp Exp
		   | Sub Exp Exp
           | Maior Exp Exp
		   | Menor Exp Exp
		   | Igual Exp Exp
		   | Var String
		  deriving Show
data Cmd = Atr String Exp   -- atribuicao, ex.: x=1
         | Seq Cmd Cmd      -- sequencia,  ex.: x=1 ; y=x
         | Dcl String       -- declaracao, ex.: int x
		 | Rep Exp Cmd
        deriving Show
		
c4 = Rep (Menor (Var "x") (Var "y")) 
            (Seq 
                  (Atr "ret" (Add (Var "x") (Add (Var "x") (Num 1))))
                  (Atr "x" (Add (Var "x") (Num 2)))
            )

-- Consulta o elemento na memória
consulta :: Mem -> String -> Int
consulta ((id',v'):l) id = if id' == id then
				v'
			   else
        			consulta l id
consulta [] id = 0

escreve :: Mem -> String -> Int -> Mem
escreve mem str v = (str,v):mem
		 		
		   
c1 = Maior (Add (Num 1) (Num 2)) (Num 1)
type Mem  = [(String, Int)]
-- avalia uma expressão com variáveis
avaliaexp :: Mem -> Exp -> Int
avaliaexp _ (Num x) = x
avaliaexp amb (Add exp1 exp2) = (avaliaexp amb exp1) + (avaliaexp amb exp2)
avaliaexp amb (Sub exp1 exp2) = (avaliaexp amb exp1) - (avaliaexp amb exp2)
avaliaexp amb (Var c) = consulta amb c

avaliacmd :: Mem -> Cmd -> Mem
avaliacmd amb (Atr cad exp1) = escreve amb cad v 
			        where
				   v = avaliaexp amb exp1

avaliacmd amb (Seq cm1 cm2) = avaliacmd amb' cm2 
			      where 
				  amb' = avaliacmd amb cm1
avaliacmd amb (Dcl _) = amb
avaliacmd amb (Rep exp cmd) = if (valor amb exp) then
                              avaliacmd (avaliacmd amb cmd) (Rep exp cmd)
							  else
							     amb

		   
valor :: Mem -> Exp -> Bool
valor mem (Maior exp1 exp2) = if (avaliaexp mem exp1) > (avaliaexp mem exp2) then
                              True
					      else 
                              False						  
valor mem (Menor exp1 exp2) = if (avaliaexp mem exp1) < (avaliaexp mem exp2) then
                              True
					      else 
                              False
valor mem (Igual exp1 exp2) = if (avaliaexp mem exp1) == (avaliaexp mem exp2) then
                              True
					      else 
                              False						  							  
amb = [("x",1),("y",10)]							  
main = print (consulta (avaliacmd amb c4) "ret" )