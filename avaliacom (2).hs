data Exp = Num Int
        | Add Exp Exp
        | Sub Exp Exp
		| Var String
        deriving Show
data Cmd = Atr String Exp
           | Seq Cmd Cmd
		   | Cnd Exp Cmd Cmd
		   | Rep Exp Cmd
		deriving Show
		
type Mem = [(String,Int)]

avaliaexp :: Mem -> Exp -> Int
avaliaexp _ (Num x) = x
avaliaexp amb (Add exp1 exp2) = (avaliaexp amb exp1) + (avaliaexp amb exp2)
avaliaexp amb (Sub exp1 exp2) = (avaliaexp amb exp1) - (avaliaexp amb exp2)
avaliaexp amb (Var c) = consulta amb c


consulta :: Mem -> String -> Int
consulta ((id',v'):l) id = if id' == id then
								v'
						  else
							consulta l id
consulta [] id = 0

c1 = Cnd (Add (Num 2) (Num 0)) (Atr "x" (Num 3)) (Atr "x" (Num 0))
escreve :: Mem -> String -> Int -> Mem
escreve amb id c = (id , c):amb

avaliacmd :: Mem -> Cmd -> Mem
avaliacmd amb (Atr cad exp1) = escreve amb cad v 
						where 
						   v = avaliaexp amb exp1

avaliacmd amb (Seq cm1 cm2) = avaliacmd amb' cm1 
								where 
									amb' = avaliacmd amb cm2

avaliacmd amb (Cnd exp cm1 cm2) =  if (avaliaexp amb exp) /= 0 then
                                       avaliacmd amb cm1
								   else  
								     avaliacmd amb cm2

{-- avaliacmd amb (Rep exp cmd) = if (avaliaexp amb exp) /= 0 then
								 avaliacmd amb (Rep exp cmd)
										where 
											mem' = avaliacmd amb cmd										
							  else
									amb
	--}

avaliaProg :: Cmd -> Int
avaliaProg cmd = avaliacmd [] cmd

principal = print avaliaProg c1	