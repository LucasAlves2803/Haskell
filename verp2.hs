-- Revisando declarações de uma linguagem

-- verificaExp é uma função que verifica se as variáveis já foram declaradas, o que significa 
-- que estão escritas na memória

-- Rep (Menor (Var "x") (Var "y")) 
--            (Seq 
--                  (Atr "ret" (Add (Var "x") (Add (Var "x") (Num 1))))
--                  (Atr "x" (Add (Var "x") (Num 1)))
--            )

import Debug.Trace
data Talvez a = Apenas Int
                 | Nada
           deriving Show				 
data Cmd = Atr String Exp   -- atribuicao, ex.: x=1
         | Seq Cmd Cmd      -- sequencia,  ex.: x=1 ; y=x
         | Dcl String       -- declaracao, ex.: int x
		 | Rep Exp Cmd
        deriving Show
data Exp = Num Int
         | Add Exp Exp
         | Sub Exp Exp
         | Var String
		 deriving Show
		 
		 
type Mem  = [(String, Int)]
-- avalia uma expressão com variáveis
avaliaexp :: Mem -> Exp -> Int
avaliaexp _ (Num x) = x
avaliaexp amb (Add exp1 exp2) = (avaliaexp amb exp1) + (avaliaexp amb exp2)
avaliaexp amb (Sub exp1 exp2) = (avaliaexp amb exp1) - (avaliaexp amb exp2)
avaliaexp amb (Var c) = consulta amb c

-- avalia um comando
avaliacmd :: Mem -> Cmd -> Mem
avaliacmd amb (Atr cad exp1) = escreve amb cad v 
			        where
				   v = avaliaexp amb exp1

avaliacmd amb (Seq cm1 cm2) = avaliacmd amb' cm2 
			      where 
				  amb' = avaliacmd amb cm1
-- avaliacmd amb (Dcl _) = amb
avaliacmd amb (Rep exp cmd) = if (avaliaexp amb exp) then
                              avaliacmd (avaliacmd amb cmd) (Rep exp cmd)
							  else
							     amb




-- Consulta o elemento na memória
consulta :: Mem -> String -> Int
consulta ((id',v'):l) id = if id' == id then
				v'
			   else
        			consulta l id
consulta [] id = 0

escreve :: Mem -> String -> Int -> Mem
escreve mem str v = (str,v):mem
		 
		 
cmd1 = Seq (Dcl "x") (Atr "x" (Num 1))	
cmd2 = Seq (Dcl "x")
           (Seq (Dcl "y")
                 (Seq ( Atr "x" (Num 2))
				      ( Seq  (Dcl "ret") 
					         (Seq (Atr "y" (Num 1))
                                  ((Atr "ret" (Add (Var "x") (Var "y"))))
							 )
                      )
                 )	
           )				 
cria :: [String] -> String -> [String] 
cria amb id = (id):amb

verificaExp :: [String] -> Exp -> Bool
verificaExp mem (Var str) = elem str mem
verificaExp mem (Num _) = True
verificaExp mem (Add exp1 exp2) = (verificaExp mem exp1) && (verificaExp mem exp2)
verificaExp mem (Sub exp1 exp2) = (verificaExp mem exp1) && (verificaExp mem exp2)

-- verificaCmd é uma função que declara as variáveis pelo comando Dcl e também verifica se todas as variáveis 
-- foram declaradas

verificaCmd :: [String] -> Cmd -> ([String], Bool)
verificaCmd amb (Dcl str) = (cria amb str, True)
verificaCmd amb (Atr str exp) = (amb , (elem str amb) && (verificaExp amb exp))
verificaCmd amb (Seq cmd1 cmd2) = (amb', v1 && v2) where  
								(mem, v1 ) = verificaCmd amb cmd1
								(amb', v2) = verificaCmd mem cmd2
								

verificaProg :: Cmd -> Bool
verificaProg cmd1 = valor where
                    (_, valor ) = verificaCmd [] cmd1

avaliaProg :: Cmd -> Talvez Int
avaliaProg cmd = if (verificaProg cmd) then
					Apenas (consulta (avaliacmd [] cmd) "ret")
				else
					Nada
					

avaliaProg2 :: Cmd -> Talvez Int
avaliaProg2 cmd = if (verificaProg cmd) then
					Apenas (consulta (avaliacmd [] (eliminaDcl cmd)) "ret")
				else
					Nada				

eliminaDcl :: Cmd -> Cmd
eliminaDcl (Seq (Dcl _) (cmd)) = (eliminaDcl cmd)
eliminaDcl (Seq (cmd) (Dcl _)) = (eliminaDcl cmd)
eliminaDcl (Seq (cmd1) (cmd2)) = Seq (eliminaDcl cmd1) (eliminaDcl cmd2)
eliminaDcl (Atr x y) = Atr x y



	
main = print (avaliaProg2 cmd2)


