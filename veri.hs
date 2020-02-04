import Debug.Trace
data Cmd = Atr String Exp
	   | Seq Cmd Cmd
	   | Dcl String -- declarar é criar a variável na memória
	   | Null
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

avaliacmd amb (Seq cm1 cm2) = avaliacmd amb' cm1 
			      where 
				  amb' = avaliacmd amb cm2
avaliacmd amb Null = amb




-- Consulta o elemento na memória
consulta :: Mem -> String -> Int
consulta ((id',v'):l) id = if id' == id then
				v'
			   else
        			consulta l id
consulta [] id = 0


{-- elem na memória
elem :: [String] -> String -> Bool
elem ((id'):l) id = if id' == id then
		       True
		    else
		       elem l id
elem [] id = False
--}
-- cria na memória
cria :: [String] -> String -> [String] 
cria amb id = (id):amb

-- cria um ambiente
c1 = Atr "x" (Num 1)
c3 = Seq (Dcl "x") c1



escreve :: Mem -> String -> Int -> Mem
escreve mem str v = (str,v):mem

-- verifica uma expressão
verificaExp :: [String] -> Exp -> Bool
verificaExp _ (Num x) = True
verificaExp amb (Add exp1 exp2) = (verificaExp amb exp1) && (verificaExp amb exp2)
verificaExp amb (Sub exp1 exp2) = (verificaExp amb exp1) && (verificaExp amb exp2)
verificaExp amb (Var x) = elem x amb


-- verifica um comando
verificaCmd :: [String] -> Cmd -> ([String],Bool)
verificaCmd amb (Dcl str) = (cria amb str, True)
verificaCmd amb (Atr str exp) = (amb , (elem str amb) && (verificaExp amb exp))
verificaCmd amb (Seq cmd1 cmd2) = (amb', v1 && v2) where  
								(mem, v1 ) = verificaCmd amb cmd1
								(amb', v2) = verificaCmd mem cmd2
								
verificaProg :: Cmd -> Bool
verificaProg cmd = snd (verificaCmd [] cmd)

data Talvez a = Nada | Apenas a
      deriving Show

exp1 = Add (Var "x") (Add (Var "y") (Var "z")) 
cmd = Seq (Dcl "x") (Seq (Dcl "y") (Seq (Dcl "z") (Seq (Atr "x" (Num 1)) (Seq (Atr "y" (Num 2)) (Seq (Atr "z" (Num 3)) (Dcl "ret"))))))


eliminaDcl :: Cmd -> Cmd
eliminaDcl (Seq cmd1 cmd2) = Seq (eliminaDcl cmd1) (eliminaDcl cmd2)
eliminaDcl (Atr str exp) = (Atr str exp)
eliminaDcl (Dcl _) = Null


avaliaProg :: Cmd -> Talvez Int
avaliaProg cmd = if (verificaProg cmd) then
			Apenas (avaliaexp ( avaliacmd [] (eliminaDcl cmd)) (exp1) )
		  else
			Nada
					

principal = print (verificaCmd [] c3)
main = print(verificaProg c3)					
	  

		