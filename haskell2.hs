-- exs
--  - pares
--  - multiplos args
--  - Exp.Arg
--  - Exp.Func
--  - data Mem a

import Debug.Trace

type Mem = [(String,Int)]

consulta :: [(String,a)] -> String -> a
consulta []           id = undefined
consulta ((id',v'):l) id = if id == id' then
                            v'
                           else
                            consulta l id

escreve :: [(String,a)] -> String -> a -> [(String,a)]
escreve l id v = (id,v):l

-------------------------------------------------------------------------------

data Exp = Num Int
         | Add Exp Exp
         | Sub Exp Exp
         | Var String
         | App String Exp
  deriving Show

avaliaExp :: Env -> Exp -> (Int,Env)
avaliaExp env        (Num v)     = (v,env)
avaliaExp env       (Add e1 e2) = (fst (avaliaExp env e1) + fst (avaliaExp env e2),env)
avaliaExp env       (Sub e1 e2) = (fst (avaliaExp env e1) - fst (avaliaExp env e2), env)
avaliaExp (mem,cod)   (Var id)    = (consulta mem id,(mem,cod))
avaliaExp (mem,cod) (App id e)  = (ret,(mem'',cod)) where
                                    ret = consulta mem'' "ret" --  retorno da função
                                    (mem'',_) = avaliaCmd (mem',cod) fun -- EXECUTA A FUNÇÃO
                                    mem' = escreve mem "arg" arg  -- escreve na memória o valor de parâmetro da função
                                    arg  = fst (avaliaExp (mem,cod) e) -- retorna o valor da exp da App
                                    fun  = consulta cod id   -- retorna a função que vai ser executada

-------------------------------------------------------------------------------

type Cod = [(String,Cmd)]
type Env = (Mem,Cod)

data Cmd = Atr String Exp
         | Prt Exp
         | Seq Cmd Cmd
         | Cnd Exp Cmd Cmd
         | Fun String Cmd
  deriving Show

avaliaCmd :: Env -> Cmd -> Env
avaliaCmd env       (Prt e)         = traceShow (fst (avaliaExp env e)) snd (avaliaExp env e)
avaliaCmd (mem,cod) (Atr id exp)    = (escreve mem id v,cod) where
                                        v = fst (avaliaExp (mem,cod) exp)
avaliaCmd env       (Seq c1 c2)     = avaliaCmd env' c2 where
                                        env' = avaliaCmd env c1
avaliaCmd env       (Cnd exp c1 c2) = if (fst( avaliaExp env exp)) /= 0 then
                                        avaliaCmd env c1
                                      else
                                        avaliaCmd env c2
avaliaCmd (mem,cod) (Fun id c)      = (mem, escreve cod id c)

-------------------------------------------------------------------------------
p3 = Seq 
		(Seq 
			(Atr "y" (Num 10)) 
			(Seq  
			   (Fun "F" 
					(Seq 
						(Seq
						    (Atr "y" (Var "arg"))
							(Prt (Var "y")))
						(Atr "ret" (Var "y"))))
			   (Prt (App "F" (Num 20))))
	     )
		 (Prt (Var "y"))
		
			

main = print (avaliaCmd ([],[]) p3)