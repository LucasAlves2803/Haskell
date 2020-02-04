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

avaliaExp :: Env -> Exp -> Int
avaliaExp _         (Num v)     = v
avaliaExp env       (Add e1 e2) = (avaliaExp env e1) + (avaliaExp env e2)
avaliaExp env       (Sub e1 e2) = (avaliaExp env e1) - (avaliaExp env e2)
avaliaExp (mem,_)   (Var id)    = consulta mem id
avaliaExp (mem,cod) (App id e)  = ret where
                                    ret = consulta mem'' "ret"
                                    (mem'',_) = avaliaCmd (mem',cod) fun -- executa a função
                                    mem' = escreve mem (fst (head mem)) arg -- escreve o valor na memoria com o nome arg
                                    arg  = avaliaExp (mem,cod) e -- arg recebe o valor da espressão passado por App
                                    fun  = consulta cod id -- fun recebe o código da função

{-- 
	p0 = Seq 
	   (Fun "F" "x" (Atr "ret" (Var "x")))
	   (Prt ( App "F" (Num 4)))
	------------------------------------
	p0 = Seq s
	   (Fun "F" (Atr "ret" (Var "arg")))
	   (Prt ( App "F" (Num 1)))

--}


-------------------------------------------------------------------------------

type Cod = [(String,Cmd)]
type Env = (Mem,Cod)

data Cmd = Atr String Exp
         | Prt Exp
         | Seq Cmd Cmd
         | Cnd Exp Cmd Cmd
    --   | Fun String Cmd
		 | Fun String String Cmd
  deriving Show

avaliaCmd :: Env -> Cmd -> Env
avaliaCmd env       (Prt e)         = traceShow (avaliaExp env e) env
avaliaCmd (mem,cod) (Atr id exp)    = (escreve mem id v,cod) where
                                        v = avaliaExp (mem,cod) exp
avaliaCmd env       (Seq c1 c2)     = avaliaCmd env' c2 where
                                        env' = avaliaCmd env c1
avaliaCmd env       (Cnd exp c1 c2) = if (avaliaExp env exp) /= 0 then
                                        avaliaCmd env c1
                                      else
                                        avaliaCmd env c2
avaliaCmd (mem,cod) (Fun id nome c)      = ( escreve mem nome 0 , escreve cod id c)

-------------------------------------------------------------------------------
p0 = Seq 
	   (Fun "F" "x" (Atr "ret" (Var "x")))
	   (Prt ( App "F" (Num 3)))

p1 = Seq
      (Fun "duplica" "x"
        (Atr "ret"
          (Add (Var "x")
               (Var "x"))))
      (Prt (App "duplica" (Num 10)))

p2 = Seq
      (Fun "soma" "x"
        (Cnd (Var "x")
          (Atr "ret"
            (Add (Var "x")
                 (App "soma"
                   (Sub (Var "x")
                        (Num 1)))))
          (Atr "ret" (Num 0))))
      (Prt (App "soma" (Num 10)))

main = print (avaliaCmd ([],[]) p0)