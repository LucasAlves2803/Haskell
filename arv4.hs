data Arv a = Folha | Ramo a (Arv a) (Arv a) 
         deriving Show
{- data Turma = Aluno Int String         
      		deriving Show
		n1 :: Turma
	n1 = Aluno 22 "Lucas"
-}

d1 = Ramo 5 Folha (Ramo 2.5 Folha Folha)
d2 = Ramo 3 ( Ramo 7.7 ( Ramo 9.8 Folha Folha) (Ramo 10 Folha Folha)) (Ramo 3.3 Folha Folha) 
-- Calcula a quantidade de folhas
m_folhas :: Arv a -> Int

m_folhas Folha = 1
m_folhas (Ramo _ arv1 arv2) = (m_folhas arv1 ) + (m_folhas arv2 )

-- 2 Calcula a altura
altura :: Arv a -> Int

altura Folha = 0
altura (Ramo _ arv1 arv2) = 1 + max (altura arv1) (altura arv2)




-- 3 espelha a árvore
espelho :: Arv a -> Arv a

espelho Folha = Folha
espelho (Ramo z arv1 arv2) = Ramo z (espelho arv2) (espelho arv1)

-- 4 soma os valores guardados nos Ramos
soma :: Num a => Arv a -> a

soma (Ramo x arv1 arv2) = x + soma arv1 + soma arv2
soma Folha = 0

-- 5 dobra os valores guardados nos ramos
dobra :: Num a => Arv a -> Arv a
dobra Folha = Folha
dobra (Ramo x arv1 arv2) = Ramo (2*x) (dobra arv1) (dobra arv2)

-- 6 descobre se um número está na árvore
possui :: Ord a => a -> Arv a -> Bool
possui v Folha = False
possui v (Ramo x arv1 arv2) = if ( v /= x) then (possui v arv1) || (possui v arv2)
else True



