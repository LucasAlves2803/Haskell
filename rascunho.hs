{-
-- 2 Calcula a altura

altura :: Arv a -> Int

altura Folha = 0
altura (Ramo _ arv1 arv2) = 1 + max (altura arv1) (altura arv2)




-- 3 espelha a árvore
espelho :: Arv a -> Arv a

espelho Folha = Folha
espelho (Ramo _ arv1 arv2) = Ramo z (espelho arv2) (espelho arv1)
-}