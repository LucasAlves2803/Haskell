-- revisao de arvores
data Arvore = Folha | Galho Arvore Arvore
     deriving Show

a1 = Galho (Galho Folha Folha) (Galho Folha Folha)
a2 = Galho (Galho (Galho Folha Folha) Folha) Folha
a3 = Galho Folha (Galho (Galho Folha Folha) (Galho Folha Folha))
a4 = Galho Folha (Galho Folha (Galho Folha Folha))
folhas :: Arvore -> Int
folhas Folha = 1
folhas (Galho arv1 arv2) = folhas(arv1) + folhas(arv2)

altura :: Arvore -> Int
altura Folha = 0
altura (Galho arv1 arv2)= max (altura arv1 + 1)  (altura arv2 + 1)

espelho :: Arvore -> Arvore

espelho Folha = Folha
espelho (Galho arv1 arv2) = Galho (espelho arv2) (espelho arv1)


main = print (espelho a2)