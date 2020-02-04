
import Debug.Trace
data Arvore = Folha | Galho Arvore Arvore
      deriving Show

a1 = Galho (Galho Folha Folha) (Galho Folha Folha)

a2 = Galho ( Galho (Galho Folha Folha) Folha) Folha

a3 = Galho Folha (Galho ( Galho Folha Folha ) (Galho Folha Folha))


-- ex1.1
n_folhas :: Arvore -> Int

n_folhas Folha = 1
n_folhas (Galho arv1 arv2) = n_folhas(arv1) + n_folhas(arv2)


-- main = print (n_folhas a1 , n_folhas a2, n_folhas a3)

-- ex1.2
altura :: Arvore -> Int
altura Folha = 1
altura (Galho arv1 arv2) = 1 + max (altura arv1) (altura arv2) 


-- main = print (altura a1, altura a2, altura a3)

-- ex1.3 
espelho :: Arvore -> Arvore
espelho Folha = Folha
espelho (Galho arv1 arv2) = (Galho (espelho arv2) (espelho arv1))

-- main = print (espelho a1, espelho a2, espelho a3)

data Arv = Filha | Ramo Int Arv Arv
         deriving Show
b1 = Ramo 1 (Ramo 2 Filha Filha) (Ramo 3 Filha Filha)

b2 = Ramo 4 ( Ramo 5 (Ramo 6 Filha Filha) Filha) Filha

b3 = Ramo 7 Filha (Ramo 8 ( Ramo 9 Filha Filha ) (Ramo 10 Filha Filha))


-- ex2.1
m_folhas :: Arv -> Int

m_folhas Filha = 1
m_folhas (Ramo _ arv1 arv2) = m_folhas(arv1) + m_folhas(arv2)

-- main = print (m_folhas b1, m_folhas b2, m_folhas b3)

-- ex2.2
alt :: Arv -> Int
alt Filha = 1
alt (Ramo _ arv1 arv2) = 1 + max (alt arv1) (alt arv2) 

-- main = print (alt b1, alt b2, alt b3)

-- ex2.3
esp :: Arv -> Arv
esp Filha = Filha
esp (Ramo z arv1 arv2) = (Ramo z (esp arv2) (esp arv1))

-- main = print (esp b1, esp b2, esp b3)

-- ex2.4
soma :: Arv -> Int
soma Filha = 0
soma (Ramo x arv1 arv2) = x + soma arv1 + soma arv2

-- main = print (soma b1, soma b2, soma b3)

-- ex2.5
dobra :: Arv -> Arv
dobra Filha = Filha
dobra (Ramo x arv1 arv2) = Ramo (2*x) (dobra arv1) (dobra arv2)

--main = print (dobra b1, dobra b2, dobra b3)
-- ex2.6
possui :: Int -> Arv -> Bool
possui v Filha = False
possui v (Ramo x arv1 arv2) = if (x /= v) then (possui v arv1) || (possui v arv2)
            else True

-- main = print (possui 7 b1, possui 7 b2,possui 7 b3)
-- ex2.7
possui2 :: Int -> Arv -> Bool
possui2 v Filha = False
possui2 v (Ramo x arv1 arv2) = if ( x < v ) then (possui2 v arv2)
    else if ( x > v) then ( possui2 v arv1)
    else True
-- main  = print (possui2 1 b4)
b4 = Ramo 16 (Ramo 4 (Ramo 1 Filha Filha) (Ramo 11 (Ramo 8 Filha Filha) (Ramo 14 Filha Filha))) (Ramo 19 Filha Filha)

-- ex2.8
maximo :: Int -> Arv -> Int
maximo v0 Filha = v0
maximo _ (Ramo x _ arv2) = maximo x arv2

v0f :: Arv -> Int
v0f (Ramo x _ _) = x

-- main = print ( maximo (v0f b4) b4 )  

-- ex2.9
insere :: Int -> Arv -> Arv
insere v Filha = Ramo v Filha Filha
insere v (Ramo x arv1 arv2) = if (v >= x) then (Ramo x arv1 (insere v arv2))
else (Ramo x (insere v arv1) arv2)

-- main = print (insere 20 b4)

--ex3.0
data Tree = Nulo Int | Ponto Tree Tree
         deriving Show      
c1 = Ponto (Nulo 1) (Nulo 2)
c2 = Ponto (Ponto (Ponto (Nulo 3) (Nulo 4)) (Ponto (Nulo 5) (Nulo 6))) (Ponto (Nulo 7) (Nulo 8))
 --  Ponto (Ponto (Nulo 7) (Nulo 8)) (Ponto (Ponto (Nulo 5) (Nulo 6) (Ponto (Nulo 3) (Nulo 4)))
--ex3.1
x_folhas :: Tree -> Int
x_folhas (Nulo _) = 1
x_folhas (Ponto arv1 arv2) = x_folhas(arv1) + x_folhas(arv2)

-- main = print ( x_folhas c2, x_folhas c1)
--ex3.2
dia :: Tree -> Int
dia (Nulo _ ) = 0
dia (Ponto arv1 arv2) = 1 + (max (dia arv1) (dia arv2))

-- main = print ( dia c1, dia c2)
--ex3.4
mirror :: Tree -> Tree
mirror (Nulo x ) = (Nulo x)
mirror (Ponto arv1 arv2) = Ponto (mirror arv2) (mirror arv1)

--main = print (mirror c1, mirror c2)
-- ex3.5
soma_x :: Tree -> Int
soma_x (Ponto arv1 arv2) = (soma_x arv1) + (soma_x arv2)
soma_x (Nulo x) = x
-- main = print (soma_x c1, soma_x c2)

-- ex3.6
dobra_x :: Tree -> Tree
dobra_x (Nulo x) = (Nulo (2*x))
dobra_x (Ponto arv1 arv2) = Ponto (dobra_x arv1) (dobra_x arv2)

-- main = print(dobra_x c1, dobra_x c2)

-- ex3.7
possui_x :: Int -> Tree -> Bool
possui_x v (Nulo x) = if (x /= v) then False
                      else True
possui_x v (Ponto arv1 arv2) = (possui_x v arv1) || (possui_x v arv2)

-- main = print (possui_x 3 c1, possui_x 4 c2)

c3 = Ponto ( Ponto (Ponto (Ponto (Nulo 1) (Nulo 2)) (Nulo 3)) (Nulo 4)) (Ponto (Nulo 5) (Ponto (Nulo 6) (Nulo 7)))
c4 = Ponto (Ponto (Nulo 3) (Nulo 4)) (Nulo 5)
c5 = Ponto 
            (Ponto 
                           (Ponto  
						             (Nulo 5) 
								     (Nulo 6)) 
				          (Nulo 10)) 
            (Nulo 13)

-- ex3.8
maxi :: Tree -> Int
maxi (Nulo x) = x
maxi (Ponto _ arv2) = maxi arv2

-- main = print ( maxi c3, maxi c4)

-- ex3.9
insere_x :: Int -> Tree -> Tree
insere_x v (Ponto arv1 arv2) = if (v <= (maxi arv1)) then Ponto (insere_x v arv1) arv2 
                               else  Ponto (Ponto (arv1) (Nulo v)) arv2 

main = print (insere_x 8 c3, insere_x 7 c5)