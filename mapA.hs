data Arv a = Folha | Ramo a (Arv a) (Arv a)
       deriving Show

mapa :: (a -> b) -> (Arv a) -> (Arv b)
mapa f Folha = Folha
mapa f (Ramo x arv1 arv2) = Ramo (f x) (mapa f arv1) ( mapa f arv2)

-- A função paridade triplica o nº e soma uma unidade
paridade :: Int -> Int
paridade x =  (trip x)+1  
trip :: Int -> Int
trip x = 3*x

bn :: Int -> Int
bn x = x `mod` 2

sub1 = Ramo 10 (Ramo 5 (Ramo 3 Folha Folha) (Ramo 7 Folha Folha)) (Ramo 15 (Ramo 13 (Ramo 12 Folha Folha) (Ramo 14 Folha Folha)) (Ramo 18 Folha Folha))
sub2 = Ramo 25 (Ramo 23 ( Ramo 21 Folha Folha) (Ramo 24 Folha Folha)) (Ramo 30 ( Ramo 27 Folha Folha) (Ramo 33 Folha Folha))
pai = Ramo 20 sub1 sub2

a4 = mapa paridade sub1
a2 = mapa bn sub1 
a3 = mapa trip sub1

foldA :: (a -> a -> a) -> a -> Arv a -> a
foldA f ini Folha = ini
foldA f ini (Ramo x arv1 arv2) = f x (f (foldA f ini arv1) (foldA f ini arv2))

a5 = Ramo 1 (Ramo 2 Folha Folha) (Ramo 3 Folha Folha)

main = print (foldA maxi 0 a5, foldA maxi 0 pai)

soma :: Int -> Int -> Int
soma x y = x+y
-- f x  (f (foldA f max arv1) (foldA f max arv2))

maxi :: Ord a => a -> a -> a
maxi x y = if ( x > y ) then x
          else y

mini :: Ord a => a -> a -> a
mini x y = if ( x < y) then x
          else y