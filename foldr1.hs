 
data Lista a = No a (Lista a) | Vazio
   deriving Show
lucas :: Lista Int
lucas = No 1 (No 2 (No 3 (No 4 Vazio)))

soma :: Int -> Int -> Int
soma x y = x + y


fold1 :: (a -> a -> a) -> a -> Lista a -> a
fold1 f ini Vazio = ini
fold1 f ini (No x xs) = f x (fold1 f ini xs)

main = print (fold1 soma 0 lucas)