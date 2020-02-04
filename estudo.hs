type Time = (String, (Int,Int,Int),(Int,Int))

cariocas :: [Time]

cariocas = [("Flamengo", (10,2,2), (104,26)),("Fluminense", (4,4,6),(56,14)),("Botafogo",(8,0,6),(66,15)),("Vasco",(5,7,2),(45,48))]




nome :: [String]

nome = map f_cap cariocas

f_cap :: Time -> String

f_cap (nome,_,_) = nome

pontos :: [(String,Int)]

pontos = map f_pt cariocas

f_pt :: Time -> (String, Int)

f_pt (nome,ved,_) = (nome, get_pt ved)

get_pt :: (Int,Int,Int) -> Int

get_pt (x,y,z) = 3*x + y 



maisN :: [Time] -> Int -> [String]

maisN clubes n = map f_cap ( filter fn clubes) 
                    where  
                       fn :: Time -> Bool
                       fn (_,ved,_) = (get_pt ved) > n 

main = print (maisN cariocas 33)

campeao :: (String, Int)
campeao = foldr melhor ("",0) pontos

melhor :: (String, Int) -> (String, Int) -> (String,Int)
melhor (t1,p1) (t2,p2) = if (p1 >= p2) then (t1,p1)
                         else (t2,p2)
