-- Campeonato
brasil19 = [
    ("Corinthians", ( 8,7,2), (19, 9)),  -- (nome, (v,e,d), (gp,gc))
    ("Goias",       ( 6,3,8), (16,27)),  --   (vitorias, empates, derrotas)
    ("Flamengo",    (11,3,3), (38,18)),  --   (gols a favor, gols contra)
    ("Gremio",      ( 5,7,5), (20,21))]

-- Questão 6
type Tabela  = (String,(Int,Int,Int),(Int,Int))

getGols :: (String, (Int,Int,Int), (Int,Int)) -> (Int,Int)
getGols (nome,ved,gols) = gols


totpart :: Tabela -> Int
totpart (_,(v,e,d),_)= v+d+e

gols :: (Int,Int) -> Int
gols (x,y) = x+y

mediagols :: [Tabela] -> Float
mediagols tabela =  fromIntegral (foldr (+) 0 (map gols (map getGols tabela))) / fromIntegral (foldr (+) 0 (map totpart tabela))
-- Questão 5
nome :: Tabela -> (String)
nome (nom,_,_)= nom
ponto :: (String,(Int,Int,Int),(Int,Int)) -> (String,Int)
ponto (nome,(v,e,d),_) = (nome, 3*d + d)
pontos =  map ponto brasil19
comp :: Int -> (String, Int) -> Bool
comp v (_,pt) = pt > v

maisN :: [(String, (Int,Int,Int), (Int,Int))] -> Int -> [String]
maisN tabela v = map nome (filter comp v pontos)  

--main = print(mediagols brasil19)