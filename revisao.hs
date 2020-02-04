-- Revisando map, filter e fold
import Data.Char
turma1 :: [ (String,Float,Float) ]
turma1 = [ ("Joao",7.5,3.5), ("Maria",10.0,8.0), ("Jose",5.0,3.0)] -- 50 alunos

nome :: (String,Float,Float) -> String
nome (nm,_,_) = nm ++ " "

nota1 :: (String,Float,Float) -> Float
nota1 (_,n1,_) = n1 

nota2 :: (String,Float,Float) -> Float
nota2 (_,_,n2) = n2


media :: (String,Float,Float) -> Float
media aluno = ((nota1 aluno) + (nota2 aluno)) / 2

medias :: [ (String,Float,Float) ] -> [Float]
medias turma = map media turma

-- Questão 1
notas1 :: [Float]
notas1 = map nota1 turma1

-- Maior8
maior8 :: (String,Float,Float)  -> Bool
maior8 turma = (nota1 turma >= 8) && (nota2 turma >= 8)

-- Questão 2
oitos :: [ (String,Float,Float) ] -> [String]
oitos turma = map nome (filter maior8 turma1)



-- Questão 3 

-- Notas2

notas2 :: [Float]
notas2 = map nota2 turma1


todas turma = ((foldr (+) 0 (map nota1 turma))/3, (foldr (+) 0 (map nota2 turma))/3, (foldr (+) 0 (medias turma))/3)


-- Questão 4
baixas :: [Float]

baixas = filter baixo5 ((map nota1 turma1) ++ (map nota2 turma1))

baixo5 :: Float -> Bool
baixo5 nota = nota <= 5

-- Questão 5


resultado :: Float -> String
resultado media = if (media >= 5) then
                   "Aprovado" 
				  else
                   "Reprovado" 

pretty :: [(String,Float,Float)] -> String
pretty turma = foldr (++) ("") (map conv turma) -- o map retorna uma cadeia de String -> [String] e o fold concatena essa cadeia

conv :: (String, Float, Float) -> String
conv aluno = (nome aluno ++ " " ++ show (media aluno) ++ " " ++ resultado (media aluno))	-- cria uma string com o nome, média e situação				
 											 
