-- exemplo
soma :: Int -> [Int] -> Int

lista = [1,3,4,5,6]
c = soma 3 lista
soma x l = if (x < 4) 
           then (x + y) 
		         where y =  l !! 1
		   else	(x + y) 
		         where y = l !! 3
