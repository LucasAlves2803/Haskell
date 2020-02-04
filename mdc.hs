-- Programa que calcula o mdc de dois números

mdc :: Int -> Int -> Int

mdc di dv =  if ((di `mod` dv) /= 0) then 
			(mdc dv (di `mod` dv))
		  else
			dv