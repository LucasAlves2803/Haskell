som :: Int -> Int
som i = if (i == 1) then 1
        else i + (som (i-1))
primo :: Int -> Int -> Bool
primo n i = if (i-1 <= 1) then True
            else if (rem n (i-1) == 0) then False
            else primo n (i-1)
main = print( primo 4 4)