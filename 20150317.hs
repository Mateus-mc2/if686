vendas :: Int -> Int
vendas n 
	| n <= 0 = 0
	| (n > 0) && (n <= 5) = 5
	| otherwise = 10

funct :: Int -> Int -> Int
funct s n
	| n < 0 = 0
	| (vendas n) == s = 1 + funct s (n - 1)
	| otherwise = funct s (n - 1)