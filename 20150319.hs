-- Função: double
-- Dada uma lista L = (x_1, ... , x_n), retorna outra lista L' = (x'_1, ... , x'_n), onde x'_i = 2*x_i, 1 <= i <= n

double :: [Int] -> [Int]
double list
	| length list == 1 = [2*(head list)]
	| otherwise = [2*(head list)] ++ double (tail list)
	
-- Função: member
-- Sejam X e x uma lista de elementos de um conjunto F e um elemento de F, respectivamente.
-- Retorna True se x pertence a X ou False, caso contrário.

member :: [Int] -> Int -> Bool
member list x
	| list == [] = False
	| x == head list = True
	| otherwise = member (tail list) x
	
-- Função: digits
-- Dada uma cadeia de caracteres w, retornar uma cadeia w' contendo apenas os dígitos presentes em w.

digits :: String -> String
digits str
	| str == [] = []
	| (head str) >= '0' && (head str) <= '9' = [head str] ++ digits (tail str)
	| otherwise = digits (tail str)

-- Função: sumPairs
-- Dadas duas listas L1 e L2 de mesmo tamanho, retorna uma lista L = L1 + L2.
	
sumPairs :: [Int] -> [Int] -> [Int]
sumPairs l1 l2
	| (length l1) /= (length l2) = error "Lists must have the same size."
	| (length l1) > 0 = [(head l1) + (head l2)] ++ sumPairs (tail l1) (tail l2)
	| otherwise = [];

-- Quicksort

quicksort :: [Int] -> [Int]
quicksort items
	| length items < 2 = items
	| otherwise = quicksort [x | x <- tail items, x <= head items] ++ [head items] ++ quicksort [x | x <- tail items, x > head items]