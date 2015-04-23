--	##### Questão 1 #####
--
--	Em haskell, é possível escrever uma função que implementa um 
--	comportamento para um conjunto de tipos. Isso é feito com o 
--	uso das classes de tipo que agregam tipos com comportamentos 
--	similares sob uma ótica, exemplo: a classe Eq engloba todos os 
--	tipos que implementam o teste de igualdade. Essa inovação do Haskell 
--	 métodos que se comportam da mesma forma 
--	para tipos diferentes, ou ainda evita o uso de tipos genéricos e templates 
--	que são muito menos legíveis e oferece pouco tratamento de erro, uma vez
--	que o tipo só é considerado em tempo de execução. Já em Haskell, os erros 
--	ficam muito mais legíveis pois são lançados em tempo de compilação com informação clara 
--	a respeito dos tipos e da falha.
--	A desvantagem é que funções em haskell devem possuir identificador único, 
--	então não dá pra fazer a sobrecarga de função alterando apenas parâmetros, 
--	ou seja: ações que poderiam ser feitas de várias formas terão que ter 
--	funções com nomes diferentes.


--	##### Questão 2 #####
next_term :: Int->Int->[Int]->[Int]
next_term c e []
 | (c == 0) = []
 | otherwise = [c] ++ [e]
next_term c e (a:as)
 | (a == e) = next_term (c+1) (e) (as)
 | otherwise = [c] ++ [e] ++ next_term (0) (a) (a:as)
 
 

term_by_term :: Int->[Int]->[Int]
term_by_term n (a:as)
 | (n == 0)  = (a:as)
 | otherwise = term_by_term (n-1) (next_term (0) (a) (a:as))


lookAndSay :: Int->[Int]
lookAndSay n = term_by_term (n-1) [1]

--	##### Questão 3 #####

type Vertex t = t
type Edge t = (t, t)
type Graph t = ([Vertex t], [Edge (Vertex t)])

-- Testing data

myGraph :: Graph Int
myGraph = ([1, 2, 3, 4, 5], [(1, 2), (1, 4), (2, 1), (2, 3), (3, 2), (3, 5), (4, 1), (5, 3)])

-- Functions

quicksort :: (Ord t) => [t] -> [t]
quicksort items
	| length items < 2 = items
	| otherwise = quicksort [x | x <- tail items, x <= head items] ++ [head items] ++ [x | x <- tail items, x > head items]

isElementOf :: (Ord t) => Vertex t -> [Vertex t] -> Bool
isElementOf v list
	| list == [] = False
	| v == (head list) = True
	| otherwise = isElementOf v (tail list)
	
findPath :: (Ord t) => Graph t -> Vertex t -> Vertex t -> [Vertex t] -> [Edge (Vertex t)] -> [Edge (Vertex t)]
findPath graph src dst marked path
	| src == dst = path
	| edges == [] = []
	| otherwise = findPath graph (snd (head edges)) dst ([src] ++ marked) (path ++ [head edges])
		where 
			reachable = [(a, b) | (a, b) <- snd graph, (a == src)]
			edges = [x | x <- reachable, not (isElementOf (snd x) marked)]
			
search :: (Ord t) => Graph t -> Vertex t -> Vertex t -> [Edge (Vertex t)]
search graph src dst = findPath graph src dst [] []


--	##### Questão 4 #####
-- # Terminar função #
--filtroMediana :: [[Int]] -> Int -> [[Int]]
--filtroMediana :: 

-- Mergesort
mergesort :: [Int] -> [Int]
mergesort [] = []
mergesort [x] = [x]
mergesort xs   =  merge (mergesort (fst (divideInHalf xs))) (mergesort (snd (divideInHalf xs)))

merge :: [Int] -> [Int] -> [Int]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
 | x <= y = x : merge (xs) (y:ys)
 | otherwise = y : merge (x:xs) (ys)

divideInHalf :: [Int] -> ([Int], [Int]) 
divideInHalf x = (take (length x `div` 2) x, drop (length x `div` 2) x) 

-- Mediana
median :: [Int] -> Int
median xs = head (snd(divideInHalf xs))