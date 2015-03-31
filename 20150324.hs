-- Merge sort
-- Função auxiliar: merge.

merge :: [Int] -> [Int] -> [Int]
merge left right
	| left == [] = right
	| right == [] = left
	| head left < head right = (head left) : merge (tail left) (right)
	| otherwise = (head right) : merge (left) (tail right)

mergeSort :: [Int] -> [Int]
mergeSort items
	| length items < 2 = items
	| otherwise = merge (mergeSort left) (mergeSort right)
		where 
			left = take ((length items) `div` 2) items
			right = drop ((length items) `div` 2) items

-- Heapsort
-- Funções auxiliares: left, right, swap, minHeapify, buildMinHeap			
			
left :: Int -> Int
left i = 2*i + 1

right :: Int -> Int
right i = 2*(i + 1)

			
swap :: [Int] -> Int -> Int -> [Int]
swap list i j
	| list == [] = []
	| i < j = (take i list) ++ [list !! j] ++ (drop (i+1) (take j list)) ++ [list !! i] ++ (drop (j+1) list)
	| otherwise = (take j list) ++ [list !! i] ++ (drop (j+1) (take i list)) ++ [list !! j] ++ (drop (i+1) list)
	
minHeapify :: [Int] -> Int -> [Int]
minHeapify heap i
	| right i < length heap && (heap !! (left i) < heap !! (right i)) && (heap !! (left i) < heap !! i)  = minHeapify (swap heap i (left i)) (left i)
	| right i < length heap && (heap !! (right i) < heap !! (left i)) && (heap !! (right i) < heap !! i) = minHeapify (swap heap i (right i)) (right i)
	| left i < length heap && (heap !! (left i) < heap !! i) = minHeapify (swap heap i (left i)) (left i)
	| otherwise = heap
	
buildMinHeap :: [Int] -> Int -> [Int]
buildMinHeap heap i
	| i < 0  || i > ((length heap) `div` 2) = error "Invalid input."
	| i > 0 = buildMinHeap (minHeapify heap i) (i - 1)
	| otherwise = minHeapify heap 0
	
heapsort :: [Int] -> [Int]
heapsort list 
	| length list < 2 = list
	| otherwise = [head heap] ++ heapsort (minHeapify (tail heap) 0)
		where heap = buildMinHeap list ((length list) `div` 2)
		
-- ## Exercícios em sala ##

-- Função: menorMaior
-- Dados três inteiros a, b, c, retorna o par ordenado (x, y), onde x = min{a, b, c} e y = max{a, b, c}
-- Funções auxiliares: max, min (para 2 inteiros)

max :: Int -> Int -> Int
max a b
	| a > b = a
	| otherwise = b

min :: Int -> Int -> Int
min a b
	| a < b = a
	| otherwise = b

menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c
	| (a < b) && (b < c)= (a, c)
	| (a >= b) = ((min b c), (max a c))
	| otherwise = (a, b)
	
-- Função: ordenaTripla
-- Dada uma tripla (x1, x2, x3), retorna uma tripla (y1, y2, y3) tal que y1 <= y2 <= y3 e x_i = y_j numa certa ordem, 1 <= i, j <= 3.

ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a, b, c) = (x, y, z)
	where 
		x = fst (menorMaior a b c)
		z = snd (menorMaior a b c)
		y = (a + b + c) - (x + y)
		
-- Tipos: Ponto e Reta

type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

-- Funções: firstCoordinate, secondCoordinate
-- Dado um ponto P = (x, y), retornam x e y, respectivamente.

firstCoordinate :: Ponto -> Float
firstCoordinate p = fst p

secondCoordinate :: Ponto -> Float
firstCoordinate p = snd p

-- Função: isVertical
-- Dada uma reta r definida por dois pontos x1 e x2, retorna True se x1 == x2 ou False, caso contrário.

isVertical :: Reta -> Bool
isVertical line
	| fst line == snd line = True
	| otherwise = False
	
pontoY :: Float -> Reta -> Float
pontoY x line
	| isVertical line = error "Infinite solutions."
	| otherwise = (y2 - y1)*(x - x1) / (x2 - x1) 
		where	
			x1 = fst (fst line)
			x2 = fst (snd line)
			y1 = snd (fst line)
			y2 = snd (snd line)


-- Tipos de dados da questão
			
type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

baseExemplo :: BancoDados
baseExemplo = [(”Sergio”,”O Senhor dos Aneis”), (”Andre”,”Duna”), (”Fernando”,”Jonathan Strange & Mr. Norrell”), (”Fernando”,”A Game of Thrones”)]

-- ## As funções abaixo devem utilizar compreensão de listas ##

-- Função: membro
-- Dada uma lista de inteiros e um inteiro, retorna True se x pertence à lista ou False, caso contrário.

membro :: [Int] -> Int -> Bool
membro list member = (length ([x | x <- list, x == member]) > 0

