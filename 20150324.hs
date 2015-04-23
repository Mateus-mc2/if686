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
secondCoordinate p = snd p

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
baseExemplo = [("Sergio","O Senhor dos Aneis"), ("Andre","Duna"), ("Fernando","Jonathan Strange & Mr. Norrell"), ("Fernando","A Game of Thrones")]

-- ## As funções abaixo devem utilizar compreensão de listas ##

-- Função: membro
-- Dada uma lista de inteiros e um inteiro, retorna True se x pertence à lista ou False, caso contrário.

membro :: [Int] -> Int -> Bool
membro list member = (length ([x | x <- list, x == member])) > 0

-- Função: livros
-- Dado um banco de dados e uma pessoa, retorna todos os livros relacionados àquela pessoa

livros :: BancoDados -> Pessoa -> [Livro]
livros database person = [b | (a, b) <- database, a == person]

-- Função: emprestimos
-- Dada uma base de dados e um livro, retorna os nomes de todas as pessoas que solicitaram empréstimo do livro

emprestimos ::  BancoDados -> Livro ->[Pessoa]
emprestimos database book = [a | (a, b) <- database, b == book]

-- Função: emprestado
-- Verifica se um livro já foi emprestado alguma vez

emprestado :: BancoDados -> Livro -> Bool
emprestado database book = (length (emprestimos database book)) > 0

-- Função: qtdEmprestimos
-- Retorna o número de vezes que uma pessoa solicitou empréstimo de algum livro

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos database person = length (livros database person)

-- Função: devolver
-- Atualiza o banco de dados, removendo da lista o par que relaciona a pessoa ao livro emprestado

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver database person book = [(a, b) | (a, b) <- database, not((a == person) && (b == book))]