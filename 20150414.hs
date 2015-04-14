-- ## Trabalho 07 (14/04/2015) ##

-- ## Trabalho 07 (14/04/2015) ##

-- Questão 01

compose :: (Eq t) => (t -> t) -> [(t -> t)] -> [(t -> t)]
compose g [] = []
compose g (f:fs) = (g . f) : compose g fs


-- Questão 02

-- {
-- #### Codigo do trabalho 06 para a questão 2 ####
-- Tipos auxiliares: Vertex (representa um vértice do grafo) e Edge (representa uma aresta com seu respectivo peso)
type Vertex t = t
type Edge t = (Vertex t, Vertex t, Int) -- Vértices adjacentes + Peso da aresta

data Graph t = Graph [Vertex t] [Edge t] deriving (Eq, Ord, Show)

myGraph :: Graph Int
myGraph = Graph [1, 2, 3, 4, 5, 6, 7, 8] [(1, 2, 1), (1, 3, 1), (3, 4, 1), (3, 5, 1), (6, 7, 1), (6, 8, 1)]	
-- }

mapGraph :: (Eq t) => (t -> t) -> Graph t -> Graph t
mapGraph f (Graph v adj) = Graph (map f v) adj 

foldGraph :: (Eq t) => (t -> t -> t) -> Graph t -> t 
foldGraph f (Graph v adj) = foldr1 f v 
-- Questão 3

data Tree t = NilT | Node t (Tree t) (Tree t) deriving (Eq, Ord, Show)

-- Árvore para teste das funções

myTree :: Tree Int
myTree = Node 5 (Node 9 (Node 7 NilT NilT) (Node 15 (Node 6 NilT NilT) (Node 2 NilT NilT))) (Node 23 (Node 12 NilT NilT) (NilT))

filterTreeAux :: (Eq t) => (t -> Bool) -> Tree t -> [Tree t]
filterTreeAux f NilT = [NilT]
filterTreeAux f (Node x lft rgt) = [new_return] ++ forest_lft ++ forest_rgt
	where
	all_lft = filterTreeAux f lft
	all_rgt = filterTreeAux f rgt
	
	new_lft = head(all_lft)
	new_rgt = head(all_rgt)
	
	forest_lft = if (f x) then tail(all_lft) else all_lft
	forest_rgt = if (f x) then tail(all_rgt) else all_rgt
	
	new_return = if (f x) then (Node x new_lft new_rgt) else NilT
	
filterTree :: (Eq t) => (t -> Bool) -> Tree t -> [Tree t]
filterTree f tree = forest_clr
	where
	forest_dirty = filterTreeAux f tree
	forest_clr = [value | value <- forest_dirty, not(value == NilT)]

-- ## Exercícios em sala ##

-- Questão 1

filterList :: [[Int]] -> Int -> [[Int]]
filterList lists threshold = filter ((>= threshold) . (foldr (+) 0)) lists

-- Questão 2

isElement :: (Eq t) => [t] -> t -> Bool
isElement list x
	| list == [] = False
	| ((head list) == x) = True
	| otherwise = isElement (tail list) x

set :: (Eq t) => [t] -> [t]
set list
	| list == [] = []
	| isElement (tail list) (head list)  = set (tail list)
	| otherwise = [head list] ++ set (tail list)
	
inter :: (Eq t) => [t] -> [t] -> [t]
inter listA listB = filter (isElement setB) (setA)
	where
		setA = set listA
		setB = set listB		