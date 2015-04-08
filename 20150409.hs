-- ## Trabalho 6 (09/04/2015) ##

-- Questão 1

-- Tipos auxiliares: Vertex (representa um vértice do grafo) e Edge (representa uma aresta com seu respectivo peso)

type Vertex t = t
type Edge t = (Vertex t, Vertex t, Int) -- Vértices adjacentes + Peso da aresta

data Graph t = Graph [Vertex t] [Edge t] deriving (Eq, Ord, Show)

-- Questão 2

get2nd :: (Eq t) => Edge t -> Vertex t
get2nd (_, a, _) = a

myGraph :: Graph Int
myGraph = Graph [1, 2, 3, 4, 5, 6, 7, 8] [(1, 2, 1), (1, 3, 1), (3, 4, 1), (3, 5, 1), (6, 7, 1), (6, 8, 1)]

isElement :: (Eq t) => Vertex t -> [Vertex t] -> Bool
isElement v list
	| list == [] = False
	| (head list) == v = True
	| otherwise = isElement v (tail list)

listToSet :: (Eq t) => [Vertex t] -> [Vertex t]
listToSet list
	| list == [] = []
	| isElement (head list) (tail list) = listToSet (tail list)
	| otherwise = [head list] ++ listToSet (tail list)
	
dfs :: (Eq t) => Graph t -> Vertex t -> [Vertex t] -> [Vertex t]
dfs (Graph vertices edges) src marked
	| adjEdges == [] = listToSet ([src] ++ marked)
	| otherwise = dfs (Graph vertices edges) src (listToSet markedList)
		where
			markedList = dfs (Graph vertices edges) (get2nd (head adjEdges)) (listToSet([src] ++ marked))
			adjEdges = [(a, b, c) | (a, b, c) <- edges,  (a == src) && (not (isElement b marked))]

isGraphVertex :: (Eq t) => Graph t -> Vertex t -> Bool
isGraphVertex (Graph vertices edges) value
	| vertices == [] = False
	| isElement value marked = True
	| otherwise = isGraphVertex (Graph newVertices edges) value
		where
			marked = (dfs (Graph vertices edges) (head vertices) [])
			newVertices = [x | x <- vertices, not (isElement x marked)] 