-- ## Trabalho 09 (23/04/2015) ##

-- Questão 1: fazer os exercícios de todos os slides vistos até esta data (arquivo .txt)

-- Questão 2 (a completar)

type Vertex t = t
type Edge t = (Vertex t, Vertex t, Int) -- Vértices adjacentes + Peso da aresta

type Node t = (Vertex t, Int) -- Vértice + Distância até a origem
type Pair t = (Vertex t, Vertex t) -- Aresta sem pesos

data Graph t = Graph [Vertex t] [Edge t] deriving (Eq, Ord, Show)

getFirst :: Edge t -> Vertex t
getFirst (u, _, _) = u

getSecond :: Edge t -> Vertex t
getSecond (_, v, _) = v

getThird :: Edge t -> Int
getThird (_, _, w) = w

refresh :: (Eq t) => Vertex t -> [Edge t] -> [Node t] -> [Pair t] -> ([Node t], [Pair t])
refresh vertex edges queue prev
	| edges == [] = (queue, prev)
	| (getFirst (head edges) == vertex) && 
		where
			-- alt = dist[vertex] + length(vertex, adjVertex)

extractPath :: (Eq t) => Vertex t -> Vertex t -> [Pair t] -> [Pair t] -> [Pair t]
extractPath src target incompletePath realPath
	| target == src = realPath
	| otherwise = extractPath src parent incompletePath ((parent, target):realPath)
		where
			auxList = [x | x <- incompletePath, (snd x) == target]
			parent = fst (head auxList)

aux :: (Eq t) => Graph t -> Vertex t -> Vertex t -> [Node t] -> [Pair t] -> [Pair t]
aux (Graph vertices edges) src target minQueue prev
	| minQueue == [] = extractPath src target prev []
	| 

dijkstra :: (Eq t) => Graph t -> Vertex t -> Vertex t -> [Pair t]
dijkstra (Graph vertices edges) src target = aux (Graph vertices edges) src target queue []
	where
		maxDist = foldr ((+).(getThird)) 0 edges
		queue = (src, 0):[(x, maxDist) | x <- vertices, not(x == src)]

geraFuncaoMenorCaminho :: (Eq t) => Graph t -> (Vertex t -> Vertex t -> [Pair t])
geraFuncaoMenorCaminho graph = findShortestPath
	where
		findShortestPath src target = dijkstra graph src target