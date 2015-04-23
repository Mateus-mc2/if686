-- Trabalho 3 (26/03/2015)

type HashTable v k = [(v, k)]

myHashTable :: HashTable String Int
myHashTable = [("Introduction to Linear Algebra", 11), ("Singular Value Decomposition", 5), ("Fate/Stay Night", 19), ("Mateus", 3)]

hasKey :: (Eq k) => HashTable v k -> k -> Bool
hasKey table key
	| (length table) == 0 = False
	| (key == snd (head table)) = True
	| otherwise = hasKey (tail table) key

get :: (Eq k) => HashTable v k -> k -> v
get table key
	| not (hasKey table key) = error "Key does not exist."
	| snd (head table) == key = fst (head table)
	| otherwise = get (tail table) key
	
put :: (Eq k) => HashTable v k -> v -> k -> HashTable v k
put table value key
	| (hasKey table key) = error "Key already exists."
	| otherwise = table ++ [(value, key)]
	
remove :: (Eq k) => HashTable v k -> k -> HashTable v k
remove table key
	| not (hasKey table key) = error "Key does not exist."
	| key == snd (head table) = tail table
	| otherwise = [(head table)] ++ remove (tail table) key
	
listToSet :: (Eq t) => [t] -> [t]
listToSet list
	| (length list) == 0 = []
	| otherwise = [head list] ++ listToSet [x | x <- (tail list), not (x == (head list))]

-- Desta vez irei utilizar casamento de padrões
intersectionLength :: (Eq t) => [t] -> [t] -> Int
intersectionLength [] _ = 0
intersectionLength _ [] = 0
intersectionLength (a:tSetA) setB = length [x | x <- setB, x == a] + intersectionLength tSetA setB
-- intersectionLength setA setB
	-- | ((length setA) == 0) || ((length setB) == 0) = 0
	-- | otherwise = length [x | x <- setB, x == (head setA)] + intersectionLength (tail setA) setB

comparaConjuntos :: (Eq t) => [t] -> [t] -> String
comparaConjuntos listA listB
	| lAIntB == 0 = "Conjuntos disjuntos."
	| (lAIntB == (length setA)) && (lAIntB == (length setB)) = "A igual a B."
	| (lAIntB == (length setA)) = "B contem A."
	| (lAIntB == (length setB)) = "A contem B."
	| otherwise = "A interseciona B"
		where
			setA = listToSet listA
			setB = listToSet listB
			lAIntB = intersectionLength setA setB
			
-- ## Exercícios em sala (26/03/2015) ##

-- Função: take
-- Devolve os n primeiros elementos de uma lista

take2 :: [t] -> Int -> [t]
take2 list n 
	| n == 0 = []
	| otherwise = (head list):(take2 (tail list) (n-1))
	
-- Função: drop
-- Remove os n primeiros elementos de uma lista

drop2 :: [t] -> Int -> [t]
drop2 list n
	| n == 0 = list
	| otherwise = drop2 (tail list) (n-1)
	
-- Função: takeWhile
-- Devolve os n primeiros elementos da lista que satisfaçam um predicado

takeWhile2 :: [t] -> (t -> Bool) -> [t]
takeWhile2 list p
	| not (p (head list)) = []
	| otherwise = (head list):(takeWhile2 (tail list) p)

-- Função: dropWhile
-- Remove os n primeiros elementos da lista que satisfaçam um predicado

dropWhile2 :: [t] -> (t -> Bool) -> [t]
dropWhile2 list p
	| not (p (head list)) = list
	| otherwise = dropWhile2 (tail list) p
	
-- Função: quicksort (ordenar tipos genéricos)

quicksort :: (Ord t) => [t] -> [t]
quicksort list
	| length list < 2 = list
	| otherwise = (quicksort [x | x <- (tail list), x <= (head list)]) ++ [head list] ++ (quicksort [x | x <- (tail list), x > (head list)])
	
-- Função: agrupar
-- Funções auxiliares: joinLists, countElem, mainAux

joinLists :: (Eq t) => [[t]] -> [t]
joinLists lists
	| lists == [] = []
	| otherwise = (head lists) ++ joinLists (tail lists)

countElem :: (Eq t) => [t] -> t -> Int
countElem list elem
	| list == [] = 0
	| (head list) == elem = 1 + countElem (tail list) elem
	| otherwise = countElem (tail list) elem
	
mainAux :: (Eq t) => [t] -> [t] -> [(t, Int)]
mainAux list set
	| set == [] = []
	| otherwise = [((head set), (countElem list (head set)))] ++ (mainAux newList (tail set))
		where
			newList = [x | x <- list, not (x == (head set))]

agrupar :: (Eq t) => [[t]] -> [(t, Int)]
agrupar lists = mainAux fullList fullSet
	where
		fullList = joinLists lists
		fullSet = listToSet fullList