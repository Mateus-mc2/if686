-- ## Trabalho 10 (28/04/2015) ##

-- Questão 1

{- Item (a): 
	**Função: foldr (+).(.).map
	
	# Resolução #

--> Queremos saber qual a imagem de (+).(.).map (x). O primeiro argumento de map é do tipo (a -> b),
logo (.).map (x) = (.) (map (x)), onde map (x) é do tipo ([a] -> [b]).
--> Desta forma, (.).map retorna uma função do tipo ((c -> [a]) -> c -> [b]),
pois (.) :: ([a] -> [b]) -> (c -> [a]) -> c -> [b].
--> Por fim, sabemos que (+) :: (Num t) => t -> t -> t. Então t = ((c -> [a]) -> c -> [b]),
e a imagem desta função é ((c-> [a]) -> c -> [b]) -> (c -> [a]) -> c -> [b].
--> Segue daí que o tipo de (+).(.).map é (a -> b) -> ((c-> [a]) -> c -> [b]) -> (c -> [a]) -> c -> [b].
--> Nota: o argumento de foldr NÃO É a função ((+).(.).map, e sim (+).(.).map.
--> Isto significa que a função recebida tem como primeiro argumento um elemento do tipo t = ((c-> [a]) -> c -> [b]),
e o próximo, do tipo (a -> b), logo:

(RESPOSTA)	foldr (+).(.).map :: (Num ((c-> [a]) -> c -> [b])) => (a -> b) -> [((c-> [a]) -> c -> [b])] -> (c-> [a]) -> c -> [b] 

 - Item (b):
	**Função: (\x y z -> foldr z x y).map
	
	# Resolução #

--> Primeiro, vamos descobrir o tipo de (\x y z -> foldr z x y).
--> Como foldr :: (a -> b -> b) -> b -> [a] -> b, temos que z é do tipo (a -> b -> b), x é do tipo b e y, do tipo [a].
--> Então (\x y z -> foldr z x y) :: b -> [a] -> (a -> b -> b) -> b.
--> De maneira análoga à primeira questão, map (x) é do tipo ([a1] -> [b1]).
--> Então o argumento x da função (\x y z -> foldr z x y) é do tipo b = [a1] -> [b1],
logo (\x y z -> foldr z x y) = ([a1] -> [b1]) -> [a] -> (a -> ([a1] -> [b1]) -> [a1] -> [b1]) -> [a1] -> [b1].
--> Segue daí que:

(RESPOSTA)	(\x y z -> foldr z x y).map :: (a1 -> b1) -> [a] -> (a -> ([a1] -> [b1]) -> [a1] -> [b1]) -> [a1] -> [b1]

 - Item (c):
	**Função: map.((.) (foldr (++) (foldr (++) [] [[1], [2]])))
	
	# Resolução #

--> Primeiro, temos que foldr (++) [] [[1], [2]] é do tipo [a1], onde a1 é da classe Num.
--> Então foldr (++) (foldr (++) [] [[1], [2]]) :: [[a1]] -> [a1]. 
--> Sabemos que (.) :: (b -> c) -> (a -> b) -> a -> c.
--> Como o primeiro argumento é do tipo [[a1]] -> [a1], (.) possui tipo (a -> [[a1]]) -> a -> [a1].
--> Segue daí que map.((.) (foldr (++) (foldr (++) [] [[1], [2]]))) recebe um argumento do tipo (a -> [[a1]]),
e map, um argumento do tipo a -> [a1], logo:

(RESPOSTA)	map.((.) (foldr (++) (foldr (++) [] [[1], [2]]))) :: (Num a1) => (a -> [[a1]]) -> [a] -> [[a1]]

 - Item (d):
	**Função: (foldr).(.)$(!!)
	
	# Resolução #
	
--> Sabemos que foldr :: (a -> b -> b) -> b -> [a] -> b e (.) :: (b1 -> c) -> (a1 -> b1) -> a1 -> c.
--> Portanto, o argumento recebido é do tipo (b1 -> c), e o primeiro qrgumento de foldr é (a1 -> b1) -> a1 -> c,
logo a = (a1 -> b1), e b = a1 = c, ou seja, a = (b -> b1).
--> Então ((foldr).(.)) :: (b1 -> b) -> b -> [b -> b1] -> b.
--> Ora, como (!!) :: [t] -> Int -> t, a função de entrada para ((foldr).(.)) é do tipo [t] -> Int -> t,
logo (b1 -> b) = [t] -> (Int -> t), ou seja, b1 = [t] e  b = (Int -> t).
--> Por fim:

(RESPOSTA)	(foldr).(.)$(!!) :: (Int -> t) -> [(Int -> t) -> [t]] -> t	
-} 