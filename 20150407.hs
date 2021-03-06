-- ## Exercícios feitos em sala ##

-- Questão 1

data Shape = Circle Float | Rectangle Float Float

pow :: Float -> Float -> Float
pow a n
	| n == 0 = 1
	| otherwise = a*(pow a (n-1))

area :: Shape -> Float
area (Circle r) = pi*(pow r 2)
area (Rectangle a b) = a*b

-- Questão 2

data DiaSemana = Domingo | Segunda Int [String] | Terca Int [String] | Quarta Int [String]
				| Quinta Int [String] | Sexta Int [String] | Sabado

-- Item (i)	
			
isWeekend :: DiaSemana -> Bool
isWeekend Sabado = True
isWeekend Domingo = True
isWeekend _ = False

-- Item (ii)

contains :: [String] -> String -> Bool
contains list str
	| list == [] = False
	| (head list) == str = True
	| otherwise = contains (tail list) str
	
diaPLC :: DiaSemana -> Bool
diaPLC (Sabado) = False
diaPLC (Domingo) = False
diaPLC (Segunda _ aulas) = contains aulas "PLC"
diaPLC (Terca _ aulas) = contains aulas "PLC"
diaPLC (Quarta _ aulas) = contains aulas "PLC"
diaPLC (Quinta _ aulas) = contains aulas "PLC"
diaPLC (Sexta _ aulas) = contains aulas "PLC"

-- Questão 3

data Tree t = NilT | Node t (Tree t) (Tree t) deriving (Eq, Ord, Show)

-- Questão 4

data Expr = Literal Int | Add Expr Expr | Subtract Expr Expr
data List t = Nil | Constructor t (List t) deriving Show

-- Função: showExpr

showExpr :: Expr -> String
showExpr (Literal l) = show l
showExpr (Add expr1 expr2) = "(" ++ (showExpr expr1) ++ ") + (" ++ (showExpr expr2) ++ ")"
showExpr (Subtract expr1 expr2) = "(" ++ (showExpr expr1) ++ ") - (" ++ (showExpr expr2) ++ ")"

-- Função: toList

toList :: List t -> [t]
toList Nil = []
toList (Constructor val next) = (val):(toList next)

-- Função: fromList

fromList :: [t] -> List t
fromList [] = Nil
fromList (val:next) = Constructor val (fromList next)

data Tree t = NilT | Node t (Tree t) (Tree t) deriving Show

-- Função: depth

depth :: Tree t -> Int
depth NilT = 0
depth (Node val left right) = 1 + max (depth left) (depth right)

-- Função: collapse

collapse :: Tree t -> [t]
collapse NilT = []
collapse (Node val left right) = (collapse left) ++ [val] ++ (collapse right)

-- Função: bfs (a completar)

-- Função: mapTree

mapTree :: (t -> u) -> Tree t -> Tree u
mapTree _ NilT = NilT
mapTree f (Node val left right) = (Node (f val) (mapTree f left) (mapTree f right))