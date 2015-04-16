-- ## Trabalho 8 (16/04/2015) ##

-- Questão 1

merge :: (Ord a) => [a] -> [a] -> [a]
merge left right
	| left == [] = right
	| right == [] = left
	| head left < head right = (head left) : merge (tail left) (right)
	| otherwise = (head right) : merge (left) (tail right)
	
mergeSort :: (Ord a) => [a] -> [a]
mergeSort items
	| length items < 2 = items
	| otherwise = merge (mergeSort left) (mergeSort right)
		where 
			left = take ((length items) `div` 2) items
			right = drop ((length items) `div` 2) items


listPartitioner :: (Num a, Ord a) => [a] -> ([a] -> [[a]])
listPartitioner l = f
	where
		f x 
			| length x < length l = error "List l' must have length m >= n, where n is the length of the list l."
			| l == [] = [newInput]
			| otherwise =  if (headList == []) then (listPartitioner (tail sortedList)) newInput else headList:((listPartitioner (tail sortedList)) newInput)
				where
					sortedInput = mergeSort x
					sortedList = mergeSort l
					headList = if (length l > 0) then mergeSort ([min | min <- x, min <= head sortedList]) else []
					newInput = drop (length headList) sortedInput