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
	| otherwise = [head heap] ++ heapsort (minHeapify (tail heap) 1)
		where heap = buildMinHeap list ((length list) `div` 2)