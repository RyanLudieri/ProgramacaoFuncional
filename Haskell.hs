func1 :: [Int] -> [Int] -> [(Int,Int)]
func1 x y = [ (a,b) | a<-x, b<-y, a == b]



aux2 (a,[]) = 0
aux2 (a, (b:c))
	|a == b = 1 + (aux2 (a,c))
	|otherwise = aux2 (a,c) 


func2 l = [ (x,aux2(x,y)) |(x,y) <- l ]

eimpar:: [Int] -> [Int]
eimpar l = [x | x <- l, (mod) x 2 == 1]

epar:: [Int] -> [Int]
epar l = [x | x <- l, (mod) x 2 == 0]

minimo :: [Int] -> Int
minimo [x] = x
minimo (a:b)
	|a <= minimo b = a
	|otherwise = minimo b
	
remove :: Int -> [Int] -> [Int]
remove x l = [y| y<-l , y /= x]

ordena :: [Int] -> [Int]
ordena [] = []
ordena x = minimo x : ordena (remove (minimo x) x)

geraParImpar :: [Int] -> ([Int],[Int])
geraParImpar l = (ordena (epar l), ordena (eimpar l))


aux4 :: Char -> String -> Int -> [Int]
aux4 _ [] _ = []
aux4 c (a:b) x
	|c == a = x: (aux4 c b (x+1))
	|otherwise = aux4 c b (x+1)

func4 :: Char -> String -> (Char,[Int])
func4 c s = (c, (aux4 c s 1))




factors l = [x | x <- [1..l-1], mod l x == 0]

ex4 l = [x|x <-[1..l], sum (factors x) == x]


find i lt = [y| (x,y)<-lt , x == i]

scalarproduct::[Int] -> [Int] -> Int
scalarproduct x y = sum [a*b | (a,b)<-(zip x y)]

(&!) x1 x2 = (mult [x1 | _ <- [1.. (x2-1)]])
multi l = sum [x * x| x <-l]



