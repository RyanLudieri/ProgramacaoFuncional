answer :: Int
answer = 42

square :: Int -> Int
square x = x * x

--allEqual :: Int -> Int -> Int -> Bool
allEqual m n p = (m==n) && (n==p)

maxi :: Int -> Int -> Int
maxi m n
   |m >= n = m
   |otherwise	= n

f :: Int -> Int
f 0 = 3
f 1 = 9
f 2 = 7
f 3 = 15
f x = -1

totalf 0 = f 0
totalf x = f x + totalf (x - 1)      

totalff x
  | x == 0 = f x
  | otherwise = totalff (x-1) + f x
















