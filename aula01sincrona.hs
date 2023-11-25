periodo :: Int
periodo = 7


-- tabela de vendas
vendas :: Int -> Int
vendas 0 = 0
vendas 1 = 41
vendas 2 = 72
vendas 3 = 48
vendas 4 = 0
vendas 5 = 91
vendas 6 = 55
vendas 7 = 30
vendas x = -1

-- função que retorna o total de vendas

totalvendas::Int->Int
totalvendas 0 = vendas 0
totalvendas x = vendas x + (totalvendas (x-1))

totalVendasPeriodo =  totalvendas periodo

-- função que retorna quantas vendas superam um valor

diasSuperaValor::Int->Int->Int
diasSuperaValor x 0 = 0
diasSuperaValor x d
  | x < vendas d = 1 + diasSuperaValor x (d-1)
  | otherwise    = 0 + diasSuperaValor x (d-1)

dSValor :: Int -> Int
dSValor x = diasSuperaValor x periodo

-- função auxiliar maior

maior::Int->Int->Int
maior a b
  | (a>b) = a
  | otherwise = b

-- retorna maior venda

maiorVenda :: Int-> Int
maiorVenda x
   | x==0 = vendas 0
   | otherwise = maior (vendas x) (maiorVenda(x-1))

maiorV :: Int
maiorV = maiorVenda periodo

-- retorna o dia de certa venda  

dvenda :: Int -> Int -> Int
dvenda _ (-1) = (-1)
dvenda x y
  | (vendas y) == x = y
  | otherwise = dvenda x (y-1)

dV :: Int -> Int
dV x = dvenda x periodo


diaMaiorVenda = dV maiorV










