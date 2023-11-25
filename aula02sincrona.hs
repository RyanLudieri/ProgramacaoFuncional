import Data.Char

-- função que insere um elemento em uma lista ordenada
insereO a [] = [a]
insereO a (b:x) 
   | a <=b = a:b:x
   | otherwise = b:insereO a x

insereDO (a,s) [] = [(a,s)]
insereDO (a,s) ((b,z):x)
   | a <=b = (a,s):(b,z):x
   | otherwise = (b,z):insereDO (a,s) x
    

-- função que ordena uma lista de inteiros
ordenaL [] = []
ordenaL (a:x) = insereO a (ordenaL x)

ordenaLD [] = []
ordenaLD (a:x) = insereDO a (ordenaLD x)

-- função que ordena uma lista de duplas (Int, String)


-- Dada uma lista [(Bool, String)], fazer uma função que exclua os caracteres numéricos das Strings se o primeiro elemento for True ou que exclua as consoantes da String se Bool for False

filtraCon::String->String
filtraCon _ = "filtreiCon"

filtraNun::String->String
filtraNun _ = "filtereiNum"

filtraLista::[(Bool, String)]->[(Bool, String)]
filtraLista  [] = []
filtraLista (a:b) 
  |










