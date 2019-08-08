-- Funçao definida com guardas

maxi :: Int -> Int -> Int
maxi x y 
  | x >= y = x
  | otherwise = y

-- O argumento da função indica qual é a semana

vendas 0 = 5
vendas 1 = 6
vendas 2 = 7
vendas 3 = 8
vendas 4 = 2

-- -- O que foi vendido no total é igual ao que foi vendido na semana passada como argumento mais o que foi vendido nas n-1 semanas

totalVendas :: Int -> Int
totalVendas n 
  | n == 0 = vendas 0
  | otherwise = vendas n + totalVendas (n - 1)

{-
ex:
  totalVendas 2
  = vendas 2 + totalVendas 2-1
  = vendas 2 + vendas 1 + totalVendas 1-1
  = vendas 2 + vendas 1 + vendas 0
-}

-- Retorna o maior numero de itens vendidos ao decorrer de n semanas


maxVendas :: Int -> Int
maxVendas n
  | n == 0 = vendas 0
  | otherwise = maxi (vendas n) (maxVendas (n-1)) 

-- {-
-- ex:
--   maxVendas 2
--   = maxi(vendas 2) (maxVendas(vendas 1))
--   = maxi(vendas 2) maxi(vendas 1) (maxVendas (vendas 0))
--   = maxi(vendas 2) maxi(vendas 1) (vendas 0)
--   = maxi(vendas 2) (vendas 1) 
-- -}

totalVendasCasamentoPadrao :: Int -> Int
totalVendasCasamentoPadrao 0 = vendas 0
totalVendasCasamentoPadrao n = vendas n + totalVendas (n-1)


maxVendasCasamentoPadrao :: Int -> Int
maxVendasCasamentoPadrao 0 = vendas 0
maxVendasCasamentoPadrao n = maxi (vendas n) (maxVendas (n - 1))

myNot :: Bool -> Bool
myNot True = False
myNot False = True

myOr :: Bool -> Bool -> Bool
myOr True True = True
myOr True False = False
myOr False True = True
myOr False False = False

myOr2 :: Bool -> Bool -> Bool
myOr2 True _ = True
myOr2 False _ = False

myAnd :: Bool -> Bool -> Bool
myAnd True x = x
myAnd False x = False 

-- Exercicios

fat :: Int -> Int
fat n
  | n == 0 = 1
  | otherwise = n * fat (n-1) 


all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal x y z w 
  | (x==y) && (y==z) && (z==w) = True
  | otherwise = False

-- equalCount :: Int -> Int -> Int -> Int
-- equalCount x y z w
--   | (x/=y) && (y/=z) && (z/=w) = 0
--   |