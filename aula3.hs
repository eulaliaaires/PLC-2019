--AULA03 26/02/2019--

terceiro :: (a, b, c) -> c
terceiro (x, y, z) = z

--Sinonimos de tipos

type ParInt = (Int, Int)

primeiroParInt :: ParInt -> Int
primeiroParInt p = fst p

type Nome = String
type Idade = Int
type Pessoa = (Nome, Idade)

nome :: Pessoa -> Nome
nome pessoa = fst pessoa

--Casamento de padrão

myNot :: Bool -> Bool
myNot False = True
myNot True = False

myOr1 :: Bool -> Bool -> Bool
myOr1 True True = True
myOr1 True False = True
myOr1 False True = True
myOr1 False False = Falses

myOr2 :: Bool -> Bool -> Bool
myOr2 True x = True
myOr2 False x = x

--Fatorial

fatorial :: Int -> Int
fatorial x
    | x == 0 = 1 --caso base
    | otherwise = x * fatorial (x-1) --caso recursivo


fatorialCasamentoPadrao :: Int -> Int
fatorialCasamentoPadrao 0 = 1
fatorialCasamentoPadrao n = n * fatorialCasamentoPadrao (n - 1)

--Definições locais

--where
--let ... in ...

somaQuadrados :: Int -> Int -> Int
somaQuadrados x y = sqX * sqY
    where sqX = x * x
          sqY = y * y

somaQuadrados2 :: Int -> Int -> Int
somaQuadrados2 x y = sq x + sq y
    where sq n = n * n

somaQuadrados3 :: Int -> Int -> Int
somaQuadrados3 x y =
    let
        sqX = x * x
        sqY = y * y
    in
        sqX + sqY
        
--head = retorna a cabeça da lista
--tail = retorna a cauda da lista
--tail de uma lista que tem um único elemento é uma lista vazia
--drop n = ignora os primeiros n elementos e devolve uma lista com os elementos restantes
--take = dado um inteiro n e uma lista devolve uma lista com os n primeiros elementos
--reverse = dada uma lista de valores de tipo devolve uma lista de tipo a invertida