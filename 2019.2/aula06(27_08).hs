--funções polimorficas

tamLista :: [a] -> Int
tamLista [] = 0
tamLista (x:xs) = 1 + tamLista xs

--funçoes de alta ordem

soma3Int :: Int -> (Int -> (Int -> Int))
soma3Int x y z = x + y + z

--recebe uma função e um inteiro e devolve o valor depois de aplicar a funçao duas vezes a ele
aplicaDuasVezes :: (t -> t) -> t -> t
aplicaDuasVezes f n = f (f n)

inc :: Int -> Int
inc x  = x + 1

vendas 0 = 1
vendas 1 = 5
vendas 2 = 4


totalVendas :: Int -> Int
totalVendas 0 = vendas 0
totalVendas n = vendas n + totalVendas (n - 1)

total :: (Int -> Int) -> Int -> Int
total f 0 = f 0
total f n  = f n + total f (n-1)

totalVendas2 n = total vendas n

quadrado :: Int -> Int
quadrado n = n * n

--soma os quadrados dos valores de 0 a n

somaQuadrados n = total quadrado n

--

dobrarValoresLista :: [Int] -> [Int]
dobrarValoresLista [] = []
dobrarValoresLista (x:xs) = x * 2 : dobrarValoresLista xs

quadradoValoresLista :: [Int] -> [Int]
quadradoValoresLista [] = []
quadradoValoresLista (x:xs) = x * x : quadradoValoresLista xs

--serve para mapear os valores para cada elemento da lista sem precisar especificar no corpo da função o que a função passado como argumento faz
mapeamento :: (Int -> Int) -> [Int] -> [Int]
mapeamento f [] = []
mapeamento f (x:xs) = f x : mapeamento f xs

ehPar :: Int -> Bool
ehPar n  = mod n 2 == 0

mapEhPar :: [Int] -> [Bool]
mapEhPar [] = []
mapEhPar (x:xs) = ehPar x : mapEhPar xs

-- recebe uma funcao de a em b e uma lista de tipo 'a' e retorna uma lista de tipo 'b'

map1 :: (a -> b) -> [a] -> [b]
map1 f [] = []
map1 f (x:xs) = f x : map1 f xs

ehDigito :: Char -> Bool
ehDigito ch = ('0' <= ch) && (ch <= '9')

--mapeamento por compreensao de lista

mapCL f lista = [f x | x <- lista]

-- Filtro
-- no GHCi é filter

filtro :: (t -> Bool) -> [t] -> [t]
filtro f [] = []
filtro f (x:xs)  
                | f x == True = x : filtro f xs
                | otherwise = filtro f xs


--filtro por compreensao de lista

filtroCL f lista = [ x | x <- lista, f x] 

somaL :: [Int] -> Int
somaL [] = 0
somaL (x:xs) = x + somaL xs

produtoL :: [Int] -> Int
produtoL [] = 1
produtoL (x:xs) = x * produtoL xs

orL :: [Bool] -> Bool
orL [] = False
orL (x:xs) = x || (orL xs)

andL :: [Bool] -> Bool
andL [] = True
andL (x:xs) = x && (andL xs)

-- recebe uma função 'f', um valor 'v' que sera usado como default caso a lista esteja vazia, e uma lista e retorna 

mfoldr :: (t -> t -> t) -> t -> [t] -> t
mfoldr f v [] = v
mfoldr f v (x:xs) = f x (mfoldr f v xs)

{-
mfoldr (||) False [True, False]
= (||) True (mfoldr (||) False [False])
= (||) True ((||) False (mfoldr(||) False[]))
= (||) True ((||) False False)
-}

mfoldl :: (t -> t -> t) -> t -> [t] -> t
mfoldl f v [] = v
mfoldl f v (x:xs) = mfoldl (f (f v x) xs)

{-
mfoldl (||) False [True, False]
= mfoldl (||) ((||) False True) [False]
= mfoldl (||) True [False]
= mfoldl (||) ((||) True False) []
= mfoldl (||) True []
-}

