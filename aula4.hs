--AULA04 28/02/2019--

fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n-1)

-- Listas

somaLista :: [Int] -> Int
somaLista [] = 0 --caso base
somaLista (x:xs) = x + somaLista xs --caso recursivo

{-
somaLista [1,2,3]
= 1 + somaLista [2,3]
= 1 + 2 + somaLista [3]
= 1 + 2 + 3 + somaLista []
= 1 + 2 + 3 + 0
= 6
-}

tamLista :: [Int] -> Int
tamLista [] = 0 --caso base
tamLista (x:xs) = 1 + tamLista xs --caso recursivo

--Função de concatenação de listas: (++) :: [a] -> [a] -> [a]

reverterLista :: [Int] ->  [Int]
reverterLista [] = [] --caso base
reverterLista (x:xs) = reverterLista xs ++ [x]

{-
reverterLista [2,3] ++ [1]
= (reverterLista [3] ++ [2]) ++ [1]
= ((reverterLista [] ++ [3]) ++ [2]) ++ [1]
= (((([]) ++ [3]) ++ [2]) ++ [1])
-}

repeticao :: Int -> Char -> String
repeticao 0 ch = [] -- caso base
repeticao n ch = [ch] ++ repeticao (n-1) ch --caso recursivo com concatenaçao de listas
-- repeticao n ch = ch : repeticao (n-1) ch -- segunda opçao para o caso recursivo com construçao de listas
-- para usar concatenaçao deve haver uma lista dos 2 lados da expressão, para usar construçao so precisa do lado direito

-- Retorna os primeiros n elementos da lista
mtake 0 _ = []
mtake n (x:xs) = x : mtake (n-1) xs

mdrop 0 lista = lista
mdrop _ [] = []
mdrop n (x:xs) = mdrop (n-1) xs

{-
mdrop 2 [1,2,3]
= mdrop 1 [2,3]
= mdrop 0 [3]
= [3]
-}

--Ordenação 

iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = insere x (iSort xs)

insere :: Int -> [Int] -> [Int]
insere x [] = [x]
insere x (y:ys)
    | x <= y = (x:y:ys)
    | otherwise = y : insere x ys
