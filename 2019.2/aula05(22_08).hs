somaListaPares :: [(Int, Int)] -> [Int]
somaListaPares [] = []
somaListaPares (x:xs) = (fst x + snd x) : somaListaPares xs

-- Outra forma de fazer a mesma função

-- somaListaPares :: [(Int, Int)] -> [Int]
-- somaListaPares [] = []
-- somaListaPares ((a,b):xs) = (a + b) : somaListaPares xs

tamLista :: [Int] -> Int
tamLista [] = 0
tamLista (x:xs) = 1 + tamLista xs

reverterListaInt :: [Int] -> [Int]
reverterListaInt [] = []
reverterListaInt (x:xs) = reverterListaInt xs ++ [x]

-- reverterListaInt [1,2,3]
-- = reverterListaInt [2,3] ++ [1]
-- = reverterListaInt [3] ++ [2] ++ [1]
-- = reverterListaInt [] ++ [3] ++ [2] ++ [1]
-- = [] ++ [3] ++ [2] ++ [1]
-- = [3,2,1]

--Dado um caracter e uma quantidade de vezes 'n' gera uma lista com aquele caracter repetido n vezes

repeticao :: Int -> Char -> [Char]
repeticao 0 ch = []
repeticao n ch = ch : repeticao (n-1) ch

--retorna os 'n' primeiros elementos de uma lista dada

mtake :: Int -> [Int] -> [Int]
mtake _ [] = []
mtake 0 _ = []
matke n (x:xs) = x : mtake (n-1) xs

--retorna uma lista removendo os 'n' primeiros elementos da lista original

mdrop :: Int -> [Int] -> [Int]
mdrop 0 lista = lista  --qdo chegar em 0 ele retorna a lista que ele tiver ate o momento
mdrop _ [] = []
mdrop n (x:xs) = x : mdrop(n-1) xs

--ordenação

-- ordena a lista por inserção no lugar correto com a função 'ins'

iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

-- tenta inserir um numero na posição certa na lista

ins :: Int -> [Int] -> [Int]
ins n [] = [n]
ins n (y:ys)
    | n <= y = n : (y:ys)
    | otherwise = y : ins n ys -- caso o numero n seja menor que a cabeça da lista ele deixa o numero da cabeça e tenta inserir no restante da lista (a cauda ys)

-- iSort [2,1,3]
-- = ins 2 (iSort [1,3])
-- = ins 2 (ins 1 (iSort [3]))
-- = ins 2 (ins 1 (ins 3 (iSort [])))
-- = ins 2 (ins 1 (ins 3 []))
-- = ins 2 ins 1 [3]
-- = ins 2 [1,3]
-- = [1,2,3]

--retorna a cabeça da lista

head1 :: [a] -> a
head1 [] = error "Lista vazia"
head1 (x:_) = x

--retorna a cabeça da lista usando expressao 'case of'
--case of avalia algo e as opçoes de avaliação vem no 'of'
--o retorno do case of n usa igualdade usa-se '->'

head2 :: [a] -> a
head2 lista = case lista of
    [] -> error "Lista vazia"
    (x:_) -> x  -- n importa qual a cauda da lista pq queremos apenas a cabeça da lista

comoEhALista :: [a] -> String
comoEhALista lista = "Uma lista " ++ case lista of
                        [] -> "vazia"
                        [x] -> "com um elemento"
                        (x:xs) -> "com mais de um elemento"


zip2 :: [t] -> [u] ->[(t,u)]
zip2 (x:xs) (y:ys) = (x,y) : zip2 xs ys
zip2 _ _ = [] --erro de identação

-- zip2 [1,2] ['a','b','c']
-- = (1,'a') : zip2 [2] ['b','c']
-- = (1,'a') : (2,'b') zip2 [] ['c']

-- Compressao de lista

--tamanho da lista por compressao de listas

let tamL = sum [1 | _ <- lista]

-- forma tuplas (a,b,c) com os valores de 1 a 3
-- na primeira passada 'a' fica fixo, 'b' varia mais lentamente e 'c' varia mais rapido
-- ex: (1,1,1),(1,1,2),(1,1,3),(1,2,1),(1,2,2),(1,2,3),(1,3,1),(1,3,2),(1,3,3),(2,1,1),(2,1,2),(2,1,3),(2,2,1),(2,2,2),(2,2,3),(2,3,1),(2,3,2),(2,3,3),(3,1,1),(3,1,2),(3,1,3),(3,2,1),(3,2,2),(3,2,3),(3,3,1),(3,3,2),(3,3,3)]

-- [(a,b,c) | a <- [1..3], b <- [1..3], c <- [1..3]]

