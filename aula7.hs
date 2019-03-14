ehPar :: Int -> Bool
ehPar x = (mod x 2) == 0

ehParL :: [Int] -> [Bool]
ehParL [] = []
ehParL (x:xs) = ehPar x : ehParL xs

mapL :: (t->a) -> [t] -> [a]
mapL f [] = []
mapL f (x:xs) = f x : mapL f xs


--constroi uma lista em que "x" são valores da lista "l" dada como argumento, e para cada valor de "x" aplique a função "f" que também foi dada como argumento.
mapL_CL f l = [f x | x <- l]

-- Fold

--soma todos os elementos da lista 2 a 2, e soma o resultado com o proximo.
somaL :: [Int] -> Int
somaL [] = 0
somaL (x:xs) = (+) x (somaL xs)

--faz a disjunção (ou) de todos os elementos da lista sempre comparando 2 a 2, e comparando o resultado dessa comparação com o proximo.
disjL :: [Bool] -> Bool
disjL [] = False
disjL (x:xs) = (||) x (disjL xs)

mfold :: (t -> t -> t) -> [t] -> t
mfold f [x] = x
mfold f (x:xs) = f x (mfold f xs)

{-
mfold (+) [1,2,3]
= (+) 1 (mfold (+) [2,3])
= (+) 1 ((+) 2 (mfold (+) [3]))
= (+) 1 ((+) 2 3)
-}

mfold2 :: (t-> t -> t) -> t -> [t] -> t
mfold2 f v [] = v
mfold2 f v (x:xs) = f x (mfold2 f v xs)

--Filtro
parL :: [Int] -> Int
parL [] = []
parL (x:xs)
    | ehPar x = x : parL xs
    | otherwise = parL xs

maior10 :: Int -> Bool
maior10 x = x > 10

maior10L :: [Int] -> [Int]
maior10L [] = []
maior10L (x:xs)
    | maior10 x = x : maior10L xs
    | otherwise = maior10L xs

filtro :: (t -> Bool) -> [t] -> [t]
filtro f [] = []
filtro f (x:xs)
    | f x = x : filtro f xs
    | otherwise filtro xs

cAZ :: Char -> Bool
cAZ ch = (ch >= 'A') && (ch <= 'Z')

-- constroi uma lista com todos os valores de "x" que vem da lista "l", mas apenas os valores que atendem a condição "f"
filtroCL f l = [ x | x <- l, f x]

-- na expressao [ f x | x <- l] os valores da lista final serao alterados pela funçao f (todo "f" que é escrito a direita do pipe é uma expressao que altera o valor final de x)
--na expressao [x | x <- l, f x] os valores da lista final NAO serao alterados pela funçao "f" mas deverao atender essa condicao para estarem na lista final (todo "f" que é escrito a esquerda do pipe é uma expressao de condiçao para valor final de x)
