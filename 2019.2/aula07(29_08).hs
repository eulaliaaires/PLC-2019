--Composição de funções

inc :: Int -> Int
inc n =  n + 1

aplicaDuasVezes :: (t -> t) -> t -> t
-- aplicaDuasVezes f x = f (f x)
aplicaDuasVezes f x = (f . f) x -- com composiçao de funçoes

ehPar :: Int -> Bool
ehPar n = mod n 2 == 0

{-
aplicadDuasVezes inc 3
= (inc . inc ) 3 --aplicacao da funcao aplicaDuasVezes
= inc (inc 3)    --definicao de composicao
= inc 4
= 5
-}

--recebe um 'n' que é a quantidade de vezes que a funcao vai ser composta, e uma funçao, e retorna uma funcao que á funcao dada composta com ela mesma 'n' vezes.
iter :: Int -> (t -> t) -> (t -> t)
iter 0 f = id
iter n f = (iter (n-1) f) .f

{-
iter 3 inc
= (iter 2 inc) . inc
= ((iter 1 inc ) . inc) . inc
= (((iter 0 inc ) . inc) . inc) . inc
= (((id . inc) . inc) .inc )
-}

--Notaçao lambda

--uma funcao que aguarda 2 argumentos 'x' e 'y', soma eles e incrementa com 1, ex: h 2 3
-- let h = (\x y -> x + y + 1 )  --nao funciona

addNum :: Int -> (Int -> Int)
addNum n  = h
    where
        h x = n + x

addNumLambda :: Int -> (Int -> Int)
addNumLambda n = (\x -> n + x)

-- para chamar a função passa o nome da função e o x entre parenteses e qualuer argumento fora
mult2Int :: Int -> Int -> Int
mult2Int x y = x * y
--ex: (mult2Int 2) 5 = 10

--Aplicação parcial
 --ex: ("sjd" ++) "dhfb" = "sjddhfb" --concatena no começo
 -- (++ "sjd") "dhfb" = "dhfbsjd" --concatena no fim

-- multiplica todos os elementos da lista por 2 e retorna apenas todos os elementos pares, nessa ordem
-- (filter (\x -> mod x 2 == 0) . map (*2)) [1..7]
