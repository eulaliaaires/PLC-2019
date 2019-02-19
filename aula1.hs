--AULA01 19/02/2019--

constante :: Int
constante = 30

maior :: Bool
maior = constante > 40

funcaoComUmArgumento :: Int -> Bool
funcaoComUmArgumento x = x > 50

quadrado :: Int -> Int
quadrado x = x * x

todosIguais :: Int -> Int -> Int -> Bool
todosIguais x y z = (x == y) && (y == z)

--Uma função com guardas
maxi :: Int -> Int -> Int
maxi x y 
    | x >= y = x
    | otherwise = y
    

{-"div" = retorna resultado da divisão so com a parte inteira ex: 3 div 2 = 1.5

 "/" = retorna resultado da divisão so com a parte inteira ex: 3 / 2 = 1

 ":t" = retorna o tipo da expressão ex: :t True = Bool

 "nome da função :: tipo da função" = define qual vai ser o tipo de nome da função

 ":q" = sai do interpretador

 ":r" = recarrega o codigo

 ":?" = abre o helper

 ":" =  abre os comando do ghci

 ":max" = retorna a função max do ghci

-}


