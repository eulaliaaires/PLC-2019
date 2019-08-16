--Definições locais

quadrado :: Int -> Int
quadrado n = n * n

somaQuadrados :: Int -> Int -> Int
somaQuadrados x y  = quadrado x  + quadrado y

sumSquares :: Int -> Int -> Int
sumSquares x y = quad x + quad y
    where 
        quad n = n * n

sumSquares3 :: Int -> Int -> Int
sumSquares3 x y = let quadX = x * x 
                      quadY = y * y
                  in  quadX + quadY

-- Retorna se ao longo de 'n' semanas houve alguma venda igual a 0

vendas 0 = 2
vendas 1 = 4
vendas 2 = 5
vendas 3 = 7

vendasNulas :: Int -> Bool
vendasNulas 0 = vendas 0 == 0
vendasNulas n = (vendas n == 0) || vendasNulas (n -1)

-- Para usar ord e char primeiro tem que importar Data.Char

-- ord a, sendo 'a' um caracter ele retorna o valor do caracter na tabela ASC, ex: ord 'a' => 65
-- char 32,  sendo '32' um numero qlqr char retorna o respectivo caracter na tabela ASC
-- char (ord 'B' + (ord 'a' - ord 'A')), essa função transforma maiusculas em minusculas sendo 'a' e 'b' letras quaisquer

--Retorna a diferença dos valores maiusculo e minusculo do caracter na tabela ASC

offset :: Int
offset = fromEnum 'A' - fromEnum 'a'

toUpperCase :: Char -> Char
toUpperCase ch = toEnum (fromEnum ch + offset)

isDigit :: Char -> Bool
isDigit ch = (ch >= '0') && (ch <= '9')

-- A função show retona uma string, ex: show 3 => "3"
-- A função read faz o inverso de show ou seja transforma de string para o tipo que voce passar, ex: read "22" :: Int => 22

addEspacos :: Int -> String
addEspacos 1 = " "
addEspacos n = " " ++ addEspacos(n-1)

-- Retorna a string dada como entrada movida 'n' espaços para direita

paraDireita :: Int -> String -> String
paraDireita n str = addEspacos n ++ str

addPairCasamentoPadrao :: (Int,Int) -> Int
addPairCasamentoPadrao (x,y) = x + y

addPair :: (Int,Int) -> Int
addPair p = fst p + snd p

primeiro :: (a,b) -> a
primeiro (x,y) = x

primeiroInt :: (Int,Int) -> Int
primeiroInt (x,y) = x

-- Calculando equação de segundo grau exemplo no slide


