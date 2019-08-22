--Sinonimos para tipos

type Nome = String
type Idade = Int
type CPF = Int

type Pessoa = (Nome, Idade, CPF)

nome :: Pessoa -> String
nome(n,t,c) = n

--Listas

--todos os valores da lista tem que ser do mesmo tipo
-- por que [10..0] nao pega

somaLista :: [Int] -> Int
somaLista [] = 0 --caso base
somaLista (x:xs) = x + somaLista xs --caso recursivo

{-
somaLista [1,2,3]
= somaLista (1: (2: (3: [])))
= 1 + somaLista (2: (3: []))
= 1 + 2 + somaLista(3:[])
= 1 + 2 + 3 + somaLista []
= 1 + 2 + 3 + 0
= 6
-}

-- Exercícios

double :: [Int] -> [Int]
double [] = []
double (x:xs) = x * 2 : double xs

-- member sem usar guardas
{-
member2 :: [Int] -> Int -> Bool
member2 [] n = False
member2 (x:xs) n = (x==n) || member2 xs n
-}

member :: [Int] -> Int -> Bool
member [] n = False
member (x:xs) n 
                | x == n = True
                | otherwise = member xs n

isDigit :: Char -> Bool
isDigit ch = (ch >= '0') && (ch <= '9')

digits :: String -> String
digits [] = []
digits (x:xs) 
                | isDigit x = x: digits xs
                | otherwise = digits xs

                            
