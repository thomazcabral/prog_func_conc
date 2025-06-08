import Data.Char (toUpper)
import Data.Char (isLetter)
import Data.List (delete)
import Data.List (isInfixOf)

{- 1. Defina a função length usando map e sum -}
lengthh :: [a] -> Integer
lengthh xs = sum (map (\_ -> 1) xs)

{- 2. Usando a função map e funções da biblioteca, defina as seguintes funções
(a) uppers :: String −> String que transforma um string para maiúsculas
(b) doubles :: [Int] −> [Int] que dobra cada elemento uma lista
(c) centavosReais :: [Int] −> [Float] converte preços em centavos para reais-}
func :: Integer -> Float
func x = fromIntegral x/100.0

double :: Integer -> Integer
double x = x*2

uppers :: String -> String
uppers x = map toUpper x

doubles :: [Integer] -> [Integer]
doubles x = map double x

centavosReais :: [Integer] -> [Float]
centavosReais x = map func x

{- 3. Usando a função filter e funções da biblioteca, defina as seguintes funções
(a) letras :: String −> String que remove todos os caracteres não alfabéticos do string
dado como argumento
(b) rmChar :: Char −> String −> String que remove do string todas as ocorrências do
caractere dado como primeiro argumento
(c) acima :: Int −> [Int] −> [Int] que remove da lista todos os números menores ou
iguais ao número dado como primeiro argumento
(d) desiguais :: [(Int, Int)] −> [(Int,Int)]que remove todos os pares (x,y) tais que
x == y-}
letras :: String -> String
letras x = filter isLetter x

rmChar :: Char -> String -> String
rmChar x s = filter (/= x) s

acima :: Integer -> [Integer] -> [Integer]
acima x l = filter (> x) l

desiguais :: [(Integer, Integer)] -> [(Integer, Integer)]
desiguais xs = filter (\(x,y)->x /= y) xs

{- 4. Compreensão de listas processam uma lista de forma similar a map e filter . De forma
geral, [ f x | x <− xs, p x] é equivalente a map f ( filter p xs). Escreva expressões
equivalentes a seguir, usando map e filter
(a) [toUpper c | c <− s, isAlpha c ]
(b) [2 ∗x | x <− xs, x > 3 ]
(c) [reverse s | s <− strs, even (length s) ] -}
aa :: String -> String
aa c = map toUpper (filter isLetter c)

bb :: [Integer] -> [Integer]
bb x = map (*2) (filter (> 3) x)

cc :: [String] -> [String]
cc x = map reverse (filter (even . length) x)

{- 5. Escreva funções usando recursão e também foldr
(a) A função recursiva productRec :: [Int] −> Int que computa o produto de todos os
elementos de uma lista. Escreva a equivalente productFold, usando a função foldr
(b) A função recursiva andRec :: [Bool] −> Bool que verifica se todos os elementos de
uma lista são verdadeiros. Escreva a equivalente andFold, usando foldr
(c) A função recursiva concatRec :: [String] −> String que concatena todas os ele-
mentos de uma lista de strings em um único string. Escreva a função equivalente
concatFold, usando foldr -}

productRec :: [Integer] -> Integer
productRec [x] = x
productRec (x:xs) = x * productRec xs

productFold :: [Integer] -> Integer
productFold x = foldr1 (*) x

andRec :: [Bool] -> Bool
andRec [x] = if x then True else False
andRec (x:xs) = if x then andRec xs else False

andFold :: [Bool] -> Bool
andFold x = foldr1 (&&) x

concatRec :: [String] -> String
concatRec [x] = x
concatRec (x:xs) = x ++ concatRec xs

concatFold :: [String] -> String
concatFold x = foldr1 (++) x