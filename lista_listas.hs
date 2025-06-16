import Data.Char (toUpper)

{- 1. Defina a função paraMaiuscula :: String −> String que transforma toda letra minúscula em uma
string em maiúscula. Use compreensão de listas. Modifique esta função para se comportar da 
mesma forma, mas removendo todos os caracteres que não são letras da string resultante. -}
paraMaiuscula :: String -> String
paraMaiuscula x = [toUpper a | a <- x]

{-2. Defina a função divisores :: Integer −> [Integer] que retorna uma lista de divisores 
de um inteiro positivo dado como argumento e uma lista vazia para outras entradas. -}
divisores :: Integer -> [Integer]
divisores x
    | x > 0 = [a | a <- [1 .. x], x `mod` a == 0]
    | otherwise = []

{- 3. Dada uma lista de inteiros, defina a função menorLista que encontra o menor inteiro 
dessa lista -}
menorLista :: [Integer] -> Integer
menorLista [x] = x
menorLista (x:xs) = if x < menorLista (xs) then x else menorLista (xs)

{- 4. Defina função measure que, para uma lista vazia, retorna -1 e, para outras listas, retorna 
o tamanho da lista -}
measure :: [a] -> Integer
measure [] = -1
measure [x] = 1
measure (x:xs) = 1 + measure xs

{- 5. Defina a função takeFinal que retorna os últimos n elementos de uma lista dada 
como argumento -}
takeFinal :: Int -> [a] -> [a]
takeFinal n xs = if n <= 0 then [] else drop (length xs - n) xs

{- 6. Defina uma função que remove o enésimo elemento de uma lista, ou seja, retorna uma lista
que idêntica à lista recebida como argumento, com exceção de que o enésimo não consta na lista
de retorno. A indexação começa em zero. -}
remmove :: Int -> [a] -> [a]
remmove n xs | n < 0 = xs
remmove _ [] = []
remmove 0 (x:xs) = xs
remmove n (x:xs) = x : remmove (n-1) xs

{- 7.Dê uma definição, com casamento de padrão, de uma função que retorna o primeiro inteiro, se
houver, de uma lista de inteiros incrementado de um, ou retorna zero, do contrário. Apresente
também uma solução sem casamento de padrão, mas usando funções da biblioteca. -}
primeiroInteiro :: [Integer] -> Integer
primeiroInteiro [] = 0
primeiroInteiro (x:xs) = x+1

primeiroInteiro2 :: [Integer] -> Integer
primeiroInteiro2 xs =
    let
        primeiro = take 1 xs
    in
        case primeiro of
            [] -> 0
            [x] -> x+1

{- 8. Dê uma definição com casamento de padrão de uma função que faz a adição dos dois primeiros
elementos de uma lista, se a lista contém pelo menos dois elementos; retorna a cabeça da lista,
se houver um elemento apenas; retorna zero, do contrário. Apresente também uma solução sem
casamento de padrão, mas usando funções da biblioteca. -}
adic :: [Integer] -> Integer
adic [] = 0
adic [x] = x
adic (x:y:xs) = x + y

adic2 :: [Integer] -> Integer
adic2 xs = sum (take 2 xs)

{- 9. Defina a função produto :: [Integer] −> Integer que retorna o produto de uma lista
de inteiros e 1, no caso da lista vazia. -}
produto :: [Integer] -> Integer
produto [] = 1
produto (x:xs) = x * produto xs

{- 10. Defina a função unique :: [Integer] −> [Integer] que retorna os elementos que
ocorrem apenas uma vez na lista dada como argumento. -}
unique :: [Integer] -> [Integer]
unique xs = [x | x <- xs, count x xs == 1]
    where
        count e lst = length [i | i <- lst, i == e]

{- 11. Defina uma função que verifica se uma lista dada como argumento está em ordem crescente.
Use recursão e casamento de padrão. Não use funções de bibliotecas -}
crescente :: [Integer] -> Bool
crescente [] = True
crescente [x] = True
crescente (x:y:xs) = if crescente (y:xs) && x <= y then True else False