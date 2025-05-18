{- 1. Definir a função dobro de tipo Integer−>Integer. A função deve receber um argumento
e devolvê-lo multiplicado por dois. -}
dobro :: Integer -> Integer
dobro x = x*2


{-2. Definir a função quadruplo que utiliza a função dobro do exercício anterior para devolver
o seu argumento multiplicado por quatro-}
quadruplo :: Integer -> Integer
quadruplo x = dobro x * 2

{-3. Definir a função poli2. Devem ser necessários quatro argumentos do tipo Double ( a,
b, c, e x) e devolver a ∗ x2 + b ∗ x + c. Definir a assinatura do tipo poli2 , ou seja,
poli2 :: alguma coisa-}
poli2 :: Double -> Double -> Double -> Double -> Double
poli2 a b c x = a * x^2 + b * x + c


{-4. Definir a função parImpar que devolve "par" (string) para entradas pares e "impar"
(string) para entradas ímpares. Defina uma função para determinar se um número é
par.-}
parImpar :: Integer -> String
parImpar x = if x `mod` 2 == 0 then "Par" else "Impar"


{-5. Defina a função maxFour :: Integer −> Integer −> Integer −> Integer −> Integer que
retorna o máximo de quatro inteiros Dê três definições dessa função: a primeira descrita
com base em maxThree; a segunda deve usar a função max e a terceira deve usar as
funções max e maxThree.-}

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree a b c
    | a >= b && a >= c = a
    | b >= c = b
    | otherwise = c

-- Primeira definição
maxFour :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour a b c d
    | x >= d = x
    | otherwise = d
    where x = maxThree a b c

-- Segunda definição
{-maxFour :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour a b c d
    | max a b == a && max a c == a && max a d == a = a
    | max b c == b && max b d == b = b
    | max c d == c = c
    | otherwise = d

-- Terceira definição
maxFour :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour a b c d = max x d where x = maxThree a b c-}


{-6. Defina a função quantosIguais :: Integer −> Integer −> Integer −> Integer que re-
torna quantos dos três argumentos são iguais. Por exemplo,
quantosIguais 56 32 12 = 0
quantosIguais 12 12 43 = 2-}
quantosIguais :: Integer -> Integer -> Integer -> Integer
quantosIguais a b c
    | a == b && a == c = 3
    | a == b || a == c || b == c = 2
    | otherwise = 0


{-7. Usando casamento de padrão, definir a função ehZero que retorna verdadeiro se for
dado como argumento um inteiro que seja 0, e falso, caso contrário. Definir o tipo da
função ehZero-}
ehZero :: Integer -> Bool
ehZero 0 = True
ehZero x = False

{-8. Usando recursão, implemente a função sumTo, de modo que sumTo n calcula o valor de
1 + 2 + ...+ n-}
sumTo :: Integer -> Integer
sumTo 1 = 1
sumTo n = n + sumTo (n-1)


{-9. Defina a função potencia, de modo que potencia n k calcula n elevado a k. Use recursão.-}
potencia :: Integer -> Integer -> Integer
potencia n 1 = n
potencia n k = n * potencia (n) (k-1)


{-10. Usando recursão, compute os coeficientes binomiais dados pelas seguintes equações
B(n, k) = B(n − 1, k) + B(n − 1, k − 1)
B(n, 0) = 1
B(0, k) = 0, quando k > 0
Dica: usar casamento de padrão pode ser de grande ajuda.-}
cobi :: Integer -> Integer -> Integer
cobi n 0 = 1
cobi 0 k = 0
cobi n k = cobi (n-1) (k) + cobi (n-1) (k-1)


{-11. Os números de Tribonacci são dados pelas seguintes equações
T(1) = 1
T(2) = 1
T(3) = 2
T(n + 1) = T(n) + T(n − 1) + T(n − 2)
Implemente uma função recursiva eficiente que calcula T n. Considere o uso de uma
função auxiliar.-}
tribonacci :: Integer -> Integer
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci 3 = 2
tribonacci n = tribonacci (n-1) + tribonacci (n-2) + tribonacci (n-3)


{-12. Defina a função addEspacos que produz um string com uma quantidade n de espaços.
addEspacos :: Int −> String-}
addEspacos :: Int -> String
addEspacos 1 = " "
addEspacos n = " " ++ addEspacos (n-1)


{-13. Defina a função paraDireita utilizando a definição de addEspacos para adiciconar uma
quantidade n de espaços à esquerda de um dado String, movendo o mesmo para a direita.
paraDireita :: Int −> String −> String-}
paraDireita :: Int -> String -> String
paraDireita a b = addEspacos a ++ b


{-14. Escreva uma função para retornar, em forma de tabela, todas as vendas da semana 0 até
a semana n, incluindo o total e a média de vendas no período. Usem as funções definidas
previamente e defina novas funções que achar necessário.
Semana Venda
0 12
1 14
2 15
Total 41
Média 13.6667
imprimeTabela : : Int −> String
imprimeTabela n = ca b e cal ho

++ imprimeSemanas n
++ imprimeTotal n
++ imprimeMedia n-}