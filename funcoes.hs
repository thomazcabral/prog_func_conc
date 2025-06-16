tudoIgual :: Int -> Int -> Int -> Bool
tudoIgual a b c = (a == b) && (a == c)

--somaDobroQuadrado :: Int -> Int -> Int
--somaDobroQuadrado a b = sdqa + sdqb
--    where
--        sdqa = 2 * (a * a)
--        sdqb = 2 * (b * b)

--somaDobroQuadrado :: Int -> Int -> Int
--somaDobroQuadrado a b = sdq a + sdq b
--    where
--        sdq y = 2 * (y * y)


somaDobroQuadrado :: Int -> Int -> Int
somaDobroQuadrado a b = let sdqa = 2 * (a * a)
                            sdqb = 2 * (b * b)
                        in sdqa + sdqb

fat :: Int -> Int
fat 0 = 1
fat n = n * fat (n-1)

vendas :: Integer -> Integer -- vendas de uma semana específica
vendas p = p*2

totalVendas :: Integer -> Integer -- total de vendas até uma semana específica
totalVendas 0 = 1
totalVendas s = vendas s + totalVendas (s-1)

maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer, Integer)
maxThreeOccurs a b c 
    | max a (max a c) == a && a == b && a == c = (a, 3)
    | max a (max a c) == a && a == b || a == c = (a, 2)
    | max a (max a c) == a = (a, 1)
    | max a (max a c) == b && b == a && b == c = (b, 3)
    | max a (max a c) == b && b == a || b == c = (b, 2)
    | max a (max a c) == b = (b, 1)
    | max a (max a c) == c && c == b && a == c = (c, 3)
    | max a (max a c) == c && c == b || a == c = (c, 2)
    | otherwise = (c, 1)

rangeProduct :: Integer -> Integer -> Integer
rangeProduct n m
    | n < m = 0
    | n == m = n
    | otherwise = m * rangeProduct (n) (m+1)

mult :: Integer -> Integer -> Integer
mult a 0 = 0
mult a 1 = a
mult a b = a + mult (a) (b-1)

func :: (Integer -> Integer) -> Integer -> Bool
func f 0 = if f 0 == 0 then True else False
func f i = if f i == 0 then True else func (f) (i-1)