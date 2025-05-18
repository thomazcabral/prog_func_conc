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

vendas :: Int -> Int -- vendas de uma semana específica
vendas p = p*2

totalVendas :: Int -> Int -- total de vendas até uma semana específica
totalVendas 0 = 0
totalVendas s = vendas s + totalVendas (s-1)