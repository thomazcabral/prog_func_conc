ehPar :: Integer -> Bool
ehPar x = mod x 2 == 0

ex = [2,4,7]

dobraLista xs = [x*2 | x <- ex]
dobraListaPar xs = [x*2 | x <- ex, ehPar x]

adicionaPar :: [(Integer, Integer)] -> [Integer]
adicionaPar x = [a+b | (a, b) <- x]

adicionaParOrd :: [(Integer, Integer)] -> [Integer]
adicionaParOrd x = [a+b | (a, b) <- x, a < b]

todosPares xs = (xs == [x | x <- xs, ehPar x])
todosImpares xs = ([] == [x | x <- xs, ehPar x])

headd :: [a] -> a
headd (x:_) = x

taill :: [t] -> [t]
taill (_:x) = x

nulll :: [y] -> Bool
nulll [] = True
nulll (_:_) = False

somaLista :: [Integer] -> Integer
somaLista [] = 0
somaLista (a:as) = a + somaLista as

lengthh :: [a] -> Integer
lengthh [] = 0
lengthh (a:as) = 1 + lengthh as

concatt :: [a] -> [a] -> [a]
concatt [] y = y
concatt (a:as) y = a : concatt as y

elemm x [] = False
elemm x (a:as)
    | x == a = True
    | otherwise = elemm x as

zipp :: [a] -> [b] -> [(a,b)]
zipp (x:xs) (y:ys) = (x,y) : zipp xs ys
zipp _ _ = []

qsort :: [Integer] -> [Integer]
qsort [] = []
qsort (a:as) = menorA ++ [a] ++ maiorA
    where
        menorA = qsort [b | b <- as, b <= a]
        maiorA = qsort [b | b <- as, b > a]

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
 | x <= y = x : merge xs (y:ys)
 | otherwise = y : merge (x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
    let
        metade = length xs `div` 2
        metade1 = take metade xs
        metade2 = drop metade xs
    in
        merge (mergeSort metade1) (mergeSort metade2)