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

doubleAll :: [Integer] -> [Integer]
doubleAll numbers = [x*2 | x <- numbers]

divisors :: Integer -> [Integer]
divisors number = [x | x <- [1 .. number], number `mod` x == 0]

isPrime :: Integer -> Bool
isPrime x = if divisors x == [1, x] then True else False

matches :: Integer -> [Integer] -> [Integer]
matches number list = [x | x <- list, x == number]

duplicate :: String -> Integer -> String
duplicate s 0 = ""
duplicate s 1 = s
duplicate s i = s ++ duplicate (s) (i-1)

sublistas :: [a] -> [[a]]
sublistas [] = [[]]
sublistas (x:xs) = sublistas xs ++ map (x:) (sublistas xs)

ins :: Integer -> [Integer] -> [Integer]
ins i [] = [i]
ins i (x:xs) = if i < x then i:(x:xs) else x:(ins i xs)

iSort :: [Integer] -> [Integer]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

unique :: [Integer] -> [Integer]
unique xs = [x | x <- xs, count x xs == 1]
    where
        count e list = length [i | i <- list, i == e]

revverse :: [Integer] -> [Integer]
revverse [] = []
revverse xs = [(last xs)] ++ (revverse (init xs))

isSorted :: [Integer] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:xs) = if x <= (head xs) then isSorted (xs) else False

dropp :: Int -> [a] -> [a]
dropp x [] = []
dropp 0 lst = lst
dropp x (y:ys) = dropp (x-1) (ys)

zipp3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zipp3 [] _ _ = []
zipp3 _ [] _ = []
zipp3 _ _ [] = []
zipp3 (x:xs) (y:ys) (z:zs) = (x, y, z) : (zipp3 xs ys zs)
