dobrar :: Integer -> Integer
dobrar x = x*2

square :: Integer -> Integer
square x = x*x

ehPar :: Integer -> Bool
ehPar x = mod x 2 == 0

numeros :: [Integer]
numeros = [1,2,3,4,5]

numerosDobrados :: [Integer]
numerosDobrados = map dobrar numeros

numerosPares :: [Integer]
numerosPares = filter ehPar numeros

aplicarNVezes :: (a -> a) -> Integer -> a -> a
aplicarNVezes f 0 x = x
aplicarNVezes f n x = aplicarNVezes f (n-1) (f x)

decrementar :: Integer -> Integer
decrementar x = x - 1

resultado :: Integer
resultado = aplicarNVezes decrementar 3 10

vendas :: Integer -> Integer
vendas x = x*2

total :: (Integer -> Integer) -> Integer -> Integer
total f 0 = f 0
total f x = f x + total f(x-1)

double :: [Integer] -> [Integer]
double [] = []
double (a:as) = (2*a) : double as

squaree :: [Integer] -> [Integer]
squaree [] = []
squaree (a:as) = (a*a) : squaree as

mapp :: (t -> u) -> [t] -> [u]
mapp f [] = []
mapp f (a:as) = f a : mapp f as

mapp2 :: (t -> u) -> [t] -> [u]
mapp2 f l = [f a | a <- l]

ehDigito :: Char -> Bool
ehDigito x = (x >= '0') && (x <= '9')

filterr :: (t -> Bool) -> [t] -> [t]
filterr f [] = []
filterr f (a:as)
    | f a = a : filterr f as
    | otherwise = filterr f as

filterr2 :: (t -> Bool) -> [t] -> [t]
filterr2 f l = [a | a <- l, f a]

somaLista :: [Integer] -> Integer
somaLista [x] = x
somaLista (a:as) = a + somaLista as

foldr11 :: (t -> t -> t) -> [t] -> t
foldr11 f [x] = x
foldr11 f (a:as) = f a (foldr11 f as)

foldrr :: (a -> b -> b) -> b -> [a] -> b
foldrr f s [] = s
foldrr f s (a:as) = f a (foldrr f s as)

len :: [a] -> Integer
len x = sum (map (\_ -> 1) x)

seteA :: (Integer -> Integer) -> Integer -> Integer
seteA f 0 = f 0
seteA f n = min (f n) (seteA (f) (n-1))

increasing :: (Integer -> Integer) -> Integer -> Bool
increasing f 0 = True
increasing f n = if (f n) > (f (n-1)) then (increasing (f) (n-1)) else False

sumSquares :: [Integer] -> Integer
sumSquares xs = foldr (+) 0 (map square xs)

filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst f (x:xs) = if not (f x) then xs else filterFirst f xs

filterLast :: (a -> Bool) -> [a] -> [a]
filterLast f xs = reverse (filterFirst f (reverse xs))

switchMap :: (a -> a) -> (a -> a) -> [a] -> [a]
switchMap f g [] = []
switchMap f g [x] = [f x]
switchMap f g (x:y:xs) = (f x) : (g y) : (switchMap f g xs)

split :: [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([x], [])
split (x:y:xs) = (x:list1, y:list2)
    where
        (list1, list2) = split xs

merge :: ([a], [a]) -> [a]
merge (xs, []) = xs
merge ([], ys) = ys
merge (x:xs, y:ys) = x : y : (merge (xs, ys))

getWord :: String -> String
getWord [] = []
getWord (x:xs) = if x `elem` " " then [] else x : getWord xs

getUntil :: (a -> Bool) -> [a] -> [a]
getUntil f [] = []
getUntil f (x:xs) = if f x then [] else x : getUntil f xs