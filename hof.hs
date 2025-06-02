dobrar :: Integer -> Integer
dobrar x = x*2

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

ehDigito :: Char -> Bool
ehDigito x = (x >= '0') && (x <= '9')

filterr :: (t -> Bool) -> [t] -> [t]
filterr f [] = []
filterr f (a:as)
    | f a = a : filterr f as
    | otherwise = filterr f as

somaLista :: [Integer] -> Integer
somaLista [x] = x
somaLista (a:as) = a + somaLista as

foldr11 :: (t -> t -> t) -> [t] -> t
foldr11 f [x] = x
foldr11 f (a:as) = f a (foldr11 f as)

foldrr :: (a -> b -> b) -> b -> [a] -> b
foldrr f s [] = s
foldrr f s (a:as) = f a (foldrr f s as)