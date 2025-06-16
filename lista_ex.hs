sublistas :: [a] -> [[a]]
sublistas [] = [[]]
sublistas (x:xs) =  sublistas xs ++ map (x:) (sublistas xs)

poli :: Integer -> Integer -> Integer -> (Integer -> Integer)
poli a b c = (\x -> a*x*x + b*x + c)

listaPoli :: [(Integer, Integer, Integer)] -> [Integer -> Integer]
listaPoli = map (\(a, b, c) -> poli a b c)

appListaPoli :: [Integer -> Integer] -> [Integer] -> [Integer]
appListaPoli [] _ = []
appListaPoli _ [] = []
appListaPoli (f:fs) (i:is) = (f i) : (appListaPoli fs is)

isMatrix :: [[a]] -> Bool
isMatrix [[]] = True
isMatrix [x] = True
isMatrix (x:y:xs) = if length x == length y then isMatrix (y:xs) else False

permutar :: Int -> Int -> [[a]]-> [[a]]
permutar x y matriz =
    let
        parteInicial = take x matriz
        linhaY = matriz !! y
        parteMeio = take (y - x - 1) (drop (x + 1) matriz)
        linhaX = matriz !! x
        parteFinal = drop (y+1) (matriz)
    in
        parteInicial ++ [linhaY] ++ parteMeio ++ [linhaX] ++ parteFinal


type Codigo = Int
data Voto = Presidente Codigo | Senador Codigo | Deputado Codigo | Branco
            deriving (Show)
type Urna = [Voto]
type Apuracao = [(Voto, Int)]

instance Eq Voto where
    Branco == Branco = True
    Presidente x == Presidente y = x == y
    Senador x == Senador y = x == y
    Deputado x == Deputado y = x == y
    _ == _ = False

totalVotos :: Urna -> Voto -> Int
totalVotos [] _ = 0
totalVotos (u:us) v = if u == v then (1 + totalVotos us v) else (totalVotos us v)

apurar :: Urna -> Apuracao
apurar [] = []
apurar (x:xs) =
    let
        contagem = 1 + totalVotos xs x
        urna = filter (/= x) xs
    in
        (x, contagem) : apurar urna

ehMatriz :: [[a]] -> Bool
ehMatriz [] = True
ehMatriz [x] = True
ehMatriz (x:y:xs) = if length x == length y then ehMatriz (y:xs) else False

permutar2 :: [[a]] -> Int -> Int -> [[a]]
permutar2 xs x y =
    let
        antes = take x xs
        posicaoY = xs !! y
        entre = drop (x+1) (take y xs)
        posicaoX = xs !! x
        depois = drop (y+1) xs
    in
        antes ++ [posicaoY] ++ entre ++ [posicaoX] ++ depois