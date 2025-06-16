data Estacao = Inverno | Verao | Outono | Primavera
            deriving (Show, Eq, Ord)
data Temp = Frio | Quente
            deriving (Show, Eq, Ord)
--data Bool = False | True
--            deriving (Show, Eq, Ord)

clima :: Estacao -> Temp
clima Inverno = Frio
clima _ = Quente

type Nome = String
type Idade = Integer
data Pessoas = Pessoa Nome Idade -- Pessoa :: Nome -> Idade -> Pessoas
            deriving (Show, Eq, Ord) -- tipo de produto

showPerson :: Pessoas -> String
showPerson (Pessoa n a) = n ++ " " ++ show a

data Shape = Circle Float | Rectangle Float Float -- podemos usar Circle 4.9 ou Rectangle 4.2 2.0, por exemplo
            deriving (Show, Eq, Ord)

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle a b) = a * b

data Expr = Lit Integer | Add Expr Expr | Sub Expr Expr
            deriving (Show, Eq, Ord)

--data Tree = Nilt | Node Integer Tree Tree

eval :: Expr -> Integer
eval (Lit a) = a
eval (Add a b) = (eval a) + (eval b)
eval (Sub a b) = (eval a) - (eval b)

size :: Expr -> Integer
size (Lit _) = 0
size (Add e1 e2) = 1 + size e1 + size e2
size (Sub e1 e2) = 1 + size e1 + size e2

{-sumTree :: Tree -> Integer
sumTree Nilt = 0
sumTree (Node x t1 t2) = x + sumTree t1 + sumTree t2

occurs :: Tree -> Integer -> Integer
occurs Nilt x = 0
occurs (Node x t1 t2) i = if i == x then 1 + occurs t1 i + occurs t2 i else occurs t1 i + occurs t2 i
-}
data Pairs t = Pair t t -- Pair 6 8 tem o tipo Pairs Int; Pair True True tem o tempo Pairs Bool

data List t = Cons t (List t) -- junta uma constante (head) com o resto da lista (tail)

data Tree t = Nil | Node t (Tree t) (Tree t)

depth :: Tree t -> Integer
depth Nil = 0
depth (Node n t1 t2) = 1 + max (depth t1) (depth t2)

occurs2 :: Eq t => Tree t -> t -> Bool
occurs2 Nil i = False
occurs2 (Node n t1 t2) i = if n == i then True else occurs2 t1 i || occurs2 t2 i

minMaxTree :: Tree Integer -> (Integer, Integer)
minMaxTree (Node n t1 t2) = (finalMin, finalMax)
  where
    (minL, maxL) = minMaxOfSubtree t1
    (minR, maxR) = minMaxOfSubtree t2

    finalMin = min n (min minL minR)
    finalMax = max n (max maxL maxR)

    minMaxOfSubtree :: Tree Integer -> (Integer, Integer)
    minMaxOfSubtree Nil = (n, n)
    minMaxOfSubtree tree = minMaxTree tree

collapse :: Tree t -> [t]
collapse Nil = []
collapse (Node n t1 t2) = [n] ++ collapse t1 ++ collapse t2

data Either a b = Left a | Right b
                deriving (Eq, Ord, Read, Show)

-- versão padrão
{-
tail :: [t] -> [t]
tail [] = error "Prelude.tail: empty list"
tail (_:xs) = xs
-}

-- versão com dummy value
tl :: [t] -> [t]
tl [] = []
tl (_:xs) = xs

divide :: Float -> Float -> Float
divide a b = if (b /= 0) then a/b else 0

hd :: t -> [t] -> t
hd t [] = t
hd t (x:_) = x

errDiv :: Float -> Float -> Maybe Float
errDiv a b = if (b /= 0) then Just (a/b) else Nothing

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe g Nothing = Nothing
mapMaybe g (Just x) = Just (g x)

maybe :: b -> (a -> b) -> Maybe a -> b
maybe n g Nothing = n
maybe n g (Just x) = g x

mHead :: [t] -> Maybe t
mHead [] = Nothing
mHead (x:_) = Just x

usandoMhead :: (Show t) => [t] -> String
usandoMhead l = case mHead l of
                    Nothing -> "Lista vazia"
                    Just x -> "Head: " ++ show x

perimetro :: Shape -> Float
perimetro (Circle r) = 2*pi*r
perimetro (Rectangle a b) = 2*(a+b)