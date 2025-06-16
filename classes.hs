data Shape = Circle Float | Rectangle Float Float
            deriving(Eq, Show, Ord)

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle a b) = a * b

class Info a where
    examples :: [a]
    size :: a -> Integer

instance Eq Bool where -- classe Eq para o tipo Bool
    True == True = True
    False == False = True
    _ == _ = False

instance Info Bool where
    examples = [True, False]
    size _ = 1

instance Info Integer where
    examples = [-100 .. 100]
    size _ = 1

instance Info Float where
    examples = [0.3, 0.4, 0.5]
    size _ = 1

instance Info Shape where
    examples = [Circle 3.0, Rectangle 32.6 23.7]
    size = round . area

comparee :: (Ord a, Info a) => a -> a -> Bool
comparee a b = size a <= size b

class Visible t where
    toString :: t -> String
    sizee :: t -> Integer

instance Visible Char where
    toString ch = [ch]
    sizee _ = 1

instance Visible Bool where
    toString True = "True"
    toString False = "False"
    sizee _ = 1

instance Visible t => Visible [t] where
    toString = concat . (map toString)
    sizee = (foldr (+) 0) . (map size)

instance Info a => Info [a] where
    examples = [[]] ++ [[x] | x <- examples] ++ [[x, y] | x <- examples, y <- examples]
    size = foldr (+) 1 . map size

class Eq t where
    (==), (/=) :: t -> t -> Bool
    a /= b = not (a == b)
    a == b = not (a /= b)

class Eq t => Ord t where
    (<), (<=), (>=), (>) :: t -> t -> Bool
    a < b = not (a >= b)
    a <= b = not (a > b)
    a >= b = not (a < b)
    a > b = not (a <= b)
    max, min :: t -> t -> t
    max a b = if a >= b then a else b
    min a b = if max a b == a then b else a

instance (Ord a, Ord b) => Ord (a, b) where
    (a1, b1) < (a2, b2) = a1 < a2 || (a1 == a2 && b1 < b2)
    (a1, b1) <= (a2, b2) = a1 < a2 || (a1 == a2 && b1 <= b2)
    max (a1, b1) (a2, b2) = if a1 > a2 then (a1, b1) else if a1 < a2 then (a2, b2) else if b1 > b2 then (a1, b1) else (a2, b2) 
    

vSort :: (Ord t, Visible t) => [t] -> String
vSort = toString . iSort

instance (Eq t, Eq u) => Eq (t, u) where
    (a, b) == (c, d) = (a == c && b == d)

class (Ord t, Visible t) => OrdVis t
-- vSort :: Ordvis t => [t] -> String

-- show :: Show t => t -> String
-- read :: Read t => String -> t

numEqual :: Eq a => [a] -> a -> Integer
numEqual [] i = 0
numEqual (x:xs) i = if x == i then 1 + numEqual xs i else numEqual xs i

showBoolFun :: (Bool -> Bool) -> String
showBoolFun f = "True -> " ++ show(f True) ++ "; False -> " ++ show(f False)

f :: Bool -> Int
f True = 42
f False = 0

showInt :: Int -> String
showInt x = "Resultado: " ++ show x

showBoolFunGen :: (a -> String) -> (Bool -> a) -> String
showBoolFunGen s f = "True -> " ++ s(f True) ++ "; False -> " ++ s(f False)