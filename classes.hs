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

instance Info Shape where
    examples = [Circle 3.0, Rectangle 32.6 23.7]
    size = round . area

class Visible t where
    toString :: t -> String
    size :: t -> Integer

instance Visible Char where
    toString ch = [ch]
    size _ = 1

instance Visible Bool where
    toString True = "True"
    toString False = "False"
    size _ = 1

instance Visible t => Visible [t] where
    toString = concat . (map toString)
    size = (foldr (+) 0) . (map size)

instance Info a => Info [a] where
    examples = [[]] ++ [[x] | x <- examples] ++ [[x, y] | x <- examples, y <- examples]
    size = foldr (+) 1 . map size

class Eq t where
    (==), (/=) :: t -> t -> Bool
    a /= b = not (a == b)
    a == b = not (a /= b)

class Eq t => Ord t where
    (<), (<=), (>=), (>) :: t -> t -> Bool
    max, min :: t -> t -> t

vSort :: (Ord t, Visible t) => [t] -> String
vSort = toString . iSort

instance (Eq t, Eq u) => Eq (t, u) where
    (a, b) == (c, d) = (a == c && b == d)

class (Ord t, Visible t) => OrdVis t
-- vSort :: Ordvis t => [t] -> String

-- show :: Show t => t -> String
-- read :: Read t => String -> t