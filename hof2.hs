double :: Integer -> Integer
double x = x*2

triple :: Integer -> Integer
triple x = x*3

multiply :: Integer -> Integer -> Integer
multiply x y = x*y

twice :: (t -> t) -> (t -> t)
twice f = f . f

iter :: Integer -> (t -> t) -> (t -> t)
iter 0 f = id
iter n f = (iter (n-1) f) . f

zipWithh :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithh f = go
    where
        go [] _ = []
        go _ [] = []
        go (x:xs) (y:ys) = f x y : go xs ys -- zipWithh ($) [sum, product] [[1,2],[3,4]] = [3,12]

soma :: Integer -> (Integer -> Integer)
soma n = (\m -> n+m)

mapFuns :: [a -> b] -> a -> [b]
mapFuns [] x = []
mapFuns (f:fs) x = f x : mapFuns fs x -- mapFuns [double, triple] 132 = [264,396]

mapFuns2 fs x = map(\f -> f x) fs

comp2 :: (t -> u) -> (u -> u -> v) -> (t -> t -> v)
comp2 f g = (\x y -> g (f x) (f y)) -- comp2 length (+) "ola" "mundo" = 8

doubleList :: [Integer] -> [Integer]
doubleList = map (multiply 2) -- não trava. ele espera outro número pra multiplicar por 2, que virá dentro da lista