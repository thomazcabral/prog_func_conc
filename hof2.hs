double :: Integer -> Integer
double x = x*2

iter :: Integer -> (t -> t) -> (t -> t)
iter 0 f = id
iter n f = (iter (n-1) f) . f

zipWithh :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithh f = go
    where
        go [] _ = []
        go _ [] = []
        go (x:xs) (y:ys) = f x y : go xs ys

soma :: Integer -> (Integer -> Integer)
soma n = (\m -> n+m)