intP :: (Int, Int)
intP = (33, 43)

minMax :: Integer -> Integer -> (Integer, Integer)
minMax a b
 | a >= b = (b, a)
 | otherwise = (a, b)

addPar :: (Integer, Integer) -> Integer
addPar (a, b) = a + b

comparaTripla :: (Integer, Integer, Integer) -> (Integer, Integer)
comparaTripla (a, b, c)
 | a >= b && a >= c && b >= c = (a, b)
 | b >= a && b >= c && a >= c = (b, a)
 | c >= a && c >= b && a >= b = (c, a)
 | a >= c && a >= b && c >= b = (a, c)
 | b >= c && b >= a && c >= a = (b, c)
 | otherwise = (c, b)

shift :: ((Integer, Integer), Integer) -> (Integer, (Integer, Integer))
shift ((a, b), c) = (a, (b, c))

addPar2 :: (Integer, Integer) -> Integer
addPar2 p = fst p + snd p -- primeiro e segundo da tupla

oneRoot :: Float -> Float -> Float -> Float
oneRoot a b c = -b / (2.0 * a)

twoRoots :: Float -> Float -> Float -> (Float, Float)
twoRoots a b c = ((-b + sqrt(b^2 - 4.0*a*c)) / (2.0 * a), (-b - sqrt(b^2 - 4.0*a*c)) / (2.0 * a))

roots :: Float -> Float -> Float -> String
roots a b c
 | b^2 == 4.0*a*c = show(oneRoot a b c)
 | b^2 > 4.0*a*c = show f ++ " " ++ show s
 | otherwise = "No roots"
   where
    f = fst(twoRoots a b c)
    s = snd(twoRoots a b c)

xAxis :: (Float, Float) -> (Float, Bool) -- y = ax + b
xAxis (a, b) = if a == 0 then (0, False) else (-b/a, True)