funcaoConstante :: Int
funcaoConstante = 30

double :: Float -> Float
double x = x + x

doubleus :: Float -> Float -> Float
doubleus x y = x*2 + y*2

dsn :: Float -> Float
dsn x = (if x > 100 then x else x*2) + 1

square :: Int -> Int
square x = x*x

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

dw :: (Integral a) => a -> String
dw 1 = "Sunday"
dw 2 = "Monday"
dw 3 = "Tuesday"
dw 4 = "Wednesday"
dw 5 = "Thursday"
dw 6 = "Friday"
dw 7 = "Saturday"
dw x = "Not a day of the week!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell w h
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"
    where 
        bmi = w/h ^ 2
        skinny = 18.5
        normal = 25.0
        fat = 30.0

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b
    | a > b = GT
    | a == b = EQ
    | otherwise = LT

tresiguais :: Int -> Int -> Int -> Bool
tresiguais a b c = (a == b) && (a == c)

max3 :: Int -> Int -> Int -> Int
max3 a b c
    | a >= b && a >= c = a
    | b >= c = b
    | otherwise = c