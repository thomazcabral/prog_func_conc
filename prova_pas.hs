testaElementos :: (a -> Bool) -> [a] -> Bool
testaElementos f [] = True
testaElementos f (x:xs) = if f x then (testaElementos f xs) else False

testaElementos0 :: (a -> Bool) -> [a] -> Bool
testaElementos0 = and . map

testaElementos1 :: (a -> Bool) -> [a] -> Bool
testaElementos1 f xs = foldr (&&) True (map f xs)

merge :: Ord a => [a] -> [a] -> [a]
merge [] (x:xs) = (x:xs)
merge (x:xs) [] = (x:xs)
merge (x:xs) (y:ys) = if x < y then x : (merge (xs) (y:ys)) else y : (merge (x:xs) (ys))

metade :: [a] -> ([a], [a])
metade xs = (metade1, metade2)
    where
        half = (length xs) `div` 2
        metade1 = take half xs
        metade2 = drop half xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort (fst (metade xs))) (msort (snd (metade xs)))

data BTree = Leaf | Node (BTree) Int (BTree)
            deriving Show

inserirValor :: Int -> BTree -> BTree
inserirValor i Leaf = Node Leaf i Leaf
inserirValor i (Node t1 i0 t2)
    | i < i0 = Node (inserirValor i t1) i0 t2
    | i > i0 = Node t1 i0 (inserirValor i t2)
    | otherwise = Node t1 i0 t2

arvLista :: BTree -> [Int]
arvLista Leaf = []
arvLista (Node t1 i t2) = [i] ++ arvLista t1 ++ arvLista t2

somaArv :: BTree -> Int
somaArv Leaf = 0
somaArv (Node t1 i t2) = i + somaArv t1 + somaArv t2

listaArv :: [Int] -> BTree
listaArv [] = Leaf
listaArv [x] = Node Leaf x Leaf
listaArv (x:xs) = Node Leaf x (listaArv xs)