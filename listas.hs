ehPar :: Integer -> Bool
ehPar x = mod x 2 == 0

ex = [2,4,7]

dobraLista xs = [x*2 | x <- ex]
dobraListaPar xs = [x*2 | x <- ex, ehPar x]

adicionaPar :: [(Integer, Integer)] -> [Integer]
adicionaPar x = [a+b | (a, b) <- x]

adicionaParOrd :: [(Integer, Integer)] -> [Integer]
adicionaParOrd x = [a+b | (a, b) <- x, a < b]

todosPares xs = (xs == [x | x <- xs, ehPar x])
todosImpares xs = ([] == [x | x <- xs, ehPar x])
