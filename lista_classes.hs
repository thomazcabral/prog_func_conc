import Data.List (sort, group)

{- 1. Descobrir como duas listas diferem uma da outra. Se tiverem comprimentos diferentes,
devolva
Just "<comprimento da lista> /= <comprimento de outra lista>"

Se tiverem o mesmo comprimento, encontrar o primeiro índice i para o qual os os ele-
mentos diferem, e o retorno será

Just "<valor no indice i> /= < outro valor no indice i>" ,
Se as listas forem as mesmas, devolver Nothing
Escreva a assinatura de tipo para findDifference . Que classes de tipo são necessárias? -} 

--findDifference :: (Eq t, Show t) => [t] -> [t] -> Maybe String

{- 2. Este é um tipo para um vetor 3D. Implemente uma instância de Eq para ele.
data Vetor = Vetor Integer Integer Integer
deriving Show -}

data Vetor = Vetor Integer Integer Integer
        deriving Show

instance Eq Vetor where
    (Vetor a b c) == (Vetor d e f) = (a == d && b == e && c == f)

{- 3. Implemente uma instância de Num para Vetor de modo que as operações aritméticas fun-
cionem componente a componente. -}

instance Num Vetor where
    (Vetor a b c) + (Vetor d e f) = (Vetor (a+d) (b+e) (c+f))
    (Vetor a b c) * (Vetor d e f) = (Vetor (a*d) (b*e) (c*f))
    abs (Vetor a b c) = (Vetor (abs(a)) (abs(b)) (abs(c)))
    signum (Vetor a b c) = (Vetor (signum(a)) (signum(b)) (signum(c)))
    fromInteger a = (Vetor a a a)
    negate (Vetor a b c) = (Vetor (negate a) (negate b) (negate c))

{- 4. Calcular quantas vezes cada valor da lista ocorre. Devolver as frequências como uma
lista de pares (frequência,valor). Dica: pode usar funções de Data.List
Exemplo:
freqs [False ,False ,False ,True] ==> [(3,False),(1,True)] -}

freqs' :: Ord a => [a] -> [(Int, a)]
freqs' = map (\sublista -> (length sublista, head sublista)) . group . sort

{- 5. Implemente uma instância de Eq para o seguinte tipo de árvore binária.
data ITree = ILeaf | INode Int ITree ITree
deriving Show-}

data ITree = ILeaf | INode Integer ITree ITree
            deriving Show

instance Eq ITree where
    ILeaf == ILeaf = True
    INode a t1 t2 == INode b tt1 tt2 = (a == b && t1 == tt1 && t2 == tt2)
    _ == _ = False