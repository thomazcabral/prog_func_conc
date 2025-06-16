mapMaybe :: [a] -> (a -> Maybe b) -> [b]
mapMaybe [] f = []
mapMaybe (x:xs) f =
    case f x of
        Nothing -> mapMaybe xs f
        (Just y) -> y : mapMaybe xs f

data Eitherr a b = Leftt a | Rightt b
                deriving (Eq, Ord, Read, Show)

classifica :: [Eitherr a b] -> ([a], [b])
classifica [] = ([], [])
classifica (x:xs) = 
    case x of
        Leftt x -> (x:(fst(classifica xs)), (snd(classifica xs)))
        Rightt x -> ((fst(classifica xs)), x:(snd(classifica xs)))