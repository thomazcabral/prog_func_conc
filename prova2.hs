type Equipamento = String
type Uso = (Equipamento, Int)
type ListaUso = [Uso]

inv :: ListaUso -> Bool
inv [x] = snd(x) > 0
inv (x:xs) = if snd(x) > 0 then inv xs else False

duracaoDe :: Equipamento -> ListaUso -> Int
duracaoDe x [] = 0
duracaoDe x (y:ys) = if x == fst(y) then snd(y) + duracaoDe x ys else duracaoDe x ys

bemFormada :: ListaUso -> Bool
bemFormada [] = True
bemFormada (x:xs) = if (inv ([x]) && snd(x) < 24) then bemFormada xs else False

removerEqp :: Equipamento -> ListaUso -> ListaUso
removerEqp e [] = []
removerEqp e (x:xs) = if e == fst(x) then removerEqp e xs else x : removerEqp e xs

type Preco = Int
type Tarifa = (Equipamento, Preco)
type Tarifas = [Tarifa]

definidoEm :: ListaUso -> Tarifas -> Bool
definidoEm [] _ = True
definidoEm (e:es) t = procuraEquipamento (fst e) t && definidoEm es t
    where
        procuraEquipamento :: Equipamento -> Tarifas -> Bool
        procuraEquipamento _ [] = False
        procuraEquipamento eq (t:ts) = if eq == fst t then True else procuraEquipamento eq ts

precoDe :: ListaUso -> Tarifas -> Preco
precoDe [] _ = 0
precoDe (e:es) t = custoUso + precoDe es t
    where
        tempo = snd e
        equipamento = fst e
        custoUso = tempo * procuraTarifa equipamento t

        procuraTarifa :: Equipamento -> Tarifas -> Preco
        procuraTarifa _ [] = error "Tarifa não definida"
        procuraTarifa e (t:ts) = if e == fst(t) then snd(t) else procuraTarifa e ts

vendas :: Int -> Int
vendas x = if x `mod` 2 == 0 then 0 else x*2

zeroVendas :: Int -> Int
zeroVendas xs = length [vendas x | x <- [0 .. xs], vendas x == 0]

zeroVendas0 :: Int -> Int
zeroVendas0 xs = length (filter (== 0) (map vendas [0 .. xs]))