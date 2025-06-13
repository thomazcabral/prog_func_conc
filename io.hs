calcular :: String -> (Integer, Double)
calcular inputStr =
    let
        lista = words inputStr
        numeros = map read lista :: [Integer]
        soma = sum numeros
        media = fromIntegral soma / fromIntegral (length numeros)
    in
        (soma, media)

main :: IO()
main = do
    putStrLn "Primeira linha"
    putStrLn "Segunda linha"

    putStrLn "Digite o seu nome: "
    nome <- getLine
    putStrLn ("Olá, " ++ nome ++ "!!")

    putStrLn "Digite um número: "
    inputStr <- getLine -- getLine sempre retorna uma string
    let numero = read inputStr :: Integer
    putStrLn ("O dobro do seu número é " ++ show(numero*2))

    putStrLn "Digite números separados por espaço: "
    linha <- getLine
    let (soma, media) = calcular linha
    putStrLn ("Soma: " ++ show soma)
    putStrLn ("Média: " ++ show media)
