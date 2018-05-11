module MicroEntrega1 where

data Microprocesador = Microprocesador {memoria :: [Int], acumuladorA :: Int, acumuladorB :: Int, programCounter :: Int, mensajeError :: String} deriving (Show, Eq)

xt8088 = Microprocesador {memoria = replicate 1024 0, acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = ""}

fp20 = Microprocesador {memoria = replicate 1024 0, acumuladorA = 7, acumuladorB = 24, programCounter = 0, mensajeError = ""}

at8086 = Microprocesador  {memoria = [1..20] , acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = ""}

avanzarPC :: Microprocesador -> Microprocesador
avanzarPC microprocesador = microprocesador {programCounter = programCounter microprocesador + 1}

nop :: Microprocesador -> Microprocesador
nop microprocesador = microprocesador {programCounter = programCounter microprocesador + 1}

add :: Microprocesador -> Microprocesador
add microprocesador = avanzarPC microprocesador {acumuladorA = acumuladorA microprocesador + acumuladorB microprocesador, acumuladorB = 0}

divide :: Microprocesador -> Microprocesador
divide microprocesador | acumuladorB microprocesador == 0 = avanzarPC microprocesador {mensajeError = "DIVISION BY ZERO"} 
                       | otherwise = avanzarPC microprocesador { acumuladorA = div (acumuladorA microprocesador) (acumuladorB microprocesador) , acumuladorB = 0}

swap :: Microprocesador -> Microprocesador
swap microprocesador = avanzarPC microprocesador {acumuladorA = acumuladorB microprocesador, acumuladorB = acumuladorA microprocesador}
swap1 :: Microprocesador -> Microprocesador
swap1 (Microprocesador memoria acumuladorA acumuladorB programCounter mensajeError) = avanzarPC (Microprocesador memoria acumuladorB acumuladorA programCounter mensajeError)

encontrarDatoEnMemoria :: Int -> Microprocesador -> Int
encontrarDatoEnMemoria posicion microprocesador = (memoria microprocesador) !! (posicion-1)

lod :: Int -> Microprocesador -> Microprocesador
lod posicion microprocesador = avanzarPC microprocesador {acumuladorA = encontrarDatoEnMemoria posicion microprocesador}

insertarDatoEnMemoria:: Int -> Int -> [Int] -> [Int]
insertarDatoEnMemoria posicion valor lista = (take (posicion-1) lista) ++ [valor] ++ (drop posicion lista)

str :: Int -> Int -> Microprocesador -> Microprocesador
str posicion valor microprocesador = avanzarPC microprocesador {memoria = insertarDatoEnMemoria posicion valor (memoria microprocesador)}

lodv :: Int -> Microprocesador -> Microprocesador
lodv valor microprocesador = avanzarPC microprocesador {acumuladorA = valor}

avanzarPCEn3 :: Microprocesador -> Microprocesador
avanzarPCEn3 microprocesador = (nop.nop.nop) microprocesador

sumar :: Int -> Int -> Microprocesador -> Microprocesador
sumar valor1 conValor2 microprocesador = (add.lodv conValor2.swap.lodv valor1) microprocesador

divido:: Int -> Int -> Microprocesador -> Microprocesador
divido valor1 porValor2 microprocesador = (divide.lod 1.swap.lod 2.str 2 porValor2.str 1 valor1) microprocesador
