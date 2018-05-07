module MicroEntrega1 where

data Microprocesador = Microprocesador {memoria :: [Float], acumuladorA :: Float, acumuladorB :: Float, programCounter :: Int, mensajeError :: String} deriving (Show, Eq)

xt8088 = Microprocesador {memoria = replicate 1024 0, acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = ""}

fp20 = Microprocesador {memoria = replicate 1024 0, acumuladorA = 7, acumuladorB = 24, programCounter = 0, mensajeError = ""}

at8086 = Microprocesador  {memoria = [1..20] , acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = ""}

largoMemoria :: Microprocesador -> Int
largoMemoria microprocesador = length (memoria microprocesador)

nop :: Microprocesador -> Microprocesador
nop microprocesador = microprocesador {programCounter = programCounter microprocesador + 1}

add :: Microprocesador -> Microprocesador
add microprocesador = microprocesador {acumuladorA = acumuladorA microprocesador + acumuladorB microprocesador, acumuladorB = 0,programCounter = programCounter microprocesador + 1}

dividir :: Float -> Float -> Float
dividir dividendo divisor = dividendo / divisor

divide :: Microprocesador -> Microprocesador
divide microprocesador | acumuladorB microprocesador == 0 = microprocesador {programCounter = programCounter microprocesador + 1 ,  mensajeError = "DIVISION BY ZERO"} | otherwise = microprocesador { acumuladorA = dividir (acumuladorA microprocesador) (acumuladorB microprocesador) , acumuladorB = 0, programCounter= programCounter microprocesador + 1}

swap :: Microprocesador -> Microprocesador
swap microprocesador = microprocesador {acumuladorA = acumuladorB microprocesador, acumuladorB = acumuladorA microprocesador, programCounter = programCounter microprocesador + 1}

encontrarDatoEnMemoria :: Int -> Microprocesador -> Float
encontrarDatoEnMemoria posicion microprocesador = ((memoria microprocesador) !! (posicion-1))

lod :: Int -> Microprocesador -> Microprocesador
lod posicion microprocesador = microprocesador {acumuladorA = encontrarDatoEnMemoria posicion microprocesador, programCounter= programCounter microprocesador + 1}

insertarDatoEnMemoria:: Int -> Float -> [Float] -> [Float]
insertarDatoEnMemoria posicion valor lista = (take (posicion-1) lista) ++ [valor] ++ (drop posicion lista)

str :: Int -> Float -> Microprocesador -> Microprocesador
str posicion valor microprocesador = microprocesador { memoria = insertarDatoEnMemoria posicion valor (memoria microprocesador), programCounter = programCounter microprocesador + 1}

lodv :: Float -> Microprocesador -> Microprocesador
lodv valor microprocesador = microprocesador {acumuladorA = valor, programCounter = programCounter microprocesador + 1}

avanzarPCEn3 microprocesador = (nop.nop.nop) microprocesador

sumar :: Float -> Float -> Microprocesador -> Microprocesador
sumar valor1 conValor2 microprocesador = (add.lodv conValor2.swap.lodv valor1) microprocesador

divido:: Float -> Float -> Microprocesador -> Microprocesador
divido valor1 porValor2 microprocesador = (divide.lod 1.swap.lod 2.str 2 porValor2.str 1 valor1) microprocesador
