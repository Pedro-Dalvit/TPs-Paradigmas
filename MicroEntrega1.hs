data Microprocesador = Microprocesador {memoria :: [Float], acumuladorA :: Float, acumuladorB :: Float, programCounter :: Int, mensajeError :: String} deriving (Show, Eq)

xt8088 = Microprocesador {memoria = replicate 1024 0, acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = ""}

fp20 = Microprocesador {memoria = replicate 1024 0, acumuladorA = 7, acumuladorB = 24, programCounter = 0, mensajeError = ""}

at8086 = Microprocesador  {memoria = [1..20] , acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = ""}

nop microprocesador = microprocesador {programCounter = programCounter microprocesador + 1}

add microprocesador = microprocesador {acumuladorA = acumuladorA microprocesador + acumuladorB microprocesador, acumuladorB = 0,programCounter = programCounter microprocesador + 1}

divide microprocesador | acumuladorB microprocesador == 0 = microprocesador {programCounter = programCounter microprocesador + 1 ,  mensajeError = "DIVISION BY ZERO"} | otherwise = microprocesador { acumuladorA = (acumuladorA microprocesador) / (acumuladorB microprocesador) , acumuladorB = 0, programCounter= programCounter microprocesador + 1}

swap microprocesador = microprocesador {acumuladorA = acumuladorB microprocesador, acumuladorB = acumuladorA microprocesador, programCounter = programCounter microprocesador + 1}

lod posicion microprocesador = microprocesador {acumuladorA = ((memoria microprocesador) !! (posicion-1)), programCounter= programCounter microprocesador + 1}

insertarDatoEnMemoria posicion valor lista = (take (posicion-1) lista) ++ [valor] ++ (drop posicion lista)

str posicion valor microprocesador = microprocesador { memoria = insertarDatoEnMemoria posicion valor (memoria microprocesador), programCounter = programCounter microprocesador + 1}

lodv valor microprocesador = microprocesador {acumuladorA = valor, programCounter = programCounter microprocesador + 1}

avanzarPCEn3 microprocesador = (nop.nop.nop) microprocesador

sumar valor1 conValor2 microprocesador = (add.lodv conValor2.swap.lodv valor1) microprocesador

divido valor1 porValor2 microprocesador = (divide.lod 1.swap.lod 2.str 2 porValor2.str 1 valor1) microprocesador