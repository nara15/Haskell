
-- Ejemplo de un ciclo interactivo en Haskell

-- El ciclo principal recibe un estado (tira de caracteres),
-- en cada iteración del ciclo principal, se lee una línea,
-- la cual se interpreta como si fuera un comando.
--    si empieza con "f" terminar (fin)
--    si empieza con "p" imprimir estado actual (print)
--    si empieza con "a" agregar resto de la línea al final del estado
--       esto genera un nuevo estado
--
-- Luego de interpretar el comando, se invoca recursivamente 
-- al ciclo principal con el nuevo estado si es del caso.

-- main es una función que retorna una acción la cual al ser ejecutada
-- no retorna nada, pero puede tener efectos colaterales.
main :: IO ()
main = do 
       mainloop ""  -- Ejecutar mainloop con estado inicial nulo

-- mainloop es una función que recibe un estado (String) y 
-- que retorna una acción la cual al ser ejecutada no retorna nada, 
-- pero puede tener efectos colaterales (lee, modifica, escribe)
mainloop :: String -> IO ()
mainloop estado = do
    -- putStr::String -> IO (), es una función que recibe una tira y 
    -- devuelve una acción, la cual al ser ejecutada imprime la tira 
    --(efecto colateral) y retorna nada
    putStr ">> "
    
    -- getLine es una función que retorna una acción que al ser ejecutada
    -- retorna una tira; la construcción "<-" ejecuta la acción de getLine
    -- y extrae la tira leída
    inpStr <- getLine
    
    -- procesar es una función "pura" que toma la tira de entrada y el estado,
    -- y devuelve una tripleta:
    --     booleano terminar que indica si se debe terminar el ciclo principal (fin)
    --     tira nuevoestado obtenido al ejecutar el comando en el estado actual
    --     tira salida con un texto que se imprime como resultado del comando
    let (terminar,nuevoestado,salida) = procesar inpStr estado
    
    -- impresión de la salida provocada por la ejecución del como
    putStrLn salida
    
    -- terminar el ciclo si el comando así lo indica
    -- en caso contrario usar recursión de cola para obtener 
    -- el siguiente comando con el nuevo estado
    if terminar
       then return ()  -- return crea una acción que al ser ejecutada no retorna nada
                       -- es lo que se supone que debe devolver mainloop
       else mainloop nuevoestado

-- procesar recibe un comando y un estado y calcula la tripleta que
-- se produce al ejecutar el comando y afectar al estado       
procesar:: String -> String -> (Bool, String, String)
-- si el comando empieza con 'f' es fin, se debe terminar el programa,
-- la tripleta quedaría:
--     (terminar el ciclo, mismo estado, mensaje de terminación)
procesar ('f':_) estado  =  (True, estado, "Saliendo...")

-- si el comando empieza con 'p' es print, se debe mostrar el estado,
-- la tripleta quedaría:
--     (no terminar el ciclo, mismo estado, mensaje con el contenido del estado)
procesar ('p':_) estado  =  (False, estado, "Estado: " ++ estado)

-- si el comando empieza con 'a' es agregar, 
--    agregar el resto de la línea de comando al final del estado,
-- la tripleta quedaría:
--     (no terminar el ciclo, nuevo estado concatenando resto comando, mensaje)
procesar ('a':xs) estado =  (False, estado ++ "::" ++ xs, "Agregar: " ++ xs) 

-- si el comando es cualquier otra cosa, no cambiar nada, 
-- simplemente indicar que es un comando desconocido
-- la tripleta quedaría:
--     (no terminar el ciclo, mismo estado, mensaje de comando desconocido)
procesar _ estado = (False, estado, "No entiendo!")