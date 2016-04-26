
type Estado = [(String,String)]

main :: IO ()
main = do 
       mainloop []

mainloop :: Estado -> IO ()
mainloop estado = do
    putStr ">> "
    inpStr <- getLine
    let (terminar,nuevoestado,salida) = procesar inpStr estado
    putStrLn salida
    if terminar
       then return ()
	   else mainloop nuevoestado

procesar :: String -> Estado -> (Bool, Estado, String)
procesar comando estado =
     case tokens!!0 of
          "def" -> cmd_def (tail tokens) estado
          "borrar" -> cmd_borrar (tail tokens) estado
          "imp" -> cmd_imp estado   
          "fin" -> (True, estado, "Saliendo...")
          _     -> cmd_desconocido (tokens!!0) comando estado
       where tokens = words comando

  
cmd_def::[String] -> Estado -> (Bool, Estado, String)
cmd_def tokens estado = (False, nuevoestado, mensaje)
       where nuevoestado = estado ++ [(tokens!!0, tokens!!1)]
             mensaje = "Definido " ++ tokens!!0

cmd_borrar::[String] -> Estado -> (Bool, Estado, String)
cmd_borrar [] estado = (False, estado, "No se especificó qué borrar")
cmd_borrar (v:_) estado = let (res, nuevoestado) = borrar v estado
                             in if res
                                  then (False, nuevoestado, v ++ " borrado")
                                  else (False, estado, v ++ " no estaba definido")

buscar :: String -> Estado -> (Bool, String)
buscar _ [] = (False, "")
buscar v1 ((v2,y):estado) = if v1 == v2
                               then (True,y)
                               else  buscar v1 estado

borrar :: String -> Estado -> (Bool, Estado)
borrar _ [] = (False, [])
borrar v1 ((v2,y):estado) = let (res,nuevoestado) = borrar v1 estado
                                 in if v1 == v2
                                      then (True,estado)
                                      else  (res, (v2,y):nuevoestado)

cmd_desconocido :: String -> String -> Estado -> (Bool, Estado, String)
cmd_desconocido cmd comando estado = (False, estado, mensaje)
       where mensaje = "Comando desconocido ("++ cmd ++"): '" ++ comando ++ "'"

cmd_imp :: Estado -> (Bool, Estado, String)
cmd_imp estado = (False, estado, show estado)

cmd_fin :: Estado -> (Bool, Estado, String)
cmd_fin estado = (False, estado, show estado)