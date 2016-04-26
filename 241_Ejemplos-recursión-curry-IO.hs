import System.IO
import Data.Char(toUpper)

-- función que toma una lista de pares enteros, tiras de caracteres,
-- y concatene las tiras incluyendo un separador
miconcat :: [(Int,[Char])]->Char->[Char]
miconcat [] _ = ""
miconcat [(_,s)] _ = s
miconcat ((_,s):rs) c = s ++ [c] ++ (miconcat rs c)


-- función que toma una lista de pares enteros, tiras de caracteres,
-- y retorna un par con la suma de enteros y la concatenación de las
-- tiras incluyendo un separador
sumconcat :: [(Int,[Char])]->Char->(Int,[Char])
sumconcat [] _ = (0,"")
sumconcat [(n,s)] _ = (n,s)
sumconcat ((n,s):rs) c = (n+nn,s ++ [c] ++ ss)
               where (nn,ss) = (sumconcat rs c)


-- Ejemplos de currying	
miconcatfijo :: Char->[Char]
miconcatfijo = miconcat [(1,"uno"),(1,"uno"),(2,"dos"),(3,"tres")]

sumconcat' :: Char->(Int,[Char])
sumconcat' = sumconcat [(1,"uno"),(1,"uno"),(2,"dos"),(3,"tres"),(5,"cinco"),(8,"ocho")]


-- Ejemplo de currying para el segundo argumento (separador)   
sumcoma = sumacomacat' ','
             where sumacomacat' c lp = sumconcat lp c


-- Entrada/Salida básico
main = do
    putStrLn "Greetings!  What is your name?"
    inpStr <- getLine
    putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"


-- Combinación E/S con funciones "puras"
name2reply :: String -> String
name2reply name =
    "Pleased to meet you, " ++ name ++ ".\n" ++
    "Your name contains " ++ charcount ++ " characters."
    where charcount = show (length name)

main2 :: IO ()
main2 = do
       putStrLn "Greetings once again.  What is your name?"
       inpStr <- getLine
       let outStr = name2reply inpStr
       putStrLn outStr


-- Manejo de archivos arbitrarios con ciclo de lectura
main3 :: IO ()
main3 = do 
       inh <- openFile "input.txt" ReadMode
       outh <- openFile "output.txt" WriteMode
       mainloop inh outh
       hClose inh
       hClose outh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = 
    do ineof <- hIsEOF inh
       if ineof
           then return ()
           else do inpStr <- hGetLine inh
                   hPutStrLn outh (map toUpper inpStr)
                   mainloop inh outh


-- E/S perezosa (lee todo el archivo "de una vez")
main4 :: IO ()
main4 = do 
       inh <- openFile "input.txt" ReadMode
       outh <- openFile "output.txt" WriteMode
       inpStr <- hGetContents inh
       let result = processData inpStr
       hPutStr outh result
       hClose inh
       hClose outh

processData :: String -> String
processData = map toUpper


-- Otra forma de hacer E/S perezosa
main5 = do 
       inpStr <- readFile "input.txt"
       writeFile "output.txt" (map toUpper inpStr)
