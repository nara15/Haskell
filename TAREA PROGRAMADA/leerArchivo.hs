import System.IO



leerArchivo :: String -> IO String
leerArchivo fileName = do
       inh <- openFile fileName ReadMode
       inpStr <- hGetContents inh
       return (inpStr)

main  = do
       str <- leerArchivo "R1.csv"

       print $ str
