

import System.Environment
import qualified Data.ByteString.Lazy as B

main = do
    copyFile "Equipos.csv" "something1.txt"

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
    contents <- B.readFile source
    printContent [contents]
    --B.writeFile dest contents


printContent:: [B.ByteString]->IO()
printContent stuff = do
print $ stuff


