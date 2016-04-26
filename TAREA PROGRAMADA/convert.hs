
import PutJSON
import SimpleJSON

import System.IO
import  Data.List.Split
import Data.List

import Data.Maybe


{-
main1 = do 
       putJValue(convertCSV_to_JSON ["A","B","C","D","E"] ["N","X","B","N","X"]  [["10","hola mundo","True","1.2","e1"], ["12","arena y sol","False","1.2","e1"], ["34","hola mundo","True","1.2","e1"]])
-}



--FUNCIONES PARA CONVERTIR UN CSV A JSON

func :: [String]->[String]->[String]->[(String, JValue)]
func [nombreColumna] ["N"] [numero] = [(nombreColumna, JNumber (read numero :: Double))]
func [nombreColumna] ["X"] [string] = [(nombreColumna, JString string)]
func [nombreColumna] ["B"] [boolValue] = [(nombreColumna, JBool (read boolValue :: Bool))]
func (s':xs)(s'':ys)(ss:ws) = func [s'] [s''] [ss] ++ func xs ys ws


funcMapJObject :: [[(String, JValue)]] -> [JValue]
funcMapJObject array = map JObject array 


convertCSV_to_JSON :: [String]->[String]->[[String]]->JValue
convertCSV_to_JSON nombreColumnas tiposDatos columnas = JArray(funcMapJObject( map (func nombreColumnas tiposDatos) columnas) )


 -- *******************************************************************
loadCSV_to_JSON :: String -> IO JValue
loadCSV_to_JSON fileName = do
       --Leer el archivo
       inh <- openFile fileName ReadMode
       inpStr <- hGetContents inh

       --Obteniendo el nombre de las columnas y tos tipos de las columnas
       let archivoList = splitOn "\n" inpStr
       let nombreColumnas = splitOn ";" (archivoList !! 0)
       let tiposColumnas = splitOn ";" (archivoList !! 1)

       --Obteniendo las filas
       let filasAux = snd (splitAt 2 archivoList)
       let filas = init (map (splitOn ";") filasAux)

       let json = convertCSV_to_JSON nombreColumnas tiposColumnas filas

       return (json)





main :: IO()
main = do 
    --Leer el archivo
    inh <- openFile "R2.csv" ReadMode
    inpStr <- hGetContents inh
    --Obteniendo el nombre de las columnas y tos tipos de las columnas
    let archivoList = splitOn "\n" inpStr
    let nombreColumnas = splitOn ";" (archivoList !! 0)
    let tiposColumnas = splitOn ";" (archivoList !! 1)
    --Obteniendo las filas
    let filasAux = snd (splitAt 2 archivoList)
    let filas = init (map (splitOn ";") filasAux)

    print $ nombreColumnas
    print $ elemIndex "C" nombreColumnas
    print $ filas


      










-- **********************************************************************
conv :: String->String->IO()
conv fileCSV fileJSON = do

       jsonObject <- loadCSV_to_JSON (fileCSV)
       
       outh <- openFile fileJSON WriteMode

       let json = renderJValue (jsonObject)

       hPutStr outh json

       hClose outh





join :: IO()
join = do
       jsonObject <- loadCSV_to_JSON ("R1.csv")
       let arrayJObjects = fromJust (getArray jsonObject)

       print $ arrayJObjects

