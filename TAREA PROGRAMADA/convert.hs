
import PutJSON
import SimpleJSON

import System.IO
import  Data.List.Split
import Data.List

import Data.Maybe


l1 = [["a1","True","4"],["a2","False","2"],["a3","False","3"],["a4","True","4"]]
l2 = [["2","True","azul","no"],["2","False","blanco","se"],["3","True","rojo","se"],["4","False","azul","so"]]


ll1 = [[("A",JString "a1"),("B",JBool True),("C",JNumber 4.0)],[("A",JString "a2"),("B",JBool False),("C",JNumber 2.0)],[("A",JString "a3"),("B",JBool False),("C",JNumber 3.0)],[("A",JString "a4"),("B",JBool True),("C",JNumber 4.0)]]
ll2 = [[("C",JNumber 2.0),("E",JBool True),("F",JString "azul"),("H",JString "no")],[("C",JNumber 2.0),("E",JBool False),("F",JString "blanco"),("H",JString "se")],[("C",JNumber 3.0),("E",JBool True),("F",JString "rojo"),("H",JString "se")],[("C",JNumber 4.0),("E",JBool False),("F",JString "azul"),("H",JString "so")]]


-- **********************************************************************
-- **********************************************************************
-- **********************************************************************
-- **********************************************************************
-- **********************************************************************

--FUNCIONES PARA CONVERTIR UN CSV A JSON --------------------------------------------------------

func :: [String]->[String]->[String]->[(String, JValue)]
func [nombreColumna] ["N"] [numero] = [(nombreColumna, JNumber (read numero :: Double))]
func [nombreColumna] ["X"] [string] = [(nombreColumna, JString string)]
func [nombreColumna] ["B"] [boolValue] = [(nombreColumna, JBool (read boolValue :: Bool))]
func (s':xs)(s'':ys)(ss:ws) = func [s'] [s''] [ss] ++ func xs ys ws


funcMapJObject :: [[(String, JValue)]] -> [JValue]
funcMapJObject array = map JObject array 


convertCSV_to_JSON :: [String]->[String]->[[String]]->JValue
convertCSV_to_JSON nombreColumnas tiposDatos columnas = JArray(funcMapJObject( map (func nombreColumnas tiposDatos) columnas) )


 -- FUNCIÓN QUE CARGA UN ARCHIVO CSV LEIDO A UN JSON
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


-- **********************************************************************
-- **********************************************************************
-- **********************************************************************
-- **********************************************************************
-- **********************************************************************


-- FUNCIONES PARA UNIR DOS JSON -----------------------------------------------------------------------
joinRowToRows :: (Eq a) => [a] -> Int -> [[a]] -> Int -> [[a]]
joinRowToRows [] _ [] _ = []
joinRowToRows _ _ [] _ = []
joinRowToRows fila index (y : ys) index'
  | fila !! index == y !! index' = [fila ++ y] ++ joinRowToRows fila index ys index'
  | fila !! index /= y !! index' = joinRowToRows fila index ys index'

joinJSON :: (Eq a) => [[a]] -> Int -> [[a]] -> Int -> [[a]]
joinJSON [] _ [] _ = []
joinJSON lista1 _ [] _ = []
joinJSON [] _ lista2 _ = []
joinJSON (lista1 : resto1) index1 lista2 index2 = (joinRowToRows lista1 index1 lista2 index2) ++ joinJSON (resto1) (index1) (lista2) (index2)

--Función que actualiza las columnas o llaves del json
updateKey ::  [Char] -> [([Char], JValue)] ->  [([Char], JValue)]
updateKey _ [] = []
updateKey key (x : xs) = [(key ++ (fst x), snd x)] ++ updateKey key xs


prepareJSONForJOIN :: JValue -> IO [[([Char], JValue)]]
prepareJSONForJOIN jvalue = do
  let arrayJObjects = fromJust (getArray jvalue)
  let x = map (getObject) arrayJObjects
  let objectTuples = map (fromJust) x
  return (objectTuples)

getIndexof :: String -> String -> IO Int
getIndexof atributo file = do 
    --Leer el archivo
    inh <- openFile file ReadMode
    inpStr <- hGetContents inh
    --Obteniendo el nombre de las columnas y tos tipos de las columnas
    let archivoList = splitOn "\n" inpStr
    let nombreColumnas = splitOn ";" (archivoList !! 0)
    return (fromJust (elemIndex atributo nombreColumnas))



--------------------------------------------------------------------------------------------------
 
conv :: String->String->IO()
conv fileCSV fileJSON = do

       jsonObject <- loadCSV_to_JSON (fileCSV)
       
       outh <- openFile fileJSON WriteMode

       let json = renderJValue (jsonObject)

       hPutStr outh json

       hClose outh

--------------------------------------------------------------------------------------------------


join :: String->String->String->String->IO()
join rutaEntrada1 rutaEntrada2 atributoComun fileJSON = do

       jsonObjectEntrada1 <- loadCSV_to_JSON (rutaEntrada1)
       jsonObjectEntrada2 <- loadCSV_to_JSON (rutaEntrada2)

       objectTuples1 <- prepareJSONForJOIN jsonObjectEntrada1
       objectTuples2 <- prepareJSONForJOIN jsonObjectEntrada2

       index1 <- getIndexof atributoComun rutaEntrada1
       index2 <- getIndexof atributoComun rutaEntrada2

       let json = JArray (funcMapJObject (joinJSON objectTuples1 index1 objectTuples2 index2))

       outh <- openFile fileJSON WriteMode

       let outJSON = renderJValue (json)

       hPutStr outh outJSON

       hClose outh


-- CICLO DE EJECUCIÓN

type Estado = [(String, String)]