
module PutJSON (renderJValue, putJValue) where

import Data.List (intercalate)
import SimpleJSON

renderJValue :: JValue -> String

renderJValue (JString s) = show s
renderJValue (JNumber n) = show n
renderJValue (JBool True) = "true"
renderJValue (JBool False) = "false"
renderJValue JNull = "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
 where pairs [] = ""
       pairs ps = intercalate ", " (map renderPair ps)
       renderPair (k, v) = show k ++ ": " ++ renderJValue v

renderJValue (JArray a) = "[" ++ values a ++ "]"
 where values [] = ""
       values vs = intercalate ", " (map renderJValue vs)

       	   
putJValue :: JValue -> IO()
putJValue v = putStrLn(renderJValue v)


main = putJValue(JArray[(JObject [("foo", JNumber 1), ("bar", JBool False)]), (JObject [("foo", JNumber 1), ("bar", JBool False)]) ])




--FUNCIONES PARA CONVERTIR UN CSV A JSON
{-|
func :: [String]->[String]->[String]->[(String, JValue)]
func [nombreColumna] ["N"] [numero] = [(nombreColumna, JNumber (read numero :: Double))]
func [nombreColumna] ["X"] [string] = [(nombreColumna, JString string)]
func [nombreColumna] ["B"] [boolValue] = [(nombreColumna, JBool (read boolValue :: Bool))]
func (s':xs)(s'':ys)(ss:ws) = func [s'] [s''] [ss] ++ func xs ys ws


funcMapJObject :: [[(String, JValue)]] -> [JValue]
funcMapJObject array = map JObject array 



convertCSV_to_JSON :: [String]->[String]->[[String]]->JValue
convertCSV_to_JSON nombreColumnas tiposDatos columnas = JArray(funcMapJObject( map (func nombreColumnas tiposDatos) columnas) )


main1 = do 
	putJValue(convertCSV_to_JSON ["A","B","C","D","E"] ["N","X","B","N","X"]  [["10","hola mundo","True","1.2","e1"], ["12","arena y sol","False","1.2","e1"], ["34","hola mundo","True","1.2","e1"]])
-}