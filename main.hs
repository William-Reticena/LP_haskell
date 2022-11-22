import Debug.Trace
debug = flip trace

--command to input a file
--cat input.txt | ./main


data CityLocalization =
    Empty 
  | Record {
      city :: String,
      lat :: Int,
      long :: Int,
      _NE :: CityLocalization,
      _NO :: CityLocalization,
      _SO :: CityLocalization,
      _SE :: CityLocalization
    } deriving (Read, Show, Eq)

createRecord :: String -> Int -> Int -> CityLocalization
createRecord city lat long =
  Record {
    city = city,
    lat = lat,
    long = long,
    _NE = Empty,
    _NO = Empty,
    _SO = Empty,
    _SE = Empty
  }

getCityName :: CityLocalization -> String
getCityName record = city record

getCityCordinate :: CityLocalization -> (Int, Int)
getCityCordinate record = (lat record, long record)

isRecordNe :: CityLocalization -> CityLocalization -> Bool
isRecordNe prevNode currentNode = validLat && validLong `debug` ("NE " ++ show currentLat ++ " >= " ++ show prevLat  ++ " " ++ show currentLong ++ " < " ++ show prevLong)
  where
    prevCoordinates = getCityCordinate prevNode
    currentCoordinates = getCityCordinate currentNode
    prevLat = fst prevCoordinates
    prevLong = snd prevCoordinates
    currentLat = fst currentCoordinates
    currentLong = snd currentCoordinates
    validLat =  currentLat >= prevLat
    validLong = currentLong < prevLong


isRecordNo :: CityLocalization -> CityLocalization -> Bool
isRecordNo prevNode currentNode = (validLat && validLong) `debug` ("NO " ++ show currentLat ++ " >= " ++ show prevLat  ++ " " ++ show currentLong ++ " >= " ++ show prevLong)
  where
    prevCoordinates = getCityCordinate prevNode
    currentCoordinates = getCityCordinate currentNode
    prevLat = fst prevCoordinates
    prevLong = snd prevCoordinates
    currentLat = fst currentCoordinates
    currentLong = snd currentCoordinates
    validLat =  currentLat >= prevLat
    validLong = currentLong >= prevLong

isRecordSe :: CityLocalization -> CityLocalization -> Bool
isRecordSe prevNode currentNode = validLat && validLong `debug` ("SE " ++ show currentLat ++ " < " ++ show prevLat  ++ " " ++ show currentLong ++ " < " ++ show prevLong)
  where
    prevCoordinates = getCityCordinate prevNode
    currentCoordinates = getCityCordinate currentNode
    prevLat = fst prevCoordinates
    prevLong = snd prevCoordinates
    currentLat = fst currentCoordinates
    currentLong = snd currentCoordinates
    validLat = currentLat  < prevLat
    validLong =  currentLong < prevLong

getInfosNE :: CityLocalization -> CityLocalization
getInfosNE tree = _NE tree

insertRecordOrDefault :: CityLocalization -> CityLocalization -> CityLocalization -> CityLocalization
insertRecordOrDefault Empty _ node = node
insertRecordOrDefault tree prevNode node
  | isRecordNe tree node =
    if _NE tree /= Empty 
    then tree { _NE = insertRecordOrDefault (_NE tree) tree node } `debug` (city tree)
    else tree { _NE = node } `debug` (city tree)
  | isRecordNo tree node = 
    if _NO tree /= Empty
    then tree { _NO = insertRecordOrDefault (_NO tree) tree node } `debug` (city tree)
    else tree { _NO = node } `debug` (city tree)
  | isRecordSe tree node =
    if _SE tree /= Empty 
    then tree { _SE = insertRecordOrDefault (_SE tree) tree node } `debug` (city tree)
    else tree { _SE = node } `debug` (city tree)
  | otherwise =
    if _SO tree /= Empty
    then tree { _SO = insertRecordOrDefault (_SO tree) tree node } `debug` (city tree)
    else tree { _SO = node } `debug` (city tree)

run :: CityLocalization -> CityLocalization -> IO ()
run tree prevRecord = do
  putStrLn "Aperte 'q' para encerrar"
  putStrLn "Digite o nome de uma cidade: "
  city <- getLine

  if city /= "q" then do
    putStrLn "Digite a latitude"
    inputLat <- getLine
    let lat = read inputLat :: Int

    putStrLn "Digite a longitude"
    inputLong <- getLine
    let long = read inputLong :: Int

    let currentRecord = createRecord city lat long
    let updatedTree = insertRecordOrDefault tree tree currentRecord

    --print "-----------------------------------------------------------------------"
    --print "anterior"
    --print prev
    --print "-----------------------------------------------------------------------"
    --print "atual"
    --print currentRecord
    print "-----------------------------------------------------------------------"
    -- print "arvore"
    print updatedTree
    print "-----------------------------------------------------------------------"

    run updatedTree currentRecord
  else do
    putStrLn "Você saiu!"
    return ()

main :: IO ()
main = run Empty Empty
