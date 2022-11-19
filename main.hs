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

teste :: Int -> Int -> Int -> Int -> IO ()
teste prevLat prevLong currentLat currentLong = do
  print prevLat
  print prevLong
  print currentLat
  print currentLong


isRecordNe :: CityLocalization -> CityLocalization -> CityLocalization -> Bool
isRecordNe tree prevNode currentNode = validLat && validLong
  where
    prevCoordinates = getCityCordinate prevNode
    currentCoordinates = getCityCordinate currentNode
    prevLat = fst prevCoordinates
    prevLong = snd prevCoordinates
    currentLat = fst currentCoordinates
    currentLong = snd currentCoordinates
    validLat =  currentLat >= prevLat
    validLong = currentLong <= prevLong


isRecordNo :: CityLocalization -> CityLocalization -> CityLocalization -> Bool
isRecordNo tree prevNode currentNode = validLat && validLong
  where
    prevCoordinates = getCityCordinate prevNode
    currentCoordinates = getCityCordinate currentNode
    prevLat = fst prevCoordinates
    prevLong = snd prevCoordinates
    currentLat = fst currentCoordinates
    currentLong = snd currentCoordinates
    validLat =  currentLat >= prevLat
    validLong = currentLong >= prevLong

isRecordSe :: CityLocalization -> CityLocalization -> CityLocalization -> Bool
isRecordSe tree prevNode currentNode = validLat && validLong
  where
    prevCoordinates = getCityCordinate prevNode
    currentCoordinates = getCityCordinate currentNode
    prevLat = fst prevCoordinates
    prevLong = snd prevCoordinates
    currentLat = fst currentCoordinates
    currentLong = snd currentCoordinates
    validLat = currentLat  <= prevLat
    validLong =  currentLong < prevLong

getInfosNE :: CityLocalization -> CityLocalization
getInfosNE tree = _NE tree


insertRecordOrDefault :: CityLocalization -> CityLocalization -> CityLocalization -> CityLocalization
insertRecordOrDefault Empty _ node = node
insertRecordOrDefault tree prevNode node
  | isRecordNe tree prevNode node =
    if _NE tree /= Empty 
    then tree { _NE = insertRecordOrDefault (_NE tree) prevNode node }
    else tree { _NE = node }
  | isRecordNo tree prevNode node = 
    if _NO tree /= Empty
    then tree { _NO = insertRecordOrDefault (_NO tree) prevNode node }
    else tree { _NO = node }
  | isRecordSe tree prevNode node =
    if _SE tree /= Empty 
    then tree { _SE = insertRecordOrDefault (_SE tree) prevNode node }
    else tree { _SE = node }
  | otherwise =
    if _SO tree /= Empty
    then tree { _SO = insertRecordOrDefault (_SO tree) prevNode node }
    else tree { _SO = node }

getPrevRecord :: CityLocalization -> CityLocalization -> CityLocalization -> CityLocalization
getPrevRecord Empty Empty _ = Empty
getPrevRecord _ Empty current = current
getPrevRecord _ prev __ = prev

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
    let prev = getPrevRecord tree prevRecord currentRecord
    let updatedTree = insertRecordOrDefault tree prev currentRecord
    

    print "-----------------------------------------------------------------------"
    print "anterior"
    print prev
    print "-----------------------------------------------------------------------"
    print "atual"
    print currentRecord
    print "-----------------------------------------------------------------------"
    print "arvore"
    print updatedTree
    print "-----------------------------------------------------------------------"

    run updatedTree currentRecord
  else do
    putStrLn "Você saiu!"
    return ()

main :: IO ()
main = run Empty Empty