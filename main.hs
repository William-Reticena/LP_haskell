
import Debug.Trace
debug = flip trace

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

data CityInfos =
  CityInfos {
    name :: String,
    coordinates :: (Int, Int)
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
    then tree { _SO = insertRecordOrDefault (_SO tree) tree node }
    else tree { _SO = node }

insertTupleList :: [(String, (Int, Int))] -> [(String, (Int, Int))] -> [(String, (Int, Int))]
insertTupleList list xs = list ++ xs

extractTuple :: [(String, (Int, Int))] -> Int -> (String, (Int, Int))
extractTuple list index = list !! index

compareCityNames :: String -> String -> Bool
compareCityNames cityName cityNameCp = cityName == cityNameCp

handleSearch :: Int -> [(String, (Int, Int))] -> String -> CityInfos
handleSearch index list cityName =
  if compareCityNames tupleCityName cityName
    then CityInfos { name = tupleCityName, coordinates = snd tuple }
  else handleSearch (index + 1) list cityName

  where
    tuple = extractTuple list index
    tupleCityName = fst tuple

searchByCity :: [(String, (Int, Int))] -> String -> CityInfos
searchByCity list cityName =
  handleSearch 0 list cityName

isWithinPerimeter :: Float -> Float -> Int -> Int -> Float -> Bool
isWithinPerimeter latX longX latY longY distance = distance <= calc `debug` (show ltY ++ " " ++ show lgY)
  where
    -- ltX = fromIntegral latX :: Float
    -- lgX = fromIntegral longX :: Float
    ltY = fromIntegral latY :: Float
    lgY = fromIntegral longY :: Float
    -- t = ((latX - ltY) ^ 2 + (longX - lgY) ^ 2)
    calc = sqrt ((latX - ltY) ^ 2 + (longX - lgY) ^ 2)
    -- ((latX - latY) ^ 2 + (longX - longY) ^ 2)

insertList :: [String] -> String -> [String]
insertList list cityName = list ++ [cityName]

handlePerimeterSearch :: Int -> Float -> Float -> Float -> [(String, (Int, Int))] -> [String] -> Int -> [String]
handlePerimeterSearch index lat long distance list cityList elementsQuantity =
  if isWithinPerimeter lat long latElementList longElementList distance
    then insertList cityList (fst tuple)
  else if index == elementsQuantity
    then handlePerimeterSearch (index + 1) lat long distance list cityList elementsQuantity
  else cityList

  where
    tuple = extractTuple list index
    coordinates = snd tuple 
    latElementList = fst coordinates
    longElementList = snd coordinates

perimeterSearch :: Float -> Float -> Float -> [(String, (Int, Int))] -> Int -> [String]
perimeterSearch lat long distance list elementsQuantity =
  handlePerimeterSearch 0 lat long distance list [] elementsQuantity


run :: CityLocalization -> CityLocalization -> [(String, (Int, Int))] -> Int -> IO ()
run tree prevRecord arrayList countElements = do
  putStrLn "Aperte 'q' para encerrar, 'p' para pesquisar por uma cidade ou 'd' para fazer por perímetro"
  putStrLn "Digite o nome de uma cidade: "
  city <- getLine

  if city == "q" then do
    putStrLn "Você saiu!"
    return ()

  else if city == "p" then do
    putStrLn "Pesquise por uma cidade: "
    citySearch <- getLine

    let citySought = searchByCity arrayList citySearch
    print citySought

    run tree prevRecord arrayList countElements

  else if city == "d" then do
    putStrLn "Digite uma latitude para a busca"
    inputLat <- getLine
    let lat = read inputLat :: Float

    putStrLn "Digite uma longitude para a busca"
    inputLong <- getLine
    let long = read inputLong :: Float

    putStrLn "Digite uma distância para a busca"
    inputDistance <- getLine
    let distance = read inputDistance :: Float

    let listOfCities = perimeterSearch lat long distance arrayList countElements
    print listOfCities

    run tree prevRecord arrayList countElements
  else do
    putStrLn "Digite a latitude"
    inputLat <- getLine
    let lat = read inputLat :: Int

    putStrLn "Digite a longitude"
    inputLong <- getLine
    let long = read inputLong :: Int

    let currentRecord = createRecord city lat long
    let updatedTree = insertRecordOrDefault tree tree currentRecord

    let cityList = [(city, getCityCordinate currentRecord)]
    let elementsQuantity = countElements + 1
    let updatedList = insertTupleList arrayList cityList
    print updatedList

    print "-----------------------------------------------------------------------"
    -- print "arvore"
    print updatedTree
    print "-----------------------------------------------------------------------"

    run updatedTree currentRecord updatedList elementsQuantity

main :: IO ()
main = run Empty Empty [] 0