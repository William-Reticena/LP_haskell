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

handleSearch :: Int -> [(String, (Int, Int))] -> String -> Float -> [String]
handleSearch index list cityName distance =
  if compareCityNames tupleCityName cityName
    then map (\x -> if isWithinPerimeter tupleCord1 tupleCord2 (fst (snd x)) (snd (snd x)) distance then fst x else "" ) list
  else handleSearch (index + 1) list cityName distance
  --CityInfos { name = tupleCityName, coordinates = snd tuple } `debug` ("INFO 1" ++ show tupleCord1 ++  "INFO 2" ++ show tupleCord2)
  where
    tuple = extractTuple list index
    tupleCityName = fst tuple
    tupleCord = snd tuple
    tupleCord1 = fromIntegral (fst tupleCord) :: Float
    tupleCord2 = fromIntegral (snd tupleCord)  :: Float

searchByCity :: [(String, (Int, Int))] -> String -> Float -> [String]
searchByCity list cityName distance =
  handleSearch 0 list cityName distance

isWithinPerimeter :: Float -> Float -> Int -> Int -> Float -> Bool
isWithinPerimeter latX longX latY longY distance = distance >= calc `debug` ("calc " ++ show calc )
  where
    ltY = fromIntegral latY :: Float
    lgY = fromIntegral longY :: Float
    calc = sqrt (((latX - ltY) ^ 2) + ((longX - lgY) ^ 2))

insertList :: [String] -> String -> [String]
insertList list cityName = list ++ [cityName]

run :: CityLocalization -> CityLocalization -> [(String, (Int, Int))] -> IO ()
run tree prevRecord arrayList = do
  putStrLn "Aperte 'q' para encerrar, 'p' para pesquisar por uma cidade ou 'd' para fazer por perímetro"
  putStrLn "Digite o nome de uma cidade: "
  city <- getLine

  if city == "q" then do
    putStrLn "Você saiu!"
    return ()

  else if city == "p" then do
    putStrLn "Pesquise por uma cidade: "
    citySearch <- getLine

    putStrLn "Digite uma distância para a busca"
    inputDistance <- getLine
    let distance = read inputDistance :: Float

    let citySought = searchByCity arrayList citySearch distance
    print citySought

    run tree prevRecord arrayList

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

    let mapCities = map (\x -> if isWithinPerimeter lat long (fst (snd x)) (snd (snd x)) distance then fst x else "" ) arrayList
    let listOfCities = filter (\x -> x /= "") mapCities
    print listOfCities

    run tree prevRecord arrayList
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
    let updatedList = insertTupleList arrayList cityList
    print updatedList

    print "-----------------------------------------------------------------------"
    -- print "arvore"
    print updatedTree
    print "-----------------------------------------------------------------------"

    run updatedTree currentRecord updatedList

main :: IO ()
main = run Empty Empty []