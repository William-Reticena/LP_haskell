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
isRecordNe tree Empty currentNode = True
isRecordNe tree prevNode currentNode = validLat && validLong `debug` ("NE " ++ show currentLat ++ " >= " ++ show prevLat  ++ " " ++ show currentLong ++ " < " ++ show prevLong)
  where
    prevCoordinates = getCityCordinate prevNode
    currentCoordinates = getCityCordinate currentNode
    prevLat = fst prevCoordinates
    prevLong = snd prevCoordinates
    currentLat = fst currentCoordinates
    currentLong = snd currentCoordinates
    validLat =  currentLat >= prevLat
    validLong = currentLong < prevLong


isRecordNo :: CityLocalization -> CityLocalization -> CityLocalization -> Bool
isRecordNo tree Empty currentNode = True
isRecordNo tree prevNode currentNode = (validLat && validLong) `debug` ("NO " ++ show currentLat ++ " >= " ++ show prevLat  ++ " " ++ show currentLong ++ " >= " ++ show prevLong)
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
isRecordSe tree Empty currentNode = True
isRecordSe tree prevNode currentNode = validLat && validLong `debug` ("SE " ++ show currentLat ++ " < " ++ show prevLat  ++ " " ++ show currentLong ++ " < " ++ show prevLong)
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
  | isRecordNe tree tree node =
    if _NE tree /= Empty 
    then tree { _NE = insertRecordOrDefault (_NE tree) tree node } `debug` (city tree)
    else tree { _NE = node } `debug` (city tree)
  | isRecordNo tree tree node = 
    if _NO tree /= Empty
    then tree { _NO = insertRecordOrDefault (_NO tree) tree node } `debug` (city tree)
    else tree { _NO = node } `debug` (city tree)
  | isRecordSe tree tree node =
    if _SE tree /= Empty 
    then tree { _SE = insertRecordOrDefault (_SE tree) tree node } `debug` (city tree)
    else tree { _SE = node } `debug` (city tree)
  | otherwise =
    if _SO tree /= Empty
    then tree { _SO = insertRecordOrDefault (_SO tree) tree node } `debug` (city tree)
    else tree { _SO = node } `debug` (city tree)

isLeaf :: CityLocalization -> Bool
isLeaf node =  (_NO node == Empty) && (_NE node == Empty) && (_SO node == Empty) && (_SE node == Empty)

verifyPrev :: CityLocalization -> CityLocalization -> CityLocalization -> CityLocalization
verifyPrev tree node current
  | node == Empty = tree
  | isRecordNe node node current =
    if isLeaf node then node else verifyPrev tree (_NE node) current
  | isRecordNo node node current =
    if isLeaf node then node else verifyPrev tree (_NO node) current
  | isRecordSe node node current =
    if isLeaf node then node else verifyPrev tree (_SE node) current
  | otherwise =
    if isLeaf node then node else verifyPrev tree (_SO node) current


  -- | isLeaf node = _NE node
  -- | isRecordNe node prevNode current =
  --   if isLeaf (_NE node) then _NE node else verifyPrev (_NE node) (_NE node) current
  -- | isRecordNo node prevNode current =
  --   if isLeaf (_NO node) then _NO node else verifyPrev (_NO node) (_NO node) current
  -- | isRecordSe node prevNode current =
  --   if isLeaf (_SE node) then _SE node else verifyPrev (_SE node) (_SE node) current
  -- | otherwise =
  --   if isLeaf (_SO node) then _SO node else verifyPrev (_SO node) (_SO node) current




  -- if isLeaf node then Empty else Empty
  -- if isRecordNe node prevNode current then _NE node else verifyPrev (_NE node) (_NE node) current
  -- if isRecordNo node prevNode current then _NO node else verifyPrev (_NO node) (_NO node) current
--     else if isRecordNo node prevNode current then _SO node else verifyPrev (_SO node) (_SO node) current
--     else isRecordNo node prevNode current then _SE node else verifyPrev (_SE node) (_SE node) current






  -- if isLeaf node 
  -- then Empty
  -- else
  --   if isRecordNe node prevNode current
  --   then
  --     if isLeaf (_NE node)
  --     then _NE node
  --     else verifyPrev (_NE node) (_NE node) current
  --   else Empty


getPrevRecord :: CityLocalization -> CityLocalization -> CityLocalization
getPrevRecord Empty _ = Empty
getPrevRecord tree current =
  -- | isLeaf tree = Empty
  verifyPrev tree tree current
  -- | isRecordNo tree tree current = verifyPrev tree tree current
  -- | isRecordSe tree (_SE tree) current = verifyPrev tree tree current
  -- | otherwise = verifyPrev tree tree current


  -- let prev = verifyPrev tree Empty current
  -- if prev == Empty
  -- then tree
  -- else prev



-- getPrevRecord :: CityLocalization -> CityLocalization -> CityLocalization -> CityLocalization
-- getPrevRecord Empty Empty _ = Empty
-- getPrevRecord _ Empty current = current
-- getPrevRecord _ prev __ = prev

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
    let prev = getPrevRecord tree currentRecord
    -- print prev
    let updatedTree = insertRecordOrDefault tree prev currentRecord

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
