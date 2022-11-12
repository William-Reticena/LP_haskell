-- import Data.List

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
-- sum [] = 0
-- sum (x:xs) = x + sum xs
-- evens [] = 0
-- evens (x:xs)
--   | mod x 2 == 0 = x : evens xs
--   | otherwise = evens xs

-- fst :: (a, b) -> a
-- fst (x, _) = x


-- snd :: (a, b) -> a
-- snd (_, y) = y
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
isRecordNe tree node = True

isRecordNo :: CityLocalization -> CityLocalization -> Bool
isRecordNo tree node = True

isRecordSe :: CityLocalization -> CityLocalization -> Bool
isRecordSe tree node = True

insertRecordOrDefault :: CityLocalization -> CityLocalization -> CityLocalization
insertRecordOrDefault tree node
  | isRecordNe tree node =
    if _NE tree /= Empty 
    then tree { _NE = insertRecordOrDefault (_NE tree) node }
    else tree { _NE = node }
  | isRecordNo tree node = 
    if _NE tree /= Empty 
    then tree { _NE = insertRecordOrDefault (_NE tree) node }
    else tree { _NE = node }
  | isRecordSe tree node =
    if _NE tree /= Empty 
    then tree { _NE = insertRecordOrDefault (_NE tree) node }
    else tree { _NE = node }
  | otherwise =
    if _NE tree /= Empty 
    then tree { _NE = insertRecordOrDefault (_NE tree) node }
    else tree { _NE = node }

-- handleRecords:: Tree -> CityLocalization -> Tree
-- handleRecords record = do
-- j :: Bool
-- j = False
-- rootTree = "Empty"


run :: CityLocalization -> IO ()
run lastRecord = do
  putStrLn "Aperte 'q' para encerrar"
  putStrLn "Digite o nome de uma cidade: "
  city <- getLine

  if city /= "q"  then do
    putStrLn "Digite a latitude"
    inputLat <- getLine
    let lat = read inputLat :: Int

    putStrLn "Digite a longitude"
    inputLong <- getLine
    let long = read inputLong :: Int

    let recordCity = createRecord city lat long

    run (insertRecordOrDefault lastRecord recordCity)
  else do
    putStrLn "Você saiu!"
    return ()

main :: IO ()
main = run Empty


-- main :: IO ()
-- main = do
--   --recursão para entrada de dados
--   putStrLn "Aperte 'q' para encerrar"
--   putStrLn "Digite o nome de uma cidade:"
--   i <- getLine

--   if i /= "q"  then do
--     let city = i
--     putStrLn "Digite a latitude"
--     inpuLat <- getLine
--     let lat = read inpuLat :: Int

--     putStrLn "Digite a longitude"
--     inputLong <- getLine
--     let long = read inputLong :: Int

--     let recordCity = createRecord city lat long
--     let teste = "jd"
--     let teste = "df"
--     print teste

--     main
--   else do
--     putStrLn "Você saiu!"
--     return ()


  -- let (x, y) = (1, 2) in x
  -- let a = insert Null 'n'
  -- print $ a
  -- let b = insert a 'b'
  -- print $ b
  -- print $ insert b 'z'

  -- (1, 2) :: (Int, Int)
  -- let r = [("re", 6, 8, []), ("1", 9, 8, [])]
  -- let a = ["TG", 7, 8 ["CM", 4 , 5, [], [], [], []], ["MG", 7 , 3, [], [], [], []], [], []]
  -- let b = 4
  -- let c = b

  -- let d = a !! 1 !! 0
  -- print b
  -- print (r !! 0)
  -- print (r !! 1)
  -- let el = a
  -- print (el)



  -- let users = [("Saurabh", 35), ("John", 45), ("Doe", -5)]
  -- in  (find (\(_, age) -> age < 1 || age > 100) users) of
  --   Nothing -> Right users
  --   Just (name, age) -> Left $ name <> " seems to have an incorrect age: " <> show age