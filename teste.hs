data Student = Student {
    name :: String,
    grade :: Int
} deriving (Read, Show)
-----------------------------
data Tree a =
    Null
  | Leaf a
  | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

insert :: (Ord a) => Tree a -> a -> Tree a
insert (Node x l r) y
  | y > x = Node x l (insert r y)
  | y < x = Node x (insert l y) r
insert (Leaf x) y
  | y > x = Node x Null (Leaf y)
  | y < x = Node x (Leaf x) Null
insert Null b = Leaf b
-----------------------------
in_range min max x =
  let in_lower_bound = min <= x
      in_upper_bound = max >= x
  in (in_lower_bound && in_upper_bound)

in_range min max x = ilb && iub 
  where
    ilb = min <= x
    iub = max >= x

in_range min max x =
  if ilb then iub else False
  where
    ilb = min <= x
    iub = max >= x
-----------------------------
add a b = a + b
-----------------------------
fac n
  | n <= 1 = 1
  | otherwise = n * fac (n-1)

fac n = aux n 1
  where
    aux n acc
      | n <= 1 = acc
      | otherwise = aux (n-1) (n*acc)
--------------------------------
is_zero 0 = True
is_zero _ = False
---------------------------------
asc :: Int -> Int -> [Int]
asc n m
  | m < n = []
  | m == n = [m]
  | m > n = n : asc (n+1) m
--------------------------------
addTuples :: [(Int, Int)] -> [Int]
addTuples xs = [x+y | (x, y) <- xs]
--------------------------------
app :: (a -> b) -> a -> b
app f x = f x

add1 :: Int -> Int
add1 x = x + 1
------------------------------
descSort = reverse . sort
descSort = (\x -> reverse (sort x))
descSort x = reverse (sort x)

count e = 
  foldr (\x acc -> if e==x then acc+1 else acc) 0

isAll e = foldr (\x -> (&&) $ e == x)
isAll e = foldr (\x acc -> e == x && acc)

length e = foldr (const $ (+) 1) 0 --caso queira ignorar o paramentro de uma anonymous
length = foldr (\x -> (+) 1) 0
map f = foldr ((:) . f) []
-----------------------------
data Calculation =
  Add Int Int | Sub Int Int | Mul Int Int | Div Int Int
calc :: Calculation -> Int
calc (Add a b) = a + b
calc (Sub a b) = a - b
calc (Mul a b) = a * b
calc (Div a b) = div x y


data Tree a = Leaf | Node (Tree a) a (Tree a)
tree :: Tree Int
tree =
  Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 3 Leaf) 4 Leaf)

data Person = Person {name :: String, age :: Int}
greet :: Person -> String
greet person = "Hi " ++ name person
greet (Person name _) = "Hi " ++ name

data Point =
    D2 { x :: Int, y :: Int }
  | D3 { x :: Int, y :: Int, z :: Int }
sumGrades :: [Student] -> Int
sumGrades students = sum $ map grade students
-----------------------------------------
averageGrades :: [Student] -> Double
averageGrades students = fromIntegral (sumGrades students) / fromIntegral (length students)

readStudentLines :: Int -> IO [Student]
readStudentLines 0 = return []
readStudentLines count = do
  current <- fmap (\student -> read student :: Student) getLine
  theRest <- readStudentLines (count -1)
  return $ current : theRest

main :: IO ()
main = do
  -----------------------------------------------------------------
  putStrLn "How many student records do you wish to enter? (Must be int)"

  numberStudents <- readLn :: IO Int

  putStrLn $ "Start entering your " ++ show numberStudents ++ " student records"
  putStrLn "(Note, they must be in the format: Student {name, grade} )"
  putStrLn $ "For instance: " ++ show (Student "Tom" 10)

  students <- readStudentLines numberStudents

  let averageGrade = averageGrades students

  print averageGrade
------------------------------
  print $ in_range 0 3 4
-----------------------------------
  let i = add 3 4
      j = add 4 4

  print $ add 4 4
  print $ 4 `add`  4
  --------------------------------
  putStrLn $ show (Cities "Teste" 10 15 [Cities "eae" 5 13 [Cities "fuck" 5 13 []]])

  let t = Cities {name = "t", lat = 10, long = 5, _NO = []}
  let r = [("re", 6), ("1", 9)]
  putStrLn $ show (r)
------------------------------------
  print $ fac 4
------------------------------------
  print $ asc 1 3
  print $ head [1,2,3]
  print $ tail [1,2,3]
  print $ length [1,2,3]
  print $ init [1,2,3]
  print $ null [1,2,3]
  print $ and [True, False, True]
  print $ or [True, False, True]

  print  [ 2*x | x <- [1,2,3]]
  print  [ 2*x | x <- [1,2,3], x > 1]
  print  [ (x, y) | x <- [1,2,3], y <- ['a', 'b']]
  -----------------------------
  print $ addTuples [(1, 2), (2, 3), (100, 100)]
  -----------------------------
  print $ (\x -> x + 1) 1
  print $ (\x y z -> x + y + z) 1 2 3

  print $ add1 1

  print $ map (\x -> x+1) [1,2,3,4,5]
  print $ map (\(x,y) -> x+y) [(1, 2), (2, 3), (3, 4)]
  print $ filter (\x -> x > 2) [1, 2, 3, 4, 5]
  print $ filter (\(x,y) -> x /= y) [(1, 2), (2, 2)]
  -------------------------------
  print $ x (D2 1 2)
  print $ y (D2 1 2)