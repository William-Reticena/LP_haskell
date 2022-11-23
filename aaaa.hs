isMember n [] = False
isMember n (x: xs) | n == x = True | otherwise = isMember n xs
lenght [] = 0
lenght (n :xs) = 1 + lenght xs
soma [] = 0
soma (n: xs) = n + soma xs
prod [] = 1
prod (n: xs) = n * prod xs
myrevert :: [int] -> [int]
myrevert [] = []
myrevert (x: xs) = myrevert(xs) ++ (x: [])
juntar:: [int] -> [int] -> [int]
juntar xs ys = (xs ++ ys)

list3 = [0];
insertList :: [Int] -> Int -> [Int]
insertList list xs = list ++ [xs]


main :: IO ()
main = do
    let list = [1, 2, 3, 4, 5, 6, 7, 8, 9];
    let list2 = [1, 2, 3, 4, 5, 6, 7, 8, 9];
    let aux = insertList list3 1
    print aux
    let aux2 = insertList aux 2
    print aux2
    print $ insertList list3 1
    print $ insertList list3 2
    print $ list3
    
    

