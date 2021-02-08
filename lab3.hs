import Data.Char

--L3.1 1
factori :: Int -> [Int]
factori x = [d | d <- [1..x], x `mod` d == 0]

--2
prim :: Int -> Bool
prim x
    | length (factori x) == 2 = True
    | otherwise = False

--3
numerePrime :: Int -> [Int]
numerePrime n = [x | x <- [2..n], prim x == True]

--L3.2
myzip3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip3 l1 l2 l3 = [(x, y, z) | x <- l1, y <- l2, z <- l3, x == y, y == z]

--
--aplica2 :: (a -> a) -> a -> a
--aplica2 f x = f (f x)

--L3.3 1
firstEl :: [(Int, Int)] -> [Int]
firstEl l = map (fst) l

--L3.3 2
sumList :: [[Int]] -> [Int]
sumList l = map (sum) l

--L3.3 3
pre12 :: [Int] -> [Int]
pre12 xs = map f xs
    where
        f :: Int -> Int
        f x =
            if odd x then x*2
            else x `div` 2

--L3.4 1
f1 :: Char -> [[Char]] -> [[Char]]
f1 a l = filter (a `elem`) l

--L3.4 3
f2 :: [Int] -> [Int]
f2 = map ((^2) . snd) . filter ((==1) . (`mod` 2) . fst) . (zip [0..])

--L3.4 2
f3 :: [Int] -> [Int]
f3 = map (^2) . filter (\x -> x `mod` 2 == 1)

--L3.4 4
numaiVocale :: [[Char]] -> [[Char]]
numaiVocale = map (filter (\x -> x `elem` ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O','U']))

--L3.5
mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (h:t) = f h : mymap f t

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter cond (h:t)
    | cond h = h : myfilter cond t
    | otherwise = myfilter cond t












