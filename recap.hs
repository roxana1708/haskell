import Data.List
import Data.Char



--2
--a
fjumalf :: Char -> Bool
fjumalf x
    | 'a' <= toLower x && toLower x <= 'm' = True
    | 'n' <= toLower x && toLower x <= 'z' = False
    | otherwise = error "nu este litera"

fjumnr :: [Char] -> Bool
fjumnr xs = length len > length xs `div` 2 
    where
        len = filter (\x -> fjumalf x) xs

fjumnrrec :: [Char] -> Bool
fjumnrrec (x:xs) = length (helper (x:xs)) > length xs `div` 2
    where
        helper (x:xs)
            | helper x == True = x : helper xs
            | otherwise = helper xs

--3
frepet :: (Eq a) => [a] -> [a]
frepet [] = []
frepet [x] = []
frepet xs = [xs !! i | i <- [1..length xs-1], xs!!i == xs!!(i-1)]

frepet' :: (Eq a) => [a] -> [a]
frepet' [] = []
frepet' [x] = []
frepet' (x:y:xs)
    | x == y = x : frepet' (y:xs)
    | otherwise = frepet' (y:xs)

prop :: (Eq a) => [a] -> Bool
prop l = frepet l == frepet' l


--1
myLast :: [Int] -> Int
myLast xs = xs !! (length xs -1)

--2
myButLast :: [Char] -> Char
myButLast [] = error "Empty list"
myButLast [x] = error "Too few elements"
myButLast xs = xs !! (length xs - 2) 

--3
elementAt :: [a] -> Int -> a
elementAt xs k = xs !! (k-1)

--4
myLength :: [a] -> Int
--myLenght [] = 0
--myLength xs = length xs
myLength [] = 0
myLength [x] = 1
myLength (x:xs) = 1 + myLength xs

--5
myReverse :: [a] -> [a]
myReverse [] = []
--myReverse xs = reverse xs
myReverse (x:xs) = myReverse xs ++ [x]

--6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = error "empty list"
isPalindrome xs = xs == (reverse xs)

--7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

--8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
    | x /= y = [x] ++ compress (y:xs)
    | otherwise = compress (y:xs)

--9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

--10
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode xs = [(length x, head x) | x <- pack xs]

data ListItem a = Single a | Multiple Int a
--12
decodeModified :: [ListItem a] -> [a]
decodeModified [] = []
decodeModified ((Single x) : xs) = x : decodeModified xs
decodeModified ((Multiple 2 x) : xs) = x : x : decodeModified xs
decodeModified ((Multiple n x) : xs) = x : decodeModified( Multiple (n-1) x :xs)

--13
--14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

--15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli xs 2 = dupli xs
repli (x:xs) n = replicate n x ++ repli xs n

--16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = (take (n-1) xs) ++ dropEvery (drop n xs) n

--17
split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

--18
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs st end = take (end-st+1) (drop (st-1) xs) 

--19
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs n 
    | n >= 0 = drop n (xs ++ take n xs)
    | n < 0 = drop len xs ++ take len xs
    where
        len = n + length xs

--20
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs!!(n-1), take (n-1) xs ++ drop n xs)

--21
insertAt :: a -> [a] -> Int -> [a]
insertAt _ [] _ = []
insertAt x xs poz = take (poz-1) xs ++ [x] ++ drop (poz-1) xs

--22
range :: Int -> Int -> [Int]
range 0 0 = []
range x y 
    | x > y = []
    | x <= y = [x..y]

--23
--rndSelect :: [a] -> Int -> [a]
--rndSelect [] _ = []
--rndSelect xs n = 

--24


--25


--26


--27


--28
--a
lsort :: [[a]] -> [[a]]
lsort [] = []
--lsort xs = 

--11
encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified [] = []
encodeModified xs = [b| x <- pack xs, let b = if length x > 1 then Multiple (length x) (head x) else Single (head x)]

