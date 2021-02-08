f21gr :: [[Char]] -> Bool
f21gr [] = False
f21gr (x:xs)
    | x == reverse x = True
    | otherwise = f21gr xs

f22gr :: [[Char]] -> Bool
f22gr [] = False
f22gr xs = 1 <= length [x | x <- xs, x == reverse x]

f23gr :: [[Char]] -> Bool
f23gr [] = False
f23gr xs = 1 <= length (filter (\x -> x == reverse x) xs)

f24gr :: [[Char]] -> Bool
f24gr xs = f21gr xs == f23gr xs


f31gr :: (Eq a) => [a] -> Int
f31gr [] = 0
f31gr (x:xs) = f32gr (x:xs) 0
    where
        f32gr :: (Eq a) => [a] -> Int -> Int
        f32gr [] _ = 0
        f32gr (x: xs) count 
            | length (f33gr x xs) >= 1 = f32gr xs count+1
            | otherwise = f32gr xs count
            where
                f33gr x xs = [y | y <- xs, y == x]

 