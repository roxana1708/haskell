import Data.List ()
import Data.Char 


--2.1
fibonacciLiniar :: Integer -> Integer
fibonacciLiniar 0 = 0
fibonacciLiniar n = snd (fibonaciiPereche n)
    where
       fibonaciiPereche :: Integer -> (Integer, Integer)
       fibonaciiPereche 1 = (0, 1)
       fibonaciiPereche n = (right, left + right)
            where
              (left, right) = fibonaciiPereche (n - 1)


--2.2
inIntervalRec :: Integer -> Integer -> [Integer] -> [Integer]
inIntervalRec a b l
    | null l = l
    | h >= a && h <= b = h : t'
    | otherwise = t'
    where
        h = head l
        t = tail l
        t' = inIntervalRec a b t

inIntervalComp :: Integer -> Integer -> [Integer] -> [Integer]
inIntervalComp a b l = [x | x <- l, x >= a && x<= b]


--2.3
pozitiveRec :: [Int] -> Int
pozitiveRec l
    | null l = 0
    | h > 0 = 1 + t'
    | otherwise = t'
    where
        h = head l
        t = tail l
        t' = pozitiveRec t


pozitiveComp :: [Int] -> Int
pozitiveComp l = length $ map (>0) $ l


--2.4
--pozitiiImpareRec ??

pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp l = [fst x| x <- xs, odd $ snd x]
    where xs = zip [0..length l] l


--2.5
multDigitsRec :: [Char] -> Int
multDigitsRec l
    | null l = 1
    | isDigit h = digitToInt h * t'
    | otherwise = t'
    where
        h = head l
        t = tail l
        t' = multDigitsRec t

multDigitsComp :: [Char] -> Int
multDigitsComp l =  product $ map (digitToInt) $ filter (isDigit) l

--2.6
discountRec :: [Float] -> [Float]
discountRec l 
    | null l = []
    | h - 25 * h / 100 < 200 = h - 25*h/100 : t'
    | otherwise = t'
    where
        h = head l
        t = tail l
        t' = discountRec t

discountComp :: [Float] -> [Float]
discountComp l = [x - 25 * x / 100 | x <- l, x - 25 * x / 100 < 200]
