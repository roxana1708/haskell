import Data.Char
import Data.List

-- 1.
rotate :: Int -> [Char] -> [Char]
rotate a xs
    | a < 0 = error "nr negativ"
    | a > length xs -1 = error "nr mai mare decat lungimea sirului"
    | otherwise = [x | x <- [xs!!a..xs!!(length xs -1)]] ++ [y | y <- [xs!!0..xs!!(a-1)]]

-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- 3.
makeKey :: Int -> [(Char, Char)]
makeKey = undefined

-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp = undefined

-- 5.
encipher :: Int -> Char -> Char
encipher = undefined

-- 6.
normalize :: String -> String
normalize = undefined

-- 7.
encipherStr :: Int -> String -> String
encipherStr = undefined