import Numeric.Natural
import Data.Char
--1
--a Scrieti o functie recursivă care calculează produsul numerelor dintr-o listă
produsRec :: [Integer] -> Integer
produsRec [] = 1
produsRec (x:xs) = x * produsRec xs

--b Scrieti o functie echivalentă care foloseste foldr în locul recursiei.
produsFold :: [Integer] -> Integer
produsFold l = foldr (*) 1 l

--2
--a Scrieti o functie recursivă care verifică faptul că toate elementele dintr-o listă sunt True.
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs) 
    | x == False = False
    | otherwise = andRec xs

--b Scrieti o functie echivalentă care foloseste foldr în locul recursiei.
andFold :: [Bool] -> Bool
andFold l = foldr (&&) True l

--3    
--a Scrieti o functe recursivă care concatenează o listă de liste.
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) =  x ++ (concatRec xs)

--b Scrieti o functie echivalentă care foloseste foldr în locul recursiei
concatFold :: [[a]] -> [a]
concatFold l = foldr (++) [] l

c :: [Int] -> [Int]
c l = [x | x <- head l, y <- head tail l, x == y]
--
f :: Char -> Bool
f a = if (toUpper a <= 'M') then True else False

g :: String -> Bool
g str = if length [True| x <- str, f x == True] >= length str `div` 2 then True else False

--h :: String -> Bool
--h "" = True
--h (x:xs) = 
--4
--a Scrieti o functie care elimină un caracter din sir de caractere.
rmChar :: Char -> String -> String
rmChar a str = [x| x <- str, x /= a]

--b Scrieti o functie recursivă care elimină toate caracterele din al doilea argument care se găsesc în primul argument
rmCharsRec :: String -> String -> String
rmCharsRec [] [] = []
rmCharsRec _ [] = []
rmCharsRec [] _ = []
rmCharsRec xs1 (x2:xs2)
    | x2 `notElem` xs1 = x2 : rmCharsRec xs1 xs2
    | otherwise = rmCharsRec xs1 xs2

test_rmchars :: Bool
test_rmchars = rmCharsRec ['m', 'n'] "maine" == "aie"

--c Scrieti o functie echivalentă cu cea de la (b) care foloseste foldr în locul recursiei
rmCharsFold :: String -> String -> String
rmCharsFold xs1 xs2 = foldr (++) [] ([filter (\x -> x `notElem` xs1) xs2])
        

