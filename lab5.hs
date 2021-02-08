--1: verifica faptul că toate listele au aceeasi lungime
corect :: [[a]] -> Bool
corect xs = all (== length (head xs)) (map (length) xs)

--2: obtine elementul de la o anumită pozit, ie din matrice
el :: [[a]] -> Int -> Int -> a
el xs a b = (xs !! a) !! b

--3: obtinem o listă în care fiecare element să aibă asociată pozitia din matrice
transforma :: [[a]] -> [(a, Int, Int)]
transforma xs = [(el xs a b, a, b) | a <- l1, b <- l2]
    where
        l1 = [0..length xs-1]
        l2 = [0..length (head xs)-1]