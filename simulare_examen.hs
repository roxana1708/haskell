data Arbore a
    = Nod (Arbore a) a (Arbore a)
    | Frunza a
    | Empty

--1a
fcheck1 :: Ord a => [a] -> Bool
fcheck1 xs = all (\(a,b) -> a < b) $ zip xs (tail xs)

fcheck2 :: Arbore a -> [a]
fcheck2 (Empty) = []
fcheck2 (Frunza a) = [a]
fcheck2 (Nod aSt a aDr) = fcheck2 aSt ++ [a] ++ fcheck2 aDr

fcheck :: Ord a => Arbore a -> Bool
fcheck t = fcheck1 (fcheck2 t)

--1b
finsert :: Ord a => a -> Arbore a -> Arbore a
finsert v (Empty) = Nod Empty v Empty
finsert v (Frunza f)
    | v < f = Nod (Frunza v) f (Empty)
    | v > f = Nod (Empty) f (Frunza v)
    | otherwise = error "Val dupl"
finsert v (Nod l a r)
    | v < a = Nod (finsert v l) a r
    | v > a = Nod l a (finsert v r)
    | otherwise = error "Val dupl"

--1c
instance Functor Arbore where
    fmap _ (Empty) = Empty
    fmap f (Frunza a) = Frunza (f a)
    fmap f (Nod l a r) = Nod (fmap f l) (f a) (fmap f r)


--2a
instance Foldable Arbore where
    foldMap _ (Empty) = mempty
    foldMap f (Frunza a) = f a
    foldMap f (Nod l v r) = f v `mappend` (foldMap f l) `mappend` (foldMap f r)


--2b
instance Show a => Show (Arbore a) where
    show (Empty) = []
    show (Frunza a) = show (a)
    show (Nod l v r) = show l ++ show (v) ++ show r