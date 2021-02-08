type Id = String 
type Link = Maybe String
type Info = Int
data Node = Node Id Info Link
    deriving Show
data NodeSpace = NodeSpace [Node]
    deriving Show


r17grFunctieA :: NodeSpace -> Node -> Link
r17grFunctieA (NodeSpace xs) (Node _ _ Nothing) = Nothing
r17grFunctieA (NodeSpace []) _ = Nothing
r17grFunctieA (NodeSpace xs) (Node a i c)
    | c `elem` x = r17grFuncAux c
    | otherwise = Nothing
    where
        x = [c| (Node a b c) <- xs]
        r17grFuncAux c = [ Just a | (Node a b x) <- xs, x == c] !! 0

test1 = r17grFunctieA [Node "a" 1 (Just "b"), Node "b" 2 (Just "c"), Node "c" 3 (Just "d"), Node "d" 4 Nothing] (Node "a" 1 (Just "b")) == Just "a"
test2 = r17grFunctieA [Node "a" 1 (Just "b"), Node "b" 2 (Just "c"), Node "c" 3 (Just "d"), Node "d" 4 Nothing] (Node "a" 1 (Just "g")) == Nothing
test3 = r17grFunctieA [] (Node "a" 1 (Just "b")) == Nothing

--b
instance Eq NodeSpace where
    NodeSpace [Node a b c] == NodeSpace [Node d e f] = b == e
    NodeSpace xs == NodeSpace ys = [b | Node a b c <- xs] == [b | Node a b c <- ys]

--c
type MGraph = [(Id, [(Info, Id)])]

r17grFunctieC :: [Node] -> MGraph
r17grFunctieC [] = []
r17grFunctieC (Node x y z:xs) = [(x, [(p, b)| Node a p (Just b) <- xs, a == x])] ++ r17grFunctieC xs

--NodeSpace [Node "a" 1 Nothing, Node "b" 2 (Just "a"), Node "a" 3 (Just "c")]
--2
data B e = e :|: e | B e ::: B e
infixr 5 :::
infixr 6 :|:

instance Foldable B where
    foldMap f e = B (f e)

class C e where
  cFilter :: Monoid a => (a -> Bool) -> e a -> e a
  toList :: (Monoid a, Eq a) => e a -> [a]

