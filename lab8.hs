import Data.List
import Data.Maybe

type Nume = String 
data Prop
    = Var Nume
    |F
    |T
    | Not Prop
    | Prop :|: Prop 
    | Prop :&: Prop 
    deriving Eq
infixr 2 :|: 
infixr 3 :&:

par :: String -> String
par s = "(" ++ s ++ ")"

instance Show Prop where
    show (Var x) = x
    show F = "F"
    show T = "T"
    show (Not p) = par ("~" ++ show p)
    show (p :|: q) = par (show p ++ "|" ++ show q)
    show (p :&: q) = par (show p ++ "&" ++ show q)

test_ShowProp :: Bool
test_ShowProp = show (Not (Var "P") :&: Var "Q") == "((~P)&Q)"


p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q"))

p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :&: Not (Var "R")))

type Env = [(Nume, Bool)]

impureLookup :: Eq a => a -> [(a, b)] -> b
impureLookup a = fromJust . lookup a


--impl :: Bool -> Bool -> Bool
--impl False _ = True
--impl _ x = x

--echiv :: Bool -> Bool -> Bool
--echiv x y = x==y

eval :: Prop -> Env -> Bool
eval (Var x) e = impureLookup x e
eval F _ = False
eval T _ = True
eval (Not p) e = not (eval p e)
eval (p :|: q) e = eval p e || eval q e
eval (p :&: q) e = eval p e && eval q e
--eval (p :->: q) e = eval p e `impl` eval q e
--eval (p :<->: q) e = eval p e `echiv` eval q e

test_eval = eval  (Var "P" :|: Var "Q") [("P", True), ("Q", False)]

variabile :: Prop -> [Nume]
variabile (Var x) = [x]
variabile (Not p) = nub $ variabile p
variabile (p :|: q) = nub $ variabile p ++ variabile q
variabile (p :&: q) = nub $ variabile p ++ variabile q
variabile _ = []

test_variabile = variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]

envs :: [Nume] -> [[(Nume, Bool)]]
envs [] = []
envs [x] = [[(x, False)], [(x, True)]]
envs (str:xs) = let r = envs xs in map (\x -> (str, False):x) r ++ map (\x -> (str, True):x) r

satisfiabila :: Prop -> Bool
satisfiabila expr = or $ map (eval expr) $ envs $ variabile expr

test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q") == True
test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P") == False

valida :: Prop -> Bool
valida expr = and $ map (eval expr) $ envs $ variabile expr

test_valida1 = valida (Not (Var "P") :&: Var "Q") == False
test_valida2 = valida (Not (Var "P") :|: Var "P") == True 


show_Bool :: Bool -> String
show_Bool False = "F"
show_Bool True = "T"

tabelAdev :: Prop -> String
tabelAdev p = concat $ map (++ "\n") tabel
    where
        vars = variabile p
        afis_prima = concat $ (map (++ " ") vars) ++ [show p]
        evaluari = envs vars
        aux_af tv = (show_Bool tv) ++ " "
        afis_evaluare ev = concat $ (map aux_af [snd p | p <- ev]) ++ [show_Bool (eval p ev)]
        tabel = afis_prima : (map afis_evaluare evaluari)

