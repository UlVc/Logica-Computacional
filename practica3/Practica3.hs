module Practica3 where

import Data.List  

type Indice = Int

data LP = T 
    | F 
    | Var Indice
    | Neg LP
    | And LP LP 
    | Or LP LP
    | Imp LP LP deriving (Eq, Show)

varForm :: LP -> [Indice]
varForm phi = case phi of Var q -> [q]
                          Neg alpha -> map (\x -> -x) $ varForm alpha
                          And alpha beta -> varForm alpha `union` varForm beta
                          Or alpha beta -> varForm alpha `union` varForm beta
                          Imp alpha beta -> varForm alpha `union` varForm beta
                          _ -> []

clausulasDNF :: LP -> [[Indice]]
clausulasDNF phi = case phi of Or alpha beta -> clausulasDNF alpha `union` clausulasDNF beta
                               And alpha beta -> [varForm alpha `union` varForm beta]

clausulasCNF :: LP -> [[Indice]]
clausulasCNF phi = case phi of And alpha beta -> clausulasCNF alpha `union` clausulasCNF beta
                               Or alpha beta -> [varForm alpha `union` varForm beta]

buscaNegAux :: [Indice] -> Bool
buscaNegAux [] = False
buscaNegAux (x:xs)
    | -x `elem` xs = True
    | otherwise = buscaNegAux xs

buscaNeg :: [[Indice]] -> [Bool]
buscaNeg [] = []
buscaNeg (x:xs) = [buscaNegAux x] ++ buscaNeg xs
      
quitaImp:: LP -> LP
quitaImp p = case p of T -> T
                       F -> F
                       Var q -> Var q
                       Neg q -> Neg $ quitaImp q
                       And alpha beta -> And (quitaImp alpha) (quitaImp beta)
                       Or alpha beta -> Or (quitaImp alpha) (quitaImp beta)
                       Imp alpha beta -> Or (Neg (quitaImp alpha)) ((quitaImp beta))
-- Ya la implementaron en su práctica 2

{-
Las 3 siguientes funciones son funciones auxiliares que deben implementar.
La función negación hace que las negaciones solo afecten a proposiciones atómicas. 
La función distrCNF aplica la sig. distrivutividad: si tienen (p ^ q) v r = (p v r) ^ (q v r) 
La función distrDNF aplica la sig. distrivutividad: si tienen (p v q) ^ r = (p ^ r) v (q ^ r)
Ojo! En ambas distributividades deben considerar otro caso.  
-}
negacion :: LP -> LP
negacion n = case n of Var x -> Var x
                       T -> n
                       F -> n
                       Imp x y -> Imp (negacion x) $ negacion y
                       And x y -> And (negacion x) $ negacion y
                       Or x y -> Or (negacion x) $ negacion y
                       Neg x -> case x of Var p -> Neg $ Var p
                                          And p q -> Or (negacion $ Neg p) $ negacion $ Neg q
                                          Or p q -> And (negacion $ Neg p) $ negacion $ Neg q
                                          Neg n -> negacion n
                                          _ -> Neg x

distrCNF :: LP -> LP
distrCNF n = case n of T -> T
                       F -> F
                       Var x -> Var x
                       Imp x y -> Imp (distrCNF x) $ distrCNF y
                       And x y -> And (distrCNF x) $ distrCNF y
                       Neg x -> Neg $ distrCNF x
                       Or (And p q) r -> distrCNF $ And (Or (distrCNF p) (distrCNF r)) (Or (distrCNF q) (distrCNF r))
                       Or r (And p q) -> distrCNF $ And (Or (distrCNF r) (distrCNF p)) (Or (distrCNF r) (distrCNF q))
                       Or x y -> Or (distrCNF x) $ distrCNF y
                       
distrDNF :: LP -> LP
distrDNF n = case n of T -> T
                       F -> F
                       Var x -> Var x
                       Imp x y -> Imp (distrDNF x) $ distrDNF y
                       Or x y -> Or (distrDNF x) $ distrDNF y
                       Neg x -> Neg $ distrDNF x
                       And (Or p q) r -> distrDNF $ Or (And (distrDNF p) (distrDNF r)) (And (distrDNF q) (distrDNF r))
                       And r (Or p q) -> distrDNF $ Or (And (distrDNF r) (distrDNF p)) (And (distrDNF r) (distrDNF q))
                       And x y -> And (distrDNF x) $ distrDNF y

fnn :: LP -> LP
fnn p = negacion $ quitaImp p
-- Utilizan la función quitaImp y negacion. Primero utilizan quitaImp y al resultado le van a aplicar la función negacion


{-
La función que le pasamos a fnc ya NO TIENE implicaciones (ya que suponemos que ya está en fnn la fórmula que nos pasen). Entonces no deben considerar ese caso
-}
fnc :: LP -> LP
fnc phi = distrCNF phi


{-
La función que le pasamos a fnd ya NO TIENE implicaciones (ya que suponemos que ya está en fnn la fórmula que nos pasen). Entonces no deben considerar ese caso
-}
fnd :: LP -> LP
fnd phi = distrDNF phi

valCNF :: LP -> Bool
valCNF phi = (True `elem` l) && (length l == 1)
    where l = nub $ buscaNeg $ clausulasCNF phi

satDNF :: LP -> Bool
satDNF phi = True `elem` l
    where l = map not $ buscaNeg $ clausulasDNF phi