module Practica2 where

  import Data.List  

  type Indice = Int
  type Modelo = [Indice]

  data LP = T 
      | F 
      | Var Indice
      | Neg LP
      | And LP LP 
      | Or LP LP
      | Imp LP LP 
      deriving (Eq, Show)
  
  satMod :: Modelo -> LP -> Bool
  satMod m phi = case phi of T -> True
                             F -> False
                             Var n -> elem n m
                             Neg alpha -> not(satMod m alpha)
                             And alpha beta -> (satMod m alpha) && (satMod m beta)
                             Or alpha beta -> (satMod m alpha) || (satMod m beta)
                             Imp alpha beta -> not(satMod m alpha) || (satMod m beta)

  varForm :: LP -> [Indice]
  varForm phi = case phi of Var q -> [q]
                            Neg alpha -> varForm alpha
                            And alpha beta -> varForm alpha `union` varForm beta
                            Or alpha beta -> varForm alpha `union` varForm beta
                            Imp alpha beta -> varForm alpha `union` varForm beta
                            _ -> []

  conjuntoPot :: [t] -> [[t]]
  conjuntoPot [] = [[]]
  conjuntoPot (x:xs) = conjuntoPot xs ++ map (x:) (conjuntoPot xs)

  quitaImp :: LP -> LP
  quitaImp p = case p of T -> T
                         F -> F
                         Var q -> Var q
                         Neg q -> Neg $ quitaImp q
                         And alpha beta -> And (quitaImp alpha) (quitaImp beta)
                         Or alpha beta -> Or (quitaImp alpha) (quitaImp beta)
                         Imp alpha beta -> Or (Neg (quitaImp alpha)) ((quitaImp beta))
  
  esValLP :: LP -> Bool
  esValLP phi = (True `elem` val) && (length val == 1)
       where
         m = conjuntoPot $ varForm phi
         val = nub $ tabla_de_verdad m phi
  
  esSatLP :: LP -> Bool
  esSatLP phi = True `elem` val
       where
         m = conjuntoPot $ varForm phi
         val = nub $ tabla_de_verdad m phi

  tabla_de_verdad :: [Modelo] -> LP -> [Bool]
  tabla_de_verdad m phi = case m of x:xs -> [satMod x phi] ++ tabla_de_verdad xs phi
                                    [] -> []