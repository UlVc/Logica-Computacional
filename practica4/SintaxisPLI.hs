module SintaxisPLI
  where

-- Tipo de dato indice
type Indice = Int

-- Tipo de dato fórmula
data PLI = F | Var Indice | Imp PLI PLI 
        deriving (Eq,Show,Ord,Read)

-- Top en el sistema L
oT :: PLI
oT = Imp F F

-- Negación en el sistema L
oNeg :: PLI -> PLI
oNeg phi = Imp phi F

-- Disyunción en el sistema L
oAnd :: PLI -> PLI -> PLI
oAnd alpha beta = (alpha `Imp` (beta `Imp` F)) `Imp` F 

-- Conjunción en el sistema L
oOr :: PLI -> PLI -> PLI
oOr alpha beta = (alpha `Imp` F) `Imp` beta

-- Función que nos muestas los elmentos de la PLI
showPLI :: PLI -> String
showPLI phi = case phi of
  F                            -> "FF" 
  Var v                          -> "v"++show v
  -- toLuk(Top)= ~Bot
  Imp F F                   -> "TT"
  -- toLuk(f ^ g) = toLuk(¬(f -> ¬g))
  (a`Imp`(b`Imp`F))`Imp`F -> "("++ (showPLI a) ++" & "++ (showPLI b) ++")"
   -- toLuk(f v g) = toLuk((¬f) -> g)
  (alpha `Imp` F) `Imp` beta -> "("++ (showPLI alpha) ++" | "++ (showPLI beta) ++")"
  -- toLuk(¬f) = ~toLuk(f)
  Imp alpha F                 -> "~("++ (showPLI alpha) ++")"
  Imp alpha beta                -> "("++ (showPLI alpha) ++" -> "++ (showPLI beta) ++")"
