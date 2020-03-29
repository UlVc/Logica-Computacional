module SintaxisPL (PL(..), Indice, showPL)
--Sintaxis de la PL 
--mcb
where
--
--------------------------------------------------------
--
--Tipo de datos para indices de variables
type Indice = Int

-- Tipo de datos para formulas de la PL
data PL = T | F | Var Indice 
        | Neg PL | And PL PL | Or PL PL | Imp PL PL deriving (Eq,Show,Ord,Read)
--
showPL :: PL -> String
-- muestra una formula de la PL
showPL phi = case phi of
    T         -> "TT"
    F         -> "FF"
    Var v       -> "v"++show v
    Neg p      -> "¬"++(showPL p)
    And p q    -> "("++ (showPL p) ++" & "++ (showPL q) ++")"
    Or  p q    -> "("++ (showPL p) ++" | "++ (showPL q) ++")"
    Imp p q    -> "("++ (showPL p) ++" -> "++ (showPL q) ++")" 
    --_           -> error $ "showPL: no definida en este caso, phi="++show phi
--
