module Pruebas3 
where
import Practica3

f1 = fnn (Imp (And (Var 1) (Imp (Var 2) (Var 3))) (Imp (Or (Var 3) (Var 1)) (Var 2)))
f2 = fnn (Imp (Imp (Imp (Var 10) (Var 11)) (Imp (Var 12) (Var 13))) (Imp (Imp (Var 14) (Var 15)) (Imp (Var 16) (Var 17))))
f3 = fnc (Or (And (Or (And (Var 1) (Var 2)) (Var 3)) (Or (And (Var 4) (Var 5)) (Var 6))) (Var 7))
f4 = fnc (And (And (Neg (Var 5)) (Var 23)) (Or (Neg (Var 1)) (And (Or (Var 3) (Var 6)) (Var 8))))
f5 = fnd (And (Or (And (Or (Var 1) (Var 2)) (Var 3)) (And (Or (Var 4) (Var 5)) (Var 6))) (Var 7))
f6 = fnd (Or (Or (Neg (Var 5)) (Var 23)) (And (Neg (Var 1)) (Or (And (Var 3) (Var 6)) (Var 8))))

