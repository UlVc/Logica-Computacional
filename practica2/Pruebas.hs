module Pruebas
 where
import Practica2
p1 = varForm (Or (Var 1) (Neg (And (Var 2) (Var 3))))
p2 = varForm (Imp (And (Var 1) (Var 4)) (Or (Neg(Var 3)) (And (Var 4) (Var 5)))) 
p3 = conjuntoPot [1,2,3]
p4 = conjuntoPot [2,4,6,8,10]
p5 = esValLP (Or(Neg (Var 1)) (Var 1))
p6 = esValLP (Imp (Or (Neg(Var 1)) (Var 3)) (Or (And (Var 4) (Var 5)) (Var 6)))
p7 = esSatLP (And (Neg (Var 1)) (Var 1))
p8 = esSatLP (Imp (Or (Neg(Var 1)) (Var 3)) (Or (And (Var 4) (Var 5)) (Var 6)))
p9 =  quitaImp (Imp (Var 1) (Neg (Var 6)))
p10 = quitaImp (Imp (Imp (Var 1) (Var 2)) (Or (Imp (Var 5) (Var 6)) (Imp (Var 3) (Var 4))))
