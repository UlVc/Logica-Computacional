module Pruebas3b
where
import Practica3

l1 = esCNF (And (And (Or (Var 1) (Var 2)) (Or (Var 3) (Var 4))) (Or (Var 6) (Var 7)))
l2 = esCNF (And (And (Or (Var 35) (Var 43)) (Neg (Var 18))) (And (Neg (Var 64)) (Or (And (Var 13) (Var 29)) (Neg (Neg (Var 15))))))
l3 = esDNF (Or (Or (Neg (Var 56)) (And (Var 14) (Var 72))) (Or (And (Var 19) (Var 0)) (And (Var 92) (Var 1))))
l4 = esDNF (Or (Or (Var 23) (Var 89)) (And (Var 82) (Neg (Neg (Var 14)))))
l5 = satDNF (Or (Or (And (And (Var 1) (Var 4)) (Neg (Var 1))) (And (Var 3) (And (Var 5) (Neg (Var 5))))) (And (Var 6) (Neg (Var 6))))
l6 = satDNF (Or (Or (And (And (Var 1) (Var 4)) (Neg (Var 1))) (And (Var 3) (And (Var 5) (Neg (Var 5))))) (And (Var 6) (Var 8)))
l7 = valCNF (And (And (Or (Var 3) (Var 67)) (Or (Or (Var 17) (Neg (Var 27))) (Var 61))) (And (Or (Var 5) (Or (Var 52) (Neg (Var 3)))) (Or (Var 9) (Var 10))))
l8= valCNF (And (And (Or (Var 3) (Neg (Var 3))) (Or (Or (Var 17) (Neg (Var 17))) (Var 61))) (And (Or (Var 5) (Or (Var 52) (Neg (Var 5)))) (Or (Var 9) (Neg (Var 9)))))
l9 = valCNF (And (And (Or (Var 3) (Neg (Var 3))) (Or (Or (Var 17) (Neg (Var 17))) (Var 61))) (And (Or (Var 5) (Or (Var 52) (Neg (Var 5)))) (Or (Var 9) (Neg (Var 4)))))





