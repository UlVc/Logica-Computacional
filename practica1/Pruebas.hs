module Pruebas 
 where
import Practica1
l1 = mayorQue (Suc(Suc(Suc Cero))) (Suc(Suc Cero))
l2 = mayorQue (Suc Cero) (Suc (Suc Cero))
l3 = restaNat (Suc(Suc Cero)) (Suc(Suc(Suc(Suc(Suc Cero)))))
l4 = restaNat (Suc(Suc(Suc(Suc Cero)))) (Suc(Suc(Suc Cero)))
l5 = mulNat (Suc(Suc(Suc Cero))) (Suc(Suc(Suc(Suc(Suc Cero)))))
l6 = mulNat (Suc(Suc(Suc(Suc(Suc(Suc Cero)))))) (Suc Cero)
l7 = concatena (Cons (Suc Cero) Nil) (Cons Cero (Cons (Suc (Suc Cero)) Nil))
l8 = concatena (Cons (Suc(Suc(Suc Cero))) (Cons (Suc Cero) (Cons (Suc(Suc(Suc(Suc Cero)))) Nil))) (Cons Cero (Cons (Suc Cero) Nil))
l9 = reversa (Cons Cero (Cons (Suc (Suc Cero)) (Cons (Suc Cero) Nil)))
l10 = reversa (Cons (Suc(Suc(Suc(Suc Cero)))) (Cons (Suc(Suc Cero)) (Cons (Suc Cero) Nil)))
l11 = perteneceNat (Suc(Suc Cero)) (Cons Cero (Cons (Suc Cero) (Cons (Suc(Suc Cero)) Nil)))
l12 =  perteneceNat (Suc Cero) (Cons Cero Nil)
l13 = inOrden (Node (Node Void 2 Void) 4 (Node(Node Void 6 Void) 7 Void))
l14 = inOrden (Node (Node (Node Void 4 Void) 2 (Node Void 5 Void)) 1 (Node (Node Void 6 Void) 3 (Node Void 7 Void)))
l15 = agregaOrden 5 (Node (Node Void 2 Void) 4 (Node (Node Void 6 Void) 7 Void))
l16 = agregaOrden 8 (Node (Node Void 2 Void) 3 (Node Void 4 Void))
l17 = tailSnoc (Snoc (Snoc (Snoc (Snoc (Snoc Empty 'a') 'b') 'c') 'd') 'e')
l18 = tailSnoc (Snoc (Snoc (Snoc (Snoc Empty 1) 2) 3) 4)
l19 = mapSnoc (*10) (Snoc (Snoc (Snoc (Snoc Empty 1) 2) 3) 4)
l20 = mapSnoc (+25)  (Snoc (Snoc (Snoc Empty 10) 15) 20)


-- Puntos Extra
p1 = longitud 20
p2 = longitud 1997
p3 = tribonaccies 5
p4 = tribonaccies 10


