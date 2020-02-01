module Practica1 where

  data Natural = Cero | Suc Natural deriving Show
  data ListaNat = Nil | Cons Natural ListaNat deriving Show
  data BTree a = Void | Node (BTree a) a (BTree a) deriving Show
  data ListaSnoc a = Empty | Snoc (ListaSnoc a) a deriving Show

  mayorQue :: Natural -> Natural -> Bool
  mayorQue a b = case a of (Suc n) -> case b of Cero -> True
                                                (Suc m) -> mayorQue n m
                           Cero -> False

  iguales :: Natural -> Natural -> Bool
  iguales a b
      | (mayorQue a b == True) = False
      | (mayorQue b a == True) = False
      | otherwise = True

  restaNat :: Natural -> Natural -> Natural
  restaNat a b
      | (mayorQue a b == True) = case b of (Suc n) -> case a of (Suc m) -> restaNat m n
                                           _ -> a
      | (iguales a b) = Cero
      | otherwise = restaNat b a

  sumaNat :: Natural -> Natural -> Natural
  sumaNat a b = case b of (Suc n) -> sumaNat (Suc a) n
                          _ -> a

  mulNat :: Natural -> Natural -> Natural
  mulNat a b = mul a (restaNat b (Suc Cero)) a

  mul :: Natural -> Natural -> Natural -> Natural
  mul a b c = case b of (Suc n) -> mul (sumaNat a c) n c
                        otherwise -> a

  reversa :: ListaNat -> ListaNat
  reversa l = reversaAux l Nil

  reversaAux :: ListaNat -> ListaNat -> ListaNat
  reversaAux l1 l2 = case l1 of (Cons n x) -> reversaAux x (Cons n l2)
                                _ -> l2
 
  concatena :: ListaNat -> ListaNat -> ListaNat
  concatena l1 l2 = case l2 of (Cons n x) -> concatena (Cons n l1) x
                               _ -> l1

  perteneceNat :: Natural -> ListaNat-> Bool
  perteneceNat n l = case l of (Cons m x) -> if (iguales n m) then True else perteneceNat n x 
                               _ -> False
 
  inOrden :: BTree a -> [a]
  inOrden b = case b of Void -> []
                        Node Void e Void -> [e]
                        Node x e y -> inOrden x ++ [e] ++ inOrden y

  agregaOrden :: (Ord a) => a -> BTree a -> BTree a
  agregaOrden e a = case a of Void -> Node Void e Void
                              Node x el y -> if (e >= el) then Node x el (agregaOrden e y) else Node (agregaOrden e x) el y

  tailSnoc :: ListaSnoc a -> ListaSnoc a
  tailSnoc l = case l of Empty -> error "No se permiten listas vacias."
                         Snoc Empty y -> Empty
                         Snoc x y -> Snoc (tailSnoc x) y

  mapSnoc :: (a -> b) -> ListaSnoc a -> ListaSnoc b
  mapSnoc f l = case l of Snoc Empty y -> Snoc Empty $ f y
                          Snoc x y -> Snoc (mapSnoc f x) $ f y

  longitud :: Int -> Int
  longitud x = longAux x 10 1

  longAux :: (Ord a, Integral a) => a -> a -> a -> a
  longAux x d c
      | div x d >= 1 = longAux x (d*10) (c + 1)
      | otherwise = c

  tribonaccies :: Int -> [Int]
  tribonaccies n = map (tribonacciAux) ([0..n+1])

  tribonacciAux :: Int -> Int
  tribonacciAux n = case n of 0 -> 0; 1 -> 0; 2 -> 1; otherwise -> tribonacciAux(n-1) + tribonacciAux(n-2) + tribonacciAux(n-3)

