pertenece(Elem, [X|Y], 1) :- 
    X == Elem. 
pertenece(Elem, [X|Y], Pos) :- 
    Pos1 is Pos - 1, 
    pertenece(Elem, Y, Pos1).

prefijo([], L).
prefijo([X|XS], [Y|YS]) :- 
    X == Y, 
    prefijo(XS, YS).

suma_acumulada([], []).
suma_acumulada([X|XS], L) :-
    cabeza(XS, Z), 
    L1 is X + Z, 
    L2 = [X, L1], 
    cola(XS, XS1), 
    suma_acumulada_aux(XS1, L2, L).

suma_acumulada_aux([], LS, LS).
suma_acumulada_aux([X|XS], [L|LS], Res) :-
    ultimo_elemento(LS, Z),
    L1 is X + Z,
    concatenar([L|LS], [L1], A),
    suma_acumulada_aux(XS, A, Res).

nueva_lista([], []).
nueva_lista([X|XS], YS) :-
    apariciones(X, XS, N),
    N1 is N + 1,
    Z = [c(X, N1)],
    eliminar_elemento(X, XS, A),
    nueva_lista_aux(A, Z, YS).

nueva_lista_aux([], ZS, ZS).
nueva_lista_aux([X|XS], ZS, YS) :-
    apariciones(X, XS, N),
    N1 is N + 1,
    concatenar(ZS, [c(X, N1)], A),
    eliminar_elemento(X, XS, B),
    nueva_lista_aux(B, A, YS).

cabeza([], 0).
cabeza([Y|_], Y).

cola([], []).
cola([Y|YS], YS).

ultimo_elemento([Y], Y).
ultimo_elemento([_|XS], Y) :-
    ultimo_elemento(XS, Y).

reversa([X|XS], Y) :- 
    reversa_aux(XS, [X], Y).

reversa_aux([], L, L).
reversa_aux([X|XS], L, Y) :- 
    concatenar([X], L, Z),
    reversa_aux(XS, Z, Y).

concatenar([], X, X).
concatenar(A, B, W) :- 
    reversa(A, [X|XS]),
    W1 = [X|B],
    concatenar_aux(XS, W1, W).

concatenar_aux([], W, W).
concatenar_aux([X|XS], W, Res) :-
    Z = [X|W],
    concatenar_aux(XS, Z, Res). 

pertenece_v2(Elem, [X|Y]) :-
    X == Elem. 
pertenece_v2(Elem, [X|Y]) :-
    pertenece_v2(Elem, Y).

eliminar_elemento(Elem, [], []).
eliminar_elemento(Elem, [X|XS], Y) :- 
    (Elem \= X, Z = [X], 
        eliminar_elemento_aux(Elem, Z, XS, Y)); 
    eliminar_elemento(Elem, XS, Y).

eliminar_elemento_aux(Elem, Z, [], Z).
eliminar_elemento_aux(Elem, Z, [X|XS], Y) :- 
  (Elem \= X, 
    concatenar(Z, [X], A), 
    eliminar_elemento_aux(Elem, A, XS, Y));
  (Elem == X, 
    eliminar_elemento_aux(Elem, Z, XS, Y)).

apariciones(Elem, [], 0).
apariciones(Elem, [L|LS], N):-
    (Elem == L, 
        apariciones(Elem, LS, N1), 
        N is N1 + 1); 
    apariciones(Elem, LS, N).
