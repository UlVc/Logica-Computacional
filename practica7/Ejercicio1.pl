bloque(void).
bloque(on(X, Y)) :- bloque(Y).

imprimir_bloque(void) :- write('void'), nl.
imprimir_bloque(on(X, Y)) :-
    write('---'), nl,
    write('|'), write(X), write('|'), nl,
    write('---'), nl,
    imprimir_bloque(Y).

state(X, Y, Z) :-
    write('Primera posición: '), nl,
    imprimir_bloque(X),
    write('Segunda posición: '), nl,
    imprimir_bloque(Y),
    write('Tercera posición: '), nl,
    imprimir_bloque(Z).

move(state(on(X, NewX), OldY, Z), state(NewX, on(X, OldY), Z)).
move(state(on(X, NewX), Y, OldZ), state(NewX, Y, on(X, OldZ))).

move(state(OldX, on(Y, NewY), Z), state(on(Y, OldX), NewY, Z)).
move(state(X, on(Y, NewY), OldZ), state(X, NewY, on(Y, OldZ))).

move(state(OldX, Y, on(Z, NewZ)), state(on(Z, OldX), Y, NewZ)).
move(state(X, OldY, on(Z, NewZ)), state(X, on(Z, OldY), NewZ)).

path(state(X, Y, Z), state(X, Y, Z)).
path(state(X, Y, Z), state(X1, Y1, Z1)) :-
    findall(A, move(state(X, Y, Z), A), [L|LS]),
    path_aux(state(X, Y, Z), state(X1, Y1, Z1), [L|LS], LS, L).

path_aux(state(X,Y,Z), state(X1, Y1, Z1), [C|CS], [L|LS], state(X1, Y1, Z1)).
path_aux(state(X,Y,Z), state(X1, Y1, Z1), [C|CS], [L|LS], state(X,Y,Z)) :-
    head(LS, Head),
    path_aux(state(X,Y,Z), state(X1, Y1, Z1), CS, LS, Head).
path_aux(state(X,Y,Z), state(X1, Y1, Z1), [C|CS], [L|LS], state(XM, YM, ZM)) :-
    path_aux(state(X,Y,Z), state(X1, Y1, Z1), [C|CS], LS, L).
path_aux(state(X,Y,Z), state(X1, Y1, Z1), [C|CS], [], state(XM, YM, ZM)) :-
    encontrar_caminos([C|CS], [N|NS]),
    path_aux(state(X,Y,Z), state(X1, Y1, Z1), [N|NS], NS, N).

encontrar_caminos([], []).
encontrar_caminos([C|CS], XS) :-
    maplist(encontrar, [C|CS], [Z|ZS]),
    flatten([Z|ZS], XS).

encontrar(X, List) :-
    findall(A, move(C, A), List).

%path(state(on(c,on(b,on(a,void))), void, void), state(void, void, on(c,on(a,on(b,void))))).