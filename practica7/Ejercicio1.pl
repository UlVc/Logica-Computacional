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


path(X, X, []).
path(X, Y, List) :-
    path_aux(X, Y, List1),
    List = [X|List1].

path_aux(X, X, []).
path_aux(X, Y, [Z|ZS]):- 
    length([Z|ZS], _),
    move(X, Z), 
    path_aux(Z, Y, ZS).
