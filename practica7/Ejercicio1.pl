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

/* path(state(on(c,on(b,on(a,void))), void, void),
        state(void, void, on(c,on(a,on(b,void)))),
        X).
    
    X = [state(on(c, on(b, on(a, void))), void, void), 
         state(on(b, on(a, void)), on(c, void), void), 
         state(on(a, void), on(c, void), on(b, void)), 
         state(void, on(c, void), on(a, on(b, void))), 
         state(void, void, on(c, on(a, on(b, void))))]

Nota: En el pdf de la práctica 7 se pide la lista al revés, ya que creo que es más intuitivo debido a que se empieza desde el inicio (X) y termina hasta llegar a la meta (Y).
*/