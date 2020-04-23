pot(X, 0, 1) :- X =\= 0. 
pot(X, Y, Z) :-
    Y < 0, !, (Y1 is (-1)*Y, 
        pot(X, Y1, A), 
        Z is 1/A);
    Y1 is Y - 1,
    pot(X, Y1, A),
    Z is X * A.

fact(X, Y) :-
    X =< 0, !, Y is 1;
    X1 is X - 1,
    fact(X1, Y1),
    Y is X * Y1.