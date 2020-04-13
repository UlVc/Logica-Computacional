t(nil).
t(tree(X, L, R)):- t(L), t(R). 

ocurrencias(_, nil, 0).
ocurrencias(N, tree(X, nil, nil), 1):- X = N.
ocurrencias(N, tree(X, nil, nil), 0):- X \= N. 
ocurrencias(N, tree(X, L, R), M):-
   ocurrencias(N, tree(X, nil, nil), M1),
   ocurrencias(N, L, M2),
   ocurrencias(N, R, M3),
   M is M1 + M2 + M3.

%ocurrencias(1, tree(1, tree(1, nil, nil), tree(1, tree(1, nil, nil), nil)), X). X = 4.

hojas(nil, 0).
hojas(tree(X, nil, nil), 1).
hojas(tree(X, L, R), N):-
   hojas(L, M1),
   hojas(R, M2),
   N is M1 + M2.

%hojas(tree(1, tree(1, nil, tree(1, nil, nil)), tree(1, tree(1, nil, nil), tree(2, nil, nil))), X). X = 3.