/*

La idea es muy sencilla, organizamos los 15 datos que tenemos tal y como se muestra en la función casas y además, es importante añadir
a la persona que bebe agua y a la que tiene la zebra.

Finalmente, se crean las funciones quien_tiene_zebra y quien_bebe_agua, y usando éstas podemos obtener a la persona buscada.

Creo que el programa es bastante intuitivo, así que realmente basta con mirar el código para entender mejor el funcionamiento.

Nota: Cada casa se puede ver como una tupla de 5 elementos, es decir, (Nacionalidad, Color, Bebida, Cigarro, Mascota)

*/

casas_der(Casas) :- % Predicado considerando que la relación "A junto a B" se refiere a que B está a la derecha de A.
    length(Casas, 5),                                                    % 1. Hay 5 casas.
    member((ingles, roja, _, _, _), Casas),                              % 2. El inglés vive en la casa roja.
    member((español,_, _, _, perro), Casas),                             % 3. El español tiene un perro.
    member((_, verde, cafe, _, _), Casas),                               % 4. El café es bebido en la casa verde.
    member((ucraniano, _, te, _, _), Casas),                             % 5. El ucraniano toma té.
    derecha((_, marfil, _, _, _), (_, verde, _, _, _), Casas),           % 6. La casa verde está a la derecha de la casa color marfil.
    member((_, _, _ , old_gold, caracoles), Casas),                      % 7. El que fuma Old Gold tiene caracoles.
    member((_, amarilla, _, kools, _), Casas),                           % 8. Los kools son fumados en la casa amarilla.
    Casas = [_, _, (_, _, leche, _, _), _, _],                           % 9. En la casa central se toma leche .
    Casas = [(noruego, _, _, _, _)|_],                                   % 10. El noruego vive en la primera casa.
    derecha((_, _, _, _, zorro), (_, _, _, chesterfield, _), Casas),     % 11. El que fuma Chesterfields vive en la casa junto a la del hombre con zorro.
    derecha((_, _, _, kools, _), (_, _, _, _, caballo), Casas),          % 12. Los kools son fumados en la casa que está al lado de la casa donde guardan al caballo.
    member((_, _, jugo_naranja, lucky_strike, _), Casas),                % 13. El que fuma los Lucky Strike bebe jugo de naranja.
    member((japones, _, _, parliaments, _), Casas),                      % 14. El japonés fuma de la marca Parliaments
    derecha((noruego, _, _, _, _), (_, azul, _, _, _), Casas),           % 15. El noruego vive en la casa que está junto a la casa azul. 
    member((_, _, agua, _, _), Casas),                                   % Uno de ellos bebe agua.
    member((_, _, _, _, zebra), Casas).                                  % Uno de ellos tiene una zebra.

casas_izq(Casas) :- % Predicado considerando que la relación "A junto a B" se refiere a que B está en la izquierda de A.
    length(Casas, 5),                                                    % 1. Hay 5 casas.
    member((ingles, roja, _, _, _), Casas),                              % 2. El inglés vive en la casa roja.
    member((español,_, _, _, perro), Casas),                             % 3. El español tiene un perro.
    member((_, verde, cafe, _, _), Casas),                               % 4. El café es bebido en la casa verde.
    member((ucraniano, _, te, _, _), Casas),                             % 5. El ucraniano toma té.
    derecha((_, marfil, _, _, _), (_, verde, _, _, _), Casas),           % 6. La casa verde está a la derecha de la casa color marfil.
    member((_, _, _ , old_gold, caracoles), Casas),                      % 7. El que fuma Old Gold tiene caracoles.
    member((_, amarilla, _, kools, _), Casas),                           % 8. Los kools son fumados en la casa amarilla.
    Casas = [_, _, (_, _, leche, _, _), _, _],                           % 9. En la casa central se toma leche .
    Casas = [(noruego, _, _, _, _)|_],                                   % 10. El noruego vive en la primera casa.
    izquierda((_, _, _, _, zorro), (_, _, _, chesterfield, _), Casas),   % 11. El que fuma Chesterfields vive en la casa junto a la del hombre con zorro.
    izquierda((_, _, _, kools, _), (_, _, _, _, caballo), Casas),        % 12. Los kools son fumados en la casa que está al lado de la casa donde guardan al caballo.
    member((_, _, jugo_naranja, lucky_strike, _), Casas),                % 13. El que fuma los Lucky Strike bebe jugo de naranja.
    member((japones, _, _, parliaments, _), Casas),                      % 14. El japonés fuma de la marca Parliaments
    izquierda((noruego, _, _, _, _), (_, azul, _, _, _), Casas),         % 15. El noruego vive en la casa que está junto a la casa azul. 
    member((_, _, agua, _, _), Casas),                                   % Uno de ellos bebe agua.
    member((_, _, _, _, zebra), Casas).                                  % Uno de ellos tiene una zebra.

derecha(A, B, Ls) :- % B está en la derecha de A.
    append(_, [A,B|_], Ls).

izquierda(A, B, Ls) :- % B está en la izquierda de A.
    append(_, [B,A|_], Ls).

quien_tiene_zebra(X) :-
    (casas_der(CsDer); casas_izq(CsIzq)),
    (member((X, _, _, _, zebra), CsDer); member((X, _, _, _, zebra), CsIzq)).

quien_bebe_agua(X) :-
    (casas_der(CsDer); casas_izq(CsIzq)),
    (member((X, _, agua, _, _), CsDer); member((X, _, agua, _, _), CsIzq)).

/* 
   
   Por lo cúal, para saber quién tiene la zebra usamos lo siguiente.
    
   quien_tiene_zebra(X).
   X = japones.

   Y para saber quién bebe agua usamos lo siguiente:

   quien_bebe_agua(X).
   X = noruego.

*/