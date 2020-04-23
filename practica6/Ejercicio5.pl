%5.1:

estado(q0).
estado(q1).
estado(q2).
estado(q3).

estado_inicial(q0).
estado_final(q3).

abecedario(b).
abecedario(c).
abecedario(d).
abecedario(e).
abecedario(a).

delta(q0, a, q1).
delta(q1, b, q2).
delta(q2, c, q1).
delta(q2, d, q3).
delta(q3, c, q0).

aff([X|XS]) :- 
    abecedario(X),
    delta(q0, X, Q),
    aff_aux(XS, Q).

aff_aux([X|[]], Q) :- 
    abecedario(X),
    delta(Q, X, Q1),
    estado_final(Q1).
aff_aux([X|XS], Q) :- 
    abecedario(X),
    delta(Q, X, Q1),
    aff_aux(XS, Q1).

%5.2:

estado_2(1).
estado_2(2).
estado_2(3).

estado_inicial_2(1).
estado_final_2(3).

abecedario_2(a).
abecedario_2(b).

delta_2(1, b, 2).
delta_2(2, a, 2).
delta_2(2, a, 3).
delta_2(3, b, 2).

imprimir_estados(Estados) :- 
    findall(X, (estado_2(X)), Estados).

imprimir_abecedario(Abecedario) :- 
    findall(X, (abecedario_2(X)), Abecedario).

imprimir_estados_iniciales(Estados_Iniciales) :- 
    findall(X, (estado_inicial_2(X)), Estados_Iniciales).

imprimir_estados_finales(Estados_Finales) :- 
    findall(X, (estado_final_2(X)), Estados_Finales).

imprimir_funcion_de_transicion(Delta) :- 
    Delta = [delta_2(1, b, 2), delta_2(2, a, 2), delta_2(2, a, 3), delta_2(3, b, 2)].

informacion(Estados, Abecedario, Estados_Iniciales, Estados_Finales, Delta) :- 
    imprimir_estados(Estados),
    imprimir_abecedario(Abecedario),
    imprimir_estados_iniciales(Estados_Iniciales),
    imprimir_estados_finales(Estados_Finales),
    imprimir_funcion_de_transicion(Delta).

acepta([X|[]], Q, [delta(Q, X, Q1)]) :- 
    abecedario_2(X),
    delta_2(Q, X, Q1),
    estado_final_2(Q1).
acepta([X|XS], Q, [delta(Q, X, Q1)|Meta]) :- 
    abecedario_2(X),
    delta_2(Q, X, Q1),
    acepta(XS, Q1, Meta).
acepta(Cadena, Q) :- 
    informacion(Estados, Abecedario, Estados_Iniciales, Estados_Finales, Delta),
    write('Información del autómata:'), nl, nl,
    write('Estados: '), write(Estados), nl,
    write('Abecedario: '), write(Abecedario), nl,
    write('Estados Iniciales: '), write(Estados_Iniciales), nl,
    write('Estados Finales: '), write(Estados_Finales), nl,
    write('Función de transición: '), write(Delta), nl, nl,
    write('¿Es aceptada la cadena?:'), 
    acepta(Cadena, Q, _).

/** Nota:
 * La función acepta es el programa que describe la información del autómata, 
 * también nos dice que dado un estado y una cadena, es verdadero si se puede llegar al estado 3 leyendo la cadena a partir del estado dado.
 * Y también nos muestra la meta para llegar al estado 3 si es posible. 
 *
 * Para acceder a la meta se usará lo siguiente (en el caso de la cadena baba (5.2.3)): acepta([b,a,b,a], 1, Meta). (La información del autómata no se muestra en éste caso).
 * Si no nos interesa la meta, se usará lo siguiente: acepta(cadena, 1), donde la cadena se debe de descomponer en caracteres en una lista. (La información del autómata sí se muestra).
*/