%1 Simples

aprobar_examen(pedro) :- 
    estudia(pedro), 
    se_duerme_temprano(pedro); 
    hace_trampa(pedro).

se_casara(daniel, sofia) :- 
    pide_matrimonio(daniel, sofia).

maullan(gatitos) :- 
    comer_croquetas(gatitos);
    estan_alegres(gatitos);
    estan_en_celo(gatitos).

/**
 * No se puede aplciar la regla 1.2.4 porque el color del cielo y el pasto es ajeno a lo que le guste Julián. Tendría que ser algo del estilo:
 * le_gusta_usar_bicileta(julian) :-
 *     cielo_azul(julian),
 *     pasto_verde(julian). 
 * Lo cual no tiene sentido pues no se ha definido alguna relación entre el color del pasto o cielo con Julián.
 */


%2 Recursivas

naturales(cero).
naturales(s(X)) :- 
    naturales(X).  

fib(cero, cero).
fib(s(cero), s(cero)).
fib(s(s(Pos)), N) :-
    fib(s(Pos), N1),
    fib(Pos, N2),
    suma(N1, N2, N).

suma(X, cero, X).
suma(X, s(Y), Z) :- 
    suma(s(X), Y, Z).

potencia(cero, cero, 'indefinido').
potencia(X, cero, s(cero)).
potencia(X, s(Y), Z) :-
    potencia(X, Y, A),
    multiplicacion(X, A, Z).

multiplicacion(X, cero, cero).
multiplicacion(X, s(Y), Z) :-
    multiplicacion(X, Y, A),
    suma(X, A, Z).
