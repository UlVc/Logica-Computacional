animal(elefante).
animal(jirafa).

es_animal(X) :- animal(X).

persona(juan).
persona(vianey).
persona(armando).
persona(fernando).
persona(jose).
persona(yael).

amigo(juan, vianey).
amigo(armando, fernando).
amigo(josé, yael).

materia(logica).
materia(inteligencia_artificial).
materia(modelado_y_programacion).

alumno(diana).
alumno(carlos).
alumno(abraham).

aprobo(abraham, inteligencia_artificial).
aprobo(diana, modelado_y_programacion).
reprobo(carlos, logica).