struct Fraccion {
    int numerador;
    int denominador;
};

Fraccion consFraccion(int numerador, int denominador);
// PROPÓSITO: Construye una fraccion.
// PRECONDICIÓN: El denominador no es cero.

int numerador(Fraccion f);
// PROPÓSITO: Devuelve el numerador.

int denominador(Fraccion f);
// PROPÓSITO: Devuelve el denominador.

float division(Fraccion f);
// PROPÓSITO: Devuelve el resultado de hacer la división.

Fraccion multF(Fraccion f1, Fraccion f2);
// PROPÓSITO: Devuelve una fracción que resulta de multiplicar las fracciones (sin simplificar).

Fraccion simplificada(Fraccion p);
// PROPÓSITO: Devuelve una fracción que resulta de simplificar la dada por parámetro.

Fraccion sumF(Fraccion f1, Fraccion f2);
// PROPÓSITO: Devuelve la fracción resultante de sumar las fracciones.