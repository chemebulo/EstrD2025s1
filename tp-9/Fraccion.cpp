#include "Fraccion.h"
using namespace std;

// EJERCICIO 5:

// EJERCICIO 5.1:
Fraccion consFraccion(int numerador, int denominador){
// PROPÓSITO: Construye una fraccion.
// PRECONDICIÓN: El denominador no es cero.
    Fraccion f;
    f.numerador = numerador;
    f.denominador = denominador;
    return f;
}


// EJERCICIO 5.2:
int numerador(Fraccion f){
// PROPÓSITO: Devuelve el numerador.
    return f.numerador;
}


// EJERCICIO 5.3:
int denominador(Fraccion f){
// PROPÓSITO: Devuelve el denominador.
    return f.denominador;
}


// EJERCICIO 5.4:
float division(Fraccion f){
// PROPÓSITO: Devuelve el resultado de hacer la división.
    return (float)f.numerador / f.denominador;
}


// EJERCICIO 5.5:
Fraccion multF(Fraccion f1, Fraccion f2){
// PROPÓSITO: Devuelve una fracción que resulta de multiplicar las fracciones (sin simplificar).
    return consFraccion(f1.numerador * f2.numerador, f1.denominador * f2.denominador);
}


// EJERCICIO 5.6:
int valorAbsoluto(int n) {
// PROPÓSITO: Devuelve el valor absoluto del número dado.
    if(n < 0) { return -n; } 
         else { return n;  }
}

int mcd(int a, int b) {
    if (b == 0) return a;
    return mcd(b, a % b);
}

Fraccion simplificada(Fraccion p){
// PROPÓSITO: Devuelve una fracción que resulta de simplificar la dada por parámetro.
    int divisor = mcd(valorAbsoluto(p.numerador), valorAbsoluto(p.denominador));
    Fraccion s;
    s.numerador = p.numerador / divisor;
    s.denominador = p.denominador / divisor;
    return s;
}


// EJERCICIO 5.7:
int mcm(int a, int b) {
    return (a * b) / mcd(a, b);
}

Fraccion sumF(Fraccion f1, Fraccion f2){
// PROPÓSITO: Devuelve la fracción resultante de sumar las fracciones.
    int denominadorComun = mcm(f1.denominador, f2.denominador);

    int numeradorResultado = f1.numerador * (denominadorComun / f1.denominador) +
                             f2.numerador * (denominadorComun / f2.denominador);

    return consFraccion(numeradorResultado, denominadorComun);
}