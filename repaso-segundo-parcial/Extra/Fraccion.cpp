#include <iostream>
#include "assert.h"
#include "Fraccion.h"
using namespace std;

// #################################################################################################################### \\

Fraccion fraccion(int x, int y){
// PROPÓSITO: Describe una fracción con el númerador y el denominador dado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    assert(y != 0); // Verifica que el denominador no es 0 para garantizar el invariante.
    FraccionStr* fraccion = new FraccionStr;
    fraccion->numerador = x;
    fraccion->denominador = y;
    return fraccion;
}

Fraccion sumar(Fraccion f1, Fraccion f2){
// PROPÓSITO: Describe la suma de las dos fracciones dadas sin simplificarlas.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    FraccionStr* resultante = new FraccionStr;
    resultante->numerador = (f1->numerador * f2->denominador) + (f2->numerador * f1->denominador);
    resultante->denominador = f1->denominador * f2->denominador;
    return resultante;
}

Fraccion restar(Fraccion f1, Fraccion f2){
// PROPÓSITO: Describe la resta de las dos fracciones dadas sin simplificarlas.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    FraccionStr* resultante = new FraccionStr;
    resultante->numerador = (f1->numerador * f2->denominador) - (f2->numerador * f1->denominador);
    resultante->denominador = f1->denominador * f2->denominador;
    return resultante;
}

Fraccion multiplicar(Fraccion f1, Fraccion f2){
// PROPÓSITO: Describe la multiplicación de las dos fracciones dadas sin simplificarlas.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    FraccionStr* resultante = new FraccionStr;
    resultante->numerador = f1->numerador * f2->numerador;
    resultante->denominador = f1->denominador * f2->denominador;
    return resultante;
}

Fraccion dividir(Fraccion f1, Fraccion f2){
// PROPÓSITO: Describe la división de las dos fracciones dadas sin simplificarlas.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    FraccionStr* resultante = new FraccionStr;
    resultante->numerador = f1->numerador * f2->denominador;
    resultante->denominador = f1->denominador * f2->numerador;
    return resultante;
}

void invertir(Fraccion f1){
// PROPÓSITO: Invierte la fracción dada.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    int temp = f1->numerador;
    f1->numerador = f1->denominador;
    f1->denominador = temp;
}

int mcd(int a, int b){
// PROPÓSITO: Describe el múltiplo común divisor de los numeros dados.
// COSTO TIEMPO: O(log N), siendo N = min(numerador, denominador).
// COSTO ESPACIO: O(1).
    int resto;
    while(b != 0){
        resto = a % b;
        a = b;
        b = resto;
    }
    return a;
}

void simplificar(Fraccion f1){
// PROPÓSITO: Simplifica la fracción dada.
// COSTO TIEMPO: O(log N), siendo N = min(numerador, denominador).
// COSTO ESPACIO: O(1).
    int temp = mcd(f1->numerador, f1->denominador);
    f1->numerador /= temp;
    f1->denominador /= temp;
}

void destruir(Fraccion f1){
// PROPÓSITO: Destruye la fracción dada.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    delete f1;
}

// #################################################################################################################### \\