#include <iostream>
#include "Fraccion.h"
using namespace std;

// #################################################################################################################### \\

/* INTERFAZ FRACCIÓN:
    * Fraccion fraccion(int x, int y);
    * Fraccion sumar(Fraccion f1, Fraccion f2);
    * Fraccion restar(Fraccion f1, Fraccion f2);
    * Fraccion multiplicar(Fraccion f1, Fraccion f2);
    * Fraccion dividir(Fraccion f1, Fraccion f2);
    * void invertir(Fraccion f1);
    * void simplificar(Fraccion f1);
    * void destruir(Fraccion f1);
*/

// #################################################################################################################### \\

int main(){
    // Fraccion ceroFraccion = fraccion(32, 0);
    Fraccion primeraFraccion = fraccion(32, 6);
    Fraccion segundaFraccion = fraccion(25, 5);
    Fraccion terceraFraccion = sumar(primeraFraccion, segundaFraccion);
    Fraccion cuartaFraccion = restar(primeraFraccion, segundaFraccion);
    Fraccion quintaFraccion = multiplicar(terceraFraccion, cuartaFraccion);
    Fraccion sextaFraccion = dividir(terceraFraccion, cuartaFraccion);
    cout << "El valor de la primera fraccion es: " << primeraFraccion->numerador << " / " << primeraFraccion->denominador << "." << endl;
    cout << "El valor de la segunda fraccion es: " << segundaFraccion->numerador << " / " << segundaFraccion->denominador << "." << endl;
    cout << "El valor de la tercera fraccion es: " << terceraFraccion->numerador << " / " << terceraFraccion->denominador << "." << endl;
    cout << "El valor de la cuarta fraccion es:  " << cuartaFraccion->numerador << " / " << cuartaFraccion->denominador << "." << endl;
    cout << "El valor de la quinta fraccion es: " << quintaFraccion->numerador << " / " << quintaFraccion->denominador << "." << endl;
    cout << "El valor de la sexta fraccion es:  " << sextaFraccion->numerador << " / " << sextaFraccion->denominador << "." << endl;
    simplificar(primeraFraccion);
    simplificar(segundaFraccion);
    simplificar(terceraFraccion);
    simplificar(cuartaFraccion);
    simplificar(quintaFraccion);
    simplificar(sextaFraccion);
    cout << endl;
    cout << "Despues de haber simplificado todas las fracciones, quedaron:" << endl;
    cout << endl;
    cout << "El valor de la primera fraccion es: " << primeraFraccion->numerador << " / " << primeraFraccion->denominador << "." << endl;
    cout << "El valor de la segunda fraccion es: " << segundaFraccion->numerador << "  / " << segundaFraccion->denominador << "." << endl;
    cout << "El valor de la tercera fraccion es: " << terceraFraccion->numerador << " / " << terceraFraccion->denominador << "." << endl;
    cout << "El valor de la cuarta fraccion es:  " << cuartaFraccion->numerador << "  / " << cuartaFraccion->denominador << "." << endl;
    cout << "El valor de la quinta fraccion es:  " << quintaFraccion->numerador << " / " << quintaFraccion->denominador << "." << endl;
    cout << "El valor de la sexta fraccion es:   " << sextaFraccion->numerador << " / " << sextaFraccion->denominador << "." << endl;
    invertir(primeraFraccion);
    invertir(terceraFraccion);
    invertir(quintaFraccion);
    cout << endl;
    cout << "Despues de haber invertido algunas fracciones, quedaron:" << endl;
    cout << endl;
    cout << "El valor de la primera fraccion es: " << primeraFraccion->numerador << "  / " << primeraFraccion->denominador << "." << endl;
    cout << "El valor de la segunda fraccion es: " << segundaFraccion->numerador << "  / " << segundaFraccion->denominador << "." << endl;
    cout << "El valor de la tercera fraccion es: " << terceraFraccion->numerador << "  / " << terceraFraccion->denominador << "." << endl;
    cout << "El valor de la cuarta fraccion es:  " << cuartaFraccion->numerador << "  / " << cuartaFraccion->denominador << "." << endl;
    cout << "El valor de la quinta fraccion es:  " << quintaFraccion->numerador << "  / " << quintaFraccion->denominador << "." << endl;
    cout << "El valor de la sexta fraccion es:   " << sextaFraccion->numerador << " / " << sextaFraccion->denominador << "." << endl;
    cout << endl;
    cout << "Despues de eliminar algunas fracciones, quedaron:" << endl;
    cout << endl;
    destruir(segundaFraccion);
    destruir(cuartaFraccion);
    destruir(sextaFraccion);
    cout << "El valor de la primera fraccion es: " << primeraFraccion->numerador << "  / " << primeraFraccion->denominador << "." << endl;
    cout << "El valor de la segunda fraccion es: " << segundaFraccion->numerador << " / " << segundaFraccion->denominador << "." << endl;
    cout << "El valor de la tercera fraccion es: " << terceraFraccion->numerador << " / " << terceraFraccion->denominador << "." << endl;
    cout << "El valor de la cuarta fraccion es:  " << cuartaFraccion->numerador << " / " << cuartaFraccion->denominador << "." << endl;
    cout << "El valor de la quinta fraccion es:  " << quintaFraccion->numerador << " / " << quintaFraccion->denominador << "." << endl;
    cout << "El valor de la sexta fraccion es:   " << sextaFraccion->numerador << " / " << sextaFraccion->denominador << "." << endl;
    return 0;
}