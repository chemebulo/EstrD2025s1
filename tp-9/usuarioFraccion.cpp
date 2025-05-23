#include <iostream>
#include "Fraccion.h"
using namespace std;

/* INTERFAZ DE PAR:
     * Fraccion consFraccion(int numerador, int denominador)
     * int numerador(Fraccion f)
     * int denominador(Fraccion f)
     * float division(Fraccion f)
     * Fraccion multF(Fraccion f1, Fraccion f2)
     * Fraccion simplificada(Fraccion p)
     * Fraccion sumF(Fraccion f1, Fraccion f2)
*/

int main() {
     Fraccion f1 = consFraccion(10, 3);
     Fraccion f2 = consFraccion(9, 2);
     Fraccion f3 = consFraccion(74, 18);
     cout << "Fraccion 1: " << numerador(f1) << "/" << denominador(f1) << endl;
     cout << "Fraccion 2: " << numerador(f2) << "/" << denominador(f2) << endl;
     cout << "Division entre numerador y denominador en Fraccion 1 es: " << division(f1) << endl;
     cout << "Division entre numerador y denominador en Fraccion 2 es: " << division(f2) << endl;
     Fraccion mf1f2 = multF(f1, f2);
     cout << "La multplicacion entre Fraccion 1 y la Fraccion 2 es: " << numerador(mf1f2) << "/" << denominador(mf1f2) << endl;
     Fraccion sf3 = simplificada(f3);
     cout << "La simplificacion de la Fraccion 3 es: " << numerador(sf3) << "/" << denominador(sf3) << endl;
     Fraccion sf1f2 = sumF(f1, f2);
     cout << "La suma entre la Fraccion 1 y la Fraccion 2 es: " << numerador(sf1f2) << "/" << denominador(sf1f2) << endl;
     return 0;
}