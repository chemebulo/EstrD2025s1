#include "Par.h"
using namespace std;

// EJERCICIO 3 (Implementador):

// EJERCICIO 3.1:
Par consPar(int n, int m) {
// PROPÓSITO: Construye un par.
    Par p;
    p.x = n;
    p.y = m;
    return p;
}


// EJERCICIO 3.2:
int fst(Par p) {
// PROPÓSITO: Devuelve la primera componente.
    int x = p.x;   
    return x;
}


// EJERCICIO 3.3:
int snd(Par p){
// PROPÓSITO: Devuelve la segunda componente.
    int y = p.y;   
    return y;
}


// EJERCICIO 3.4:
int maxDelPar(Par p){
// PROPÓSITO: Devuelve la mayor componente.
    int x = p.x;
    int y = p.y;
    if (x > y) {
        return x;
    } else {
        return y;
    }
}


// EJERCICIO 3.5:
Par swap(Par p){
// PROPÓSITO: Devuelve un par con las componentes intercambiadas.
    int x = p.x;
    int y = p.y;
    return consPar(y, x); 
}


// EJERCICIO 3.6:
Par divisionYResto(int n, int m){
// PROPÓSITO: Devuelve un par donde la primer componente es la división y la segunda el resto entre ambos números.
    int division = n / m;
    int resto = n % m;
    Par divYResto = consPar(division, resto);
    return divYResto;
}