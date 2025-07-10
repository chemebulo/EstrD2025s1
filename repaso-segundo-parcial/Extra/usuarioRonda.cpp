#include <iostream>
#include "Ronda.h"
using namespace std;

// #################################################################################################################### \\

// EJERCICIO 1:

int* toArray(Ronda ronda){
// PROPÓSITO: Describe la ronda dada como un array.
    int l = length(ronda);
    int* r = NULL;

    if(l > 0){
        r = new int[l];

        for(int i = 0; i < l; i++){
            r[i] = current(ronda);
            move(1, ronda);
        }
    }

    return r;
}

// EJERCICIO 2:

Ronda fromArray(int len, int* arr){
// PROPÓSITO: Describe el array dado como una ronda.
    Ronda r = mkRonda();
    
    for(int i = 0; i < len; i++){
        insert(arr[i], r);
        move(1, r);
    }

    delete[] arr;

    return r;
}