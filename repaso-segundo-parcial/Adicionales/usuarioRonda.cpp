#include <iostream>
#include "Ronda.h"
using namespace std;

int* toArray(Ronda ronda){
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

Ronda fromArray(int len, int* arr){
    Ronda r = mkRonda();
    
    for(int i = 0; i < len; i++){
        insert(arr[i], r);
        move(1, r);
    }

    delete[] arr;

    return r;
}