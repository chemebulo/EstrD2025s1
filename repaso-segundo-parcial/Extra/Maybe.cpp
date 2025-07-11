#include <iostream>
#include "assert.h"
#include "Maybe.h"
using namespace std;

// #################################################################################################################### \\

MaybeInt nothing(){
// PROPÓSITO: Describe un dato sin un valor asignado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    MaybeIntStr* maybe = new MaybeIntStr;
    maybe->haveValue = false;
    return maybe;
}

MaybeInt just(int x){
// PROPÓSITO: Describe un dato con el valor asignado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    MaybeIntStr* maybe = new MaybeIntStr;
    maybe->haveValue = true;
    maybe->value = x;
    return maybe;
}

int fromJust(MaybeInt m){
// PROPÓSITO: Describe el valor del dato dado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    assert(m->haveValue);
    return m->value;
}

bool isNothing(MaybeInt m){
// PROPÓSITO: Indica si el dato dado no tiene valor.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    return !m->haveValue;
}

// #################################################################################################################### \\