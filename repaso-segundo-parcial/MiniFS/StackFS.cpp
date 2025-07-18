#include <iostream>
#include "assert.h"
#include "StackFS.h"
using namespace std;

// #################################################################################################################### \\

Stack emptyS(){
// PROPÓSITO: Describe un Stack vacío.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    StackStr* s = new StackStr;
    s->capacity = 16;
    s->elems = new ELEM_TYPE[s->capacity];
    s->size = 0;
    return s;
}

bool isEmptyS(Stack s){
// PROPÓSITO: Indica si el Stack dado está vacío.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    return s->size == 0;
}

void push(ELEM_TYPE x, int index, Stack s){
// PROPÓSITO: Inserta un elemento en el tope del Stack dado.
// COSTO TIEMPO: O(1) Amortizado.
// COSTO ESPACIO: O(1).
    if(s->size != s->capacity){
        // En caso que la capacidad actual no sea el máximo permitido, se inserta.
        s->elems[s->size] = x;
        s->size++;
    } else {
        // En caso que la capacidad actual sea el máximo permitido, se duplica la capacidad del array.
        ELEM_TYPE* nElems = new ELEM_TYPE[s->capacity * 2];

        // Se copian los elementos del array viejo al nuevo.
        for(int i = 0; i < s->capacity; i++){
            nElems[i] = s->elems[i];
        }

        // Se libera la memoria ocupada por el array viejo.
        delete[] s->elems;

        // Se actualizan los campos del Stack dado.
        s->capacity *= 2;
        s->elems = nElems;
        s->elems[s->size] = x;
        s->size++;
    }
}

ELEM_TYPE top(Stack s){
// PROPÓSITO: Describe el elemento en el tope del Stack dado sin eliminarlo.
// PRECONDICIÓN: El Stack no debe estar vacío.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    assert(s->size > 0);
    return s->elems[s->size-1];
}

void pop(Stack s){
// PROPÓSITO: Elimina el elemento en el tope del Stack dado.
// PRECONDICIÓN: El Stack no debe estar vacío.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    assert(s->size > 0);
    s->size--;
}

int size(Stack s){
// PROPÓSITO: Describe el tamaño del Stack dado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    return s->size;
}

void destroyS(Stack s){
// PROPÓSITO: Libera la memoria reservada por el Stack dado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    s->size = -1; // Solo por si accidentalmente se accede a memoria liberada y sigue existiendo este registro en la memoria heap.
    delete[] s->elems;
    delete s;
}

// #################################################################################################################### \\