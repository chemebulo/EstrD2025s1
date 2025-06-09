#include <iostream>
#include "ArrayList.h"
using namespace std;

ArrayList newArrayList(){
// PROPÓSITO: Crea una lista con 0 elementos.
// NOTA: Empezar el array list con capacidad 16.
    ArrayListSt* a = new ArrayListSt;
    a->cantidad = 0;
    a->elementos = new int[16];
    a->capacidad = 16;
    return a;
}

ArrayList newArrayListWith(int capacidad){
// PROPÓSITO: Crea una lista con 0 elementos y una capacidad dada por parámetro.
    ArrayListSt* a = new ArrayListSt;
    a->cantidad = 0;
    a->elementos = new int[capacidad];
    a->capacidad = capacidad;
    return a;
}

int lengthAL(ArrayList xs){
// PROPÓSITO: Devuelve la cantidad de elementos existentes.
    return xs->cantidad;
}

int get(int i, ArrayList xs){
// PROPÓSITO: Devuelve el iésimo elemento de la lista.
// PRECONDICIÓN: Debe existir el elemento iésimo en la lista dada.
    return xs->elementos[i];
}

void set(int i, int x, ArrayList xs){
// PROPÓSITO: Reemplaza el iésimo elemento por otro dado.
// PRECONDICIÓN: Debe existir el elemento iésimo en la lista dada.
    xs->elementos[i] = x;
}

void resize(int capacidad, ArrayList xs){
// PROPÓSITO: Decrementa o aumenta la capacidad del array.
// NOTA: En caso de decrementarla, se pierden los elementos del final de la lista.
    int* nuevoArray = new int[capacidad];
    int nuevaCantidad = xs->cantidad;

    if(capacidad < xs->cantidad){
        nuevaCantidad = capacidad;
    }

    for(int i = 0; i < nuevaCantidad; i++){
        nuevoArray[i] = xs->elementos[i];
    }

    delete[] xs->elementos;

    xs->cantidad = nuevaCantidad;
    xs->elementos = nuevoArray;
    xs->capacidad = capacidad;
}

void add(int x, ArrayList xs){
// PROPÓSITO: Agrega un elemento al final de la lista.
    if(xs->cantidad == xs->capacidad) {
        int nuevaCapacidad = xs->capacidad * 2;
        int* nuevoArray = new int[nuevaCapacidad];

        for(int i = 0; i < xs->cantidad; i++) {
            nuevoArray[i] = xs->elementos[i];
        }

        delete[] xs->elementos;

        xs->elementos = nuevoArray;
        xs->capacidad = nuevaCapacidad;
    }

    xs->elementos[xs->cantidad] = x;
    xs->cantidad += 1;
}

void remove(ArrayList xs){
// PROPÓSITO: Borra el último elemento de la lista.
    if(xs->cantidad > 0){
        xs->cantidad -= 1;
    }
}