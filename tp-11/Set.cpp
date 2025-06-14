#include <iostream>
#include "Set.h"
using namespace std;

Set emptyS(){
// PROPÓSITO: Crea un conjunto vacío.
// COSTO: O(1).
// Siendo de costo constante ya que solamente se modificaron los campos del Set dado.
    SetSt* s = new SetSt();
    s->cantidad = 0;
    s->primero = NULL;
    return s;
}

bool isEmptyS(Set s){
// PROPÓSITO: Indica si el conjunto dado está vacío.
// COSTO: O(1).
// Siendo de costo constante ya que solamente se modificaron los campos del Set dado.
    return s->cantidad == 0;
}

bool belongsS(int x, Set s){
// PROPÓSITO: Indica si el elemento pertenece al conjunto.
// COSTO: O(N).
// Siendo N la cantidad de elementos diferentes en el Set...
    NodoS* actual = s->primero;

    while (actual != NULL) {
        if (actual->elem == x) {
            break;
        }
        actual = actual->siguiente;
    }

    return actual != NULL && actual->elem == x;
}

void AddS(int x, Set s){
// PROPÓSITO: Agrega el elemento dado al conjunto dado.
// COSTO: O(N).
// Siendo N la cantidad de elementos diferentes en el Set, por cada N se realizan operaciones de costo constante. Es por
// eso que el costo total de la función es lineal.
    NodoS* actual = s->primero;

    while (actual != NULL) {
        if (actual->elem == x) {
            return;
        }
        actual = actual->siguiente;
    }

    NodoS* nuevo = new NodoS();
    nuevo->elem = x;
    nuevo->siguiente = s->primero;

    s->primero = nuevo;
    s->cantidad++;
}

void RemoveS(int x, Set s){
// PROPÓSITO: Quita el elemento dado del conjunto dado.
// PRECONDICIÓN: El Set dado no está vacío.
// COSTO: O(N).
// Siendo N la cantidad de elementos diferentes en el Set, por cada N se realizan operaciones de costo constante. Es por
// eso que el costo total de la función es lineal.
    NodoS* actual = s->primero;

    if (actual != NULL && actual->elem == x){
        NodoS* aBorrar = actual;
        s->primero = actual->siguiente;
        delete aBorrar;
        s->cantidad--;
        return;
    }

    while (actual != NULL && actual->siguiente != NULL) {
        if (actual->siguiente->elem == x) {
            NodoS* aBorrar = actual->siguiente;
            actual->siguiente = aBorrar->siguiente;
            delete aBorrar;
            s->cantidad--;
            return;
        }
        actual = actual->siguiente;
    }
}

int sizeS(Set s){
// PROPÓSITO: Devuelve la cantidad de elementos del conjunto dado.
// COSTO: O(1).
// Siendo de costo constante ya que solamente se devuelve uno de los campos del Set dado.
    return s->cantidad;
}

LinkedList setToList(Set s){
// PROPÓSITO: Devuelve una lista con los elementos del conjunto dado.
// COSTO: O(N).
// Siendo N la cantidad de elementos diferentes en el Set, por cada N se realizan operaciones de costo constante. Es por
// eso que el costo total de la función es lineal.
    NodoS* actual = s->primero;
    LinkedList xs = nil();

    while (actual != NULL) {
        Cons(actual->elem, xs);
        actual = actual->siguiente;
    }
    
    return xs;
}

void DestroyS(Set s){
// PROPÓSITO: Libera la memoria ocupada por el conjunto dado.
// COSTO: O(N).
// Siendo N la cantidad de elementos diferentes en el Set, por cada N se realizan operaciones de costo constante. Es por
// eso que el costo total de la función es lineal.
    NodoS* actual = s->primero;

    while (actual != NULL) {
        NodoS* aBorrar = actual;
        actual = actual->siguiente;
        delete aBorrar;
    }

    delete s;
}