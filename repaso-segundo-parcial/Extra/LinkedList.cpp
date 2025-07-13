#include <iostream>
#include "LinkedList.h"
using namespace std;

// #################################################################################################################### \\

LinkedList nil(){
// PROPÓSITO: Crea una LinkedList vacía.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    LinkedListStr* xs = new LinkedListStr;
    xs->size = 0;
    xs->first = NULL;
    xs->last= NULL;
    return xs;
}

bool isEmpty(LinkedList xs){
// PROPÓSITO: Indica si la LinkedList dada se encuentra vacía.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    return xs->size == 0;
}

int head(LinkedList xs){
// PROPÓSITO: Devuelve el primer valor de la LinkedList dada.
// PRECONDICIÓN: La LinkedList dada no debe estar vacía.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    return xs->first->value;
}

void Cons(int x, LinkedList xs){
// PROPÓSITO: Agrega el valor valor al principio de la LinkedList dada.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    NodoL* nuevo = new NodoL;
    nuevo->value = x;
    nuevo->next = xs->first;
    xs->first = nuevo;
    
    if(xs->last == NULL){
        // En este caso la LinkedList no tiene ningún nodo en la misma.
        xs->last = nuevo;
    }

    xs->size++;
}

void Tail(LinkedList xs){
// PROPÓSITO: Quita el primer elemento de la LinkedList dada.
// PRECONDICIÓN: La LinkedList dada no debe estar vacía.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    NodoL* temp = xs->first;
    
    // Modifica el first por el next el first que tiene actualmente la LinkedList.
    xs->first = xs->first->next;

    if(xs->first == NULL){
        // En este caso hay solamente un nodo en la LinkedList.
        xs->last = NULL;
    }

    delete temp;
    xs->size--;
}

int length(LinkedList xs){
// PROPÓSITO: Devuelve la cantidad de elementos de la LinkedList dada.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    return xs->size;
}

void Snoc(int x, LinkedList xs){
// PROPÓSITO: Agrega un elemento al final de la LinkedList dada.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    NodoL* nuevo = new NodoL;
    nuevo->value = x;
    nuevo->next = NULL;

    if(xs->last == NULL){
        // En este caso no hay ningún nodo en la LinkedList.
        xs->first = nuevo;
    } else {
        // En este caso hay más de un nodo en la LinkedList.
        xs->last->next = nuevo;
    }

    xs->last = nuevo;
    xs->size++;
}

// #################################################################################################################### \\

ListIterator getIterator(LinkedList xs){
// PROPÓSITO: Apunta el recorrido al primer elemento.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    IteratorStr* ixs = new IteratorStr;
    ixs->current = xs->first;
    return ixs;
}

int current(ListIterator ixs){
// PROPÓSITO: Devuelve el valor del elemento actual en el recorrido.
// PRECONDICIÓN: El iterador no está en el final del recorrido.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    return ixs->current->value;
}

void SetCurrent(int x, ListIterator ixs){
// PROPÓSITO: Reemplaza el elemento actual por otro elemento.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    ixs->current->value = x;
}

void Next(ListIterator ixs){
// PROPÓSITO: Pasa al siguiente elemento.
// PRECONDICIÓN: El iterador no está en el final del recorrido.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    ixs->current = ixs->current->next;
}

bool atEnd(ListIterator ixs){
// PROPÓSITO: Indica si el recorrido ha terminado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    return ixs->current == NULL;
}

void DisposeIterator(ListIterator ixs){
// PROPÓSITO: Libera la memoria ocupada por el iterador.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    delete ixs;
}

void DestroyL(LinkedList xs){
// PROPÓSITO: Libera la memoria ocupada por la lista.
// COSTO TIEMPO: O(n).
// COSTO ESPACIO: O(1).
    NodoL* temp = new NodoL;

    // Si hay más de un nodo en la LinkedList, se borran todos.
    while(xs->first != NULL){
        temp = xs->first;
        xs->first = xs->first->next;
        delete temp;
        xs->size--;
    }

    // En este punto la LinkedList se encuentra vacía.
    delete xs;
}

// #################################################################################################################### \\