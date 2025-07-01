#include <iostream>
#include "LinkedList.h"
using namespace std;

// #################################################################################################################### \\

LinkedList nil(){
// PROPÓSITO: Crea una lista vacía.
// COSTO TIEMPO: O(1).
    // Siendo constante ya que solo se asignaron valores a un registro.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante ya que solamente se utiliza un solo frame en la memoria stack en cualquier caso.
    LinkedListSt* xs = new LinkedListSt;
    xs->cantidad = 0;
    xs->primero = NULL;
    return xs;
}

bool isEmpty(LinkedList xs){
// PROPÓSITO: Indica si la lista está vacía.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante ya que solamente se accede a un campo de un registro.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante ya que solamente se utiliza un solo frame en la memoria stack en cualquier caso.
    return xs->cantidad == 0;
}

int head(LinkedList xs){
// PROPÓSITO: Devuelve el primer elemento de la lista.
// PRECONDICIÓN: La LinkedList dada no puede ser vacía.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante ya que solamente se accede a un campo de un registro.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante ya que solamente se utiliza un solo frame en la memoria stack en cualquier caso.
    return xs->primero->elem;
}

void Cons(int x, LinkedList xs){
// PROPÓSITO: Agrega un elemento al principio de la lista.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante ya que solamente se modifican campos de un registro.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante ya que solamente se utiliza un solo frame en la memoria stack en cualquier caso.
    NodoL* n = new NodoL;
    n->elem = x;
    n->siguiente = xs->primero;
    xs->primero = n;
    xs->cantidad++;
}

void Tail(LinkedList xs){
// PROPÓSITO: Quita el primer elemento de la lista.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante ya que solamente se modifican campos de un registro.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante ya que solamente se utiliza un solo frame en la memoria stack en cualquier caso.
    if(xs->primero != NULL){
        NodoL* temp = xs->primero;
        xs->primero = temp->siguiente;
        delete temp;
        xs->cantidad--;
    }
}

int length(LinkedList xs){
// PROPÓSITO: Devuelve la cantidad de elementos de la lista.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante ya que solamente se accede a un campo de un registro.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante ya que solamente se utiliza un solo frame en la memoria stack en cualquier caso.
    return xs->cantidad;
}

void Snoc(int x, LinkedList xs){
// PROPÓSITO: Agrega un elemento al final de la lista.
// COSTO TIEMPO: O(n).
    // Siendo de costo lineal, ya que se realizan operaciones constantes por cada elemento en la lista.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante ya que solamente se utiliza un solo frame en la memoria stack en cualquier caso.
    NodoL* nuevo = new NodoL;
    nuevo->elem = x;
    nuevo->siguiente = NULL;

    if(xs->primero == NULL){
        xs->primero = nuevo;
    } else {
        NodoL* temp = xs->primero;
        while(temp->siguiente != NULL){
            temp = temp->siguiente;
        }
        temp->siguiente = nuevo;
    }
    xs->cantidad++;
}

// #################################################################################################################### \\

ListIterator getIterator(LinkedList xs){
// PROPÓSITO: Apunta el recorrido al primer elemento de la lista.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante, ya que solo se inicializa un registro y se modifican sus campos.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante ya que solamente se utiliza un solo frame en la memoria stack en cualquier caso.
    IteratorSt* ixs = new IteratorSt;
    ixs->current = xs->primero;
    return ixs;
}

int current(ListIterator ixs){
// PROPÓSITO: Devuelve el elemento actual en el recorrido.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante, ya que solo se accede al campo del registro.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante ya que solamente se utiliza un solo frame en la memoria stack en cualquier caso.
    return ixs->current->elem;
}

void SetCurrent(int x, ListIterator ixs){
// PROPÓSITO: Reemplaza el elemento actual por otro elemento.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante, ya que solo se modifica un campo del registro.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante ya que solamente se utiliza un solo frame en la memoria stack en cualquier caso.
    ixs->current->elem = x;
}

void Next(ListIterator ixs){
// PROPÓSITO: Pasa al siguiente elemento.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante, ya que solo se modifica un campo del registro.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante ya que solamente se utiliza un solo frame en la memoria stack en cualquier caso.
    if(ixs->current != NULL){
        ixs->current = ixs->current->siguiente;
    }
}

bool atEnd(ListIterator ixs){
// PROPÓSITO: Indica si el recorrido ha terminado.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante, ya que solo se accede al campo del registro.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante ya que solamente se utiliza un solo frame en la memoria stack en cualquier caso.
    return ixs->current == NULL;
}

void DisposeIterator(ListIterator ixs){
// PROPÓSITO: Libera la memoria ocupada por el iterador.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante, ya que solo se libera el espacio reservado en la memoria heap para el iterador.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante ya que solamente se utiliza un solo frame en la memoria stack en cualquier caso.
    delete ixs;
}

void DestroyL(LinkedList xs){
// PROPÓSITO: Libera la memoria ocupada por la lista.
// COSTO TIEMPO: O(n).
    // Siendo de costo lineal, ya que por cada elemento de la lista se realiza la operación "delete" de costo constante.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante ya que solamente se utiliza un solo frame en la memoria stack en cualquier caso.
    NodoL* temp = xs->primero;

    while(xs->primero != NULL){
        xs->primero = xs->primero->siguiente;
        delete temp;
        temp = xs->primero;
    }

    delete xs;
}

// #################################################################################################################### \\