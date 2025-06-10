#include <iostream>
#include "LinkedList.h"
using namespace std;

LinkedList nil(){
// PROPÓSITO: Crea una lista vacía.
// COSTO: O(1).
// Siendo de costo constante ya que solamente se modifican los campos de una LinkedList sin elementos.
    LinkedListSt* xs = new LinkedListSt();
    xs->cantidad = 0;
    xs->primero = NULL;
    return xs;
}

bool isEmpty(LinkedList xs){
// PROPÓSITO: Indica si la lista está vacía.
// COSTO: O(1).
// Siendo de costo constante ya que solamente se accede a uno de los campos de la LinkedList y se realiza una comparación.
    return xs->cantidad == 0;
}

int head(LinkedList xs){
// PROPÓSITO: Devuelve el primer elemento.
// COSTO: O(1).
// Siendo de costo constante ya que solamente se accede a uno de los campos de la LinkedList y al Nodo.
    return xs->primero->elem;
}

void Cons(int x, LinkedList xs){
// PROPÓSITO: Agrega un elemento al principio de la lista.
// COSTO: O(1).
// Siendo de costo constante ya que solamente se crea un nodo, lo cual es constante, y además se modifican los campos
// de la LinkedList dada.
    NodoL* n = new NodoL();
    n->elem = x;
    n->siguiente = xs->primero;
    xs->primero = n;
    xs->cantidad++;
}

void Tail(LinkedList xs){
// PROPÓSITO: Quita el primer elemento.
// PRECONDICIÓN: La LinkedList dada no es vacía.
// COSTO: O(1).
// Siendo de costo constante ya que solamente se modifican los campos de la LinkedList dada.
    NodoL* temp = xs->primero;
    xs->primero = xs->primero->siguiente;
    xs->cantidad--;
    delete temp;
}

int length(LinkedList xs){
// PROPÓSITO: Devuelve la cantidad de elementos.
// COSTO: O(1).
// Siendo de costo constante ya que solamente se accede a uno de los campos de la LinkedList y se devuelve eso.
    return xs->cantidad;
}

// REVISAR TODO ESTO:

void Snoc(int x, LinkedList xs){
// PROPÓSITO: Agrega un elemento al final de la lista.
// COSTO: O(N).
// Siendo N la cantidad de elementos en la LinkedList dada, por cada N se realizan operaciones de costo constante.
// Es por eso que el costo total de la función es lineal.
    NodoL* n = new NodoL();
    n->elem = x;
    n->siguiente = NULL;

    NodoL* ultimoNodo = xs->primero;

    for(int i = 0; i < xs->cantidad; i++){
        ultimoNodo = ultimoNodo->siguiente;
    }

    ultimoNodo->siguiente = n;
    xs->cantidad++;
}

ListIterator getIterator(LinkedList xs){
// PROPÓSITO: Apunta el recorrido al primer elemento.
// COSTO: O(1).
// Siendo de costo constante ya que solamente se inicializa un Iterador y se modifica uno de sus campos.
    IteratorSt* ixs = new IteratorSt();
    ixs->current = xs->primero;
    return ixs; 
}

int current(ListIterator ixs){
// PROPÓSITO: Devuelve el elemento actual en el recorrido.
// COSTO: O(1).
// Siendo de costo constante ya que solamente se accede a uno de los campos del Iterador dado.
    return ixs->current->elem;
}

void SetCurrent(int x, ListIterator ixs){
// PROPÓSITO: Reemplaza el elemento actual por otro elemento.
// COSTO: O(1).
// Siendo de costo constante ya que solamente se accede a uno de los campos del Iterador dado y se realiza una comparación.
    ixs->current->elem = x;
}

void Next(ListIterator ixs){
// PROPÓSITO: Pasa al siguiente elemento.
// COSTO: O(1).
// Siendo de costo constante ya que solamente se accede a uno de los campos del Iterador dado y se devuelve el valor al que apunta.
    ixs->current = ixs->current->siguiente;
}

bool atEnd(ListIterator ixs){
// PROPÓSITO: Indica si el recorrido ha terminado.
// COSTO: O(1).
// Siendo de costo constante ya que solamente se accede a uno de los campos del Iterador dado y se realiza una comparación.
    return ixs->current->siguiente == NULL;
}

void DisposeIterator(ListIterator ixs){
// PROPÓSITO: Libera la memoria ocupada por el iterador.
// COSTO: O(1).
// Siendo de costo constante ya que solamente utiliza la operación "delete" de costo constante.
    delete ixs;
}

void DestroyL(LinkedList xs){
// PROPÓSITO: Libera la memoria ocupada por la lista.
// COSTO: O(N).
// Siendo n la cantidad de elementos en la LinkedList, por cada N se realizan operaciones constantes como "delete",
// es por eso que el costo total es lineal.
    NodoL* temp = xs->primero;
    while(xs->primero != NULL){
        xs->primero = xs->primero->siguiente;
        delete temp;
        temp = xs->primero;
    }
    delete xs;
}