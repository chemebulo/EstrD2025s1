#include <iostream>
using namespace std;

struct NodoL{
/*
    PROPÓSITO: Representa a un nodo mediante dos campos: 
        * elem: indica el valor que tiene el nodo.
        * siguiente: describe el puntero al siguiente nodo.
*/
    int elem;         
    NodoL* siguiente;
};

struct LinkedListSt{
/* 
    PROPÓSITO: Representa la LinkedList mediante dos campos:
      * cantidad: indica la cantidad de elementos. 
      * primero: describe el puntero al primer nodo. 
      * ultimo: describe el puntero el último nodo.
    INV.REP.: 
      * Cantidad indica la cantidad de nodos que se pueden recorrer desde primero 
        por siguiente hasta alcanzar a NULL.
*/
    int cantidad;   
    NodoL* primero;
    NodoL* ultimo;
};

typedef LinkedListSt* LinkedList;
/*
    PROPÓSITO: Describe que LinkedList es un puntero de tipo LinkedListSt.
    INV.REP.:
        * El puntero NO es NULL.
*/ 

struct IteratorSt{
//  PROPÓSITO: Describe el nodo actual.
    NodoL* current;
};

typedef IteratorSt* ListIterator; // INV.REP.: El puntero NO es NULL.

// ############################################################################### //

LinkedList nil();
// PROPÓSITO: Crea una lista vacía.

bool isEmpty(LinkedList xs);
// PROPÓSITO: Indica si la lista está vacía.

int head(LinkedList xs);
// PROPÓSITO: Devuelve el primer elemento.

void Cons(int x, LinkedList xs);
// PROPÓSITO: Agrega un elemento al principio de la lista.

void Tail(LinkedList xs);
// PROPÓSITO: Quita el primer elemento.

int length(LinkedList xs);
// PROPÓSITO: Devuelve la cantidad de elementos.

void Snoc(int x, LinkedList xs);
// PROPÓSITO: Agrega un elemento al nal de la lista.

ListIterator getIterator(LinkedList xs);
// PROPÓSITO: Apunta el recorrido al primer elemento.

int current(ListIterator ixs);
// PROPÓSITO: Devuelve el elemento actual en el recorrido.

void SetCurrent(int x, ListIterator ixs);
// PROPÓSITO: Reemplaza el elemento actual por otro elemento.

void Next(ListIterator ixs);
// PROPÓSITO: Pasa al siguiente elemento.

bool atEnd(ListIterator ixs);
// PROPÓSITO: Indica si el recorrido ha terminado.

void DisposeIterator(ListIterator ixs);
// PROPÓSITO: Libera la memoria ocupada por el iterador.

void DestroyL(LinkedList xs);
// PROPÓSITO: Libera la memoria ocupada por la lista.