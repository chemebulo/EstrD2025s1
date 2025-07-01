#include <iostream>
using namespace std;

// #################################################################################################################### \\

struct NodoL {
    int elem;         // Es el valor del nodo.
    NodoL* siguiente; // Es el puntero al siguiente nodo.
};

struct LinkedListSt {
// INVARIANTES DE REPRESENTACIÓN: 
//  * Cantidad indica la cantidad de nodos que se pueden recorrer desde primero por siguiente hasta alcanzar a NULL.
    int cantidad;   // Es la cantidad de nodos.
    NodoL* primero; // Es el puntero al primer nodo.
};

typedef LinkedListSt* LinkedList; // INV.REP.: El puntero NO es NULL.

struct IteratorSt {
    NodoL* current;
};

typedef IteratorSt* ListIterator; // INV.REP.: El puntero NO es NULL.

// #################################################################################################################### \\

LinkedList nil();
// PROPÓSITO: Crea una lista vacía.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

bool isEmpty(LinkedList xs);
// PROPÓSITO: Indica si la lista está vacía.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

int head(LinkedList xs);
// PROPÓSITO: Devuelve el primer elemento.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void Cons(int x, LinkedList xs);
// PROPÓSITO: Agrega un elemento al principio de la lista.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void Tail(LinkedList xs);
// PROPÓSITO: Quita el primer elemento.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

int length(LinkedList xs);
// PROPÓSITO: Devuelve la cantidad de elementos.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void Snoc(int x, LinkedList xs);
// PROPÓSITO: Agrega un elemento al final de la lista.
// COSTO TIEMPO: O(n).
// COSTO ESPACIO: O(1).

// #################################################################################################################### \\

ListIterator getIterator(LinkedList xs);
// PROPÓSITO: Apunta el recorrido al primer elemento.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

int current(ListIterator ixs);
// PROPÓSITO: Devuelve el elemento actual en el recorrido.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void SetCurrent(int x, ListIterator ixs);
// PROPÓSITO: Reemplaza el elemento actual por otro elemento.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void Next(ListIterator ixs);
// PROPÓSITO: Pasa al siguiente elemento.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

bool atEnd(ListIterator ixs);
// PROPÓSITO: Indica si el recorrido ha terminado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void DisposeIterator(ListIterator ixs);
// PROPÓSITO: Libera la memoria ocupada por el iterador.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void DestroyL(LinkedList xs);
// PROPÓSITO: Libera la memoria ocupada por la lista.
// COSTO TIEMPO: O(n).
// COSTO ESPACIO: O(1).

// #################################################################################################################### \\