#include <iostream>
using namespace std;

// #################################################################################################################### \\

struct NodoL{
    int value;   // Representa el valor en el nodo actual.
    NodoL* next; // Es el puntero al siguiente nodo.
};

struct LinkedListStr{
// INVARIANTES DE REPRESENTACIÓN: 
//  * La cantidad debe ser igual a la cantidad de veces posibles de acceder al next de cada nodo iniciando desde el nodo
//    first hasta llegar al nodo last de la LinkedList.
//  * Debe poderse alcanzar el nodo last partiendo desde el next del nodo first de la LinkedList.
//  * Si el nodo first es NULL, el nodo last también deberá serlo.
    int size;     // Representa la cantidad de nodos que existen en la LinkedList.
    NodoL* first; // Representa el primer nodo de la LinkedList.
    NodoL* last;  // Representa el último nodo de la LinkedList.
};

typedef LinkedListStr* LinkedList; // INV.REP.: El puntero NO es NULL.

struct IteratorStr{
    NodoL* current;  // Representa el nodo actual en el iterador.
};

typedef IteratorStr* ListIterator; // INV.REP.: El puntero NO es NULL.

// #################################################################################################################### \\

LinkedList nil();
// PROPÓSITO: Crea una LinkedList vacía.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

bool isEmpty(LinkedList xs);
// PROPÓSITO: Indica si la LinkedList dada se encuentra vacía.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

int head(LinkedList xs);
// PROPÓSITO: Devuelve el primer valor de la LinkedList dada.
// PRECONDICIÓN: La LinkedList dada no debe estar vacía.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void Cons(int x, LinkedList xs);
// PROPÓSITO: Agrega el valor valor al principio de la LinkedList dada.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void Tail(LinkedList xs);
// PROPÓSITO: Quita el primer elemento de la LinkedList dada.
// PRECONDICIÓN: La LinkedList dada no debe estar vacía.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

int length(LinkedList xs);
// PROPÓSITO: Devuelve la cantidad de elementos de la LinkedList dada.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void Snoc(int x, LinkedList xs);
// PROPÓSITO: Agrega un elemento al final de la LinkedList dada.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

// #################################################################################################################### \\

ListIterator getIterator(LinkedList xs);
// PROPÓSITO: Apunta el recorrido al primer elemento.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

int current(ListIterator ixs);
// PROPÓSITO: Devuelve el valor del elemento actual en el recorrido.
// PRECONDICIÓN: El iterador no está en el final del recorrido.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void SetCurrent(int x, ListIterator ixs);
// PROPÓSITO: Reemplaza el elemento actual por otro elemento.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void Next(ListIterator ixs);
// PROPÓSITO: Pasa al siguiente elemento.
// PRECONDICIÓN: El iterador no está en el final del recorrido.
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