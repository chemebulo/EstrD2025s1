#include <iostream>
using namespace std;

struct NodoQ {
    int elem;         // Es el valor del nodo.
    NodoQ* siguiente; // Es el puntero al siguiente nodo.
};

struct QueueSt {
// INVARIANTES DE REPRESENTACIÓN:
//  * La cantidad describe la cantidad de nodos hasta alcanzar el último nodo en la Queue, el cual es NULL.
//  * Primero debe apuntar al primer nodo de la Queue.
//  * Ultimo debe apuntar al último nodo de la Queue.
    int cantidad;   // Es la cantidad de elementos.
    NodoQ* primero; // Es el puntero al primer nodo.
    NodoQ* ultimo;  // Es el puntero al ultimo nodo.
};

typedef QueueSt* Queue;

// #################################################################################################################### \\

Queue emptyQ();
// PROPÓSITO: Crea una cola vacía.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

bool isEmptyQ(Queue q);
// PROPÓSITO: Indica si la cola está vacía.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

int firstQ(Queue q);
// PROPÓSITO: Devuelve el primer elemento de la cola.
// PRECONDICIÓN: La cola no está vacía.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void Enqueue(int x, Queue q);
// PROPÓSITO: Agrega un elemento al final de la cola.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void Dequeue(Queue q);
// PROPÓSITO: Devuelve el primer elemento de la cola.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

int lengthQ(Queue q);
// PROPÓSITO: Devuelve la cantidad de elementos de la cola.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void MergeQ(Queue q1, Queue q2);
// PROPÓSITO: Anexa Q2 al final de Q1, liberando la memoria inservible de Q2 en el proceso.
// NOTA: Si bien se libera memoria de Q2, no necesariamente la de sus nodos.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void DestroyQ(Queue q);
// PROPÓSITO: Libera la memoria ocupada por la cola.
// COSTO TIEMPO: O(n).
// COSTO ESPACIO: O(1).

// #################################################################################################################### \\