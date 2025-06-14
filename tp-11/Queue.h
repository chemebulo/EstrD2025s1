#include <iostream>
using namespace std;

struct NodoQ {
/*
    PROPÓSITO: Representa a un nodo mediante dos campos: 
        * elem: indica el valor que tiene el nodo.
        * siguiente: describe el puntero al siguiente nodo.
*/
    int elem;
    NodoQ* siguiente;
};

struct QueueSt {
/* 
    PROPÓSITO: Representa la Queue mediante tres campos:
      * cantidad: indica la cantidad de elementos. 
      * primero: describe el puntero al primer nodo. 
      * ultimo: describe el puntero al último nodo.
    INV.REP.: 
      * Cantidad describe la cantidad de nodos que se pueden recorrer desde primero por siguiente hasta alcanzar a NULL.
      * Primero debe apuntar al primer nodo de la Queue.
      * Ultimo debe apuntar al último nodo de la Queue.
*/
    int cantidad;
    NodoQ* primero;
    NodoQ* ultimo;
};

typedef QueueSt* Queue;

Queue emptyQ();
// PROPÓSITO: Crea una cola vacía.
// COSTO: O(1).

bool isEmptyQ(Queue q);
// PROPÓSITO: Indica si la cola está vacía.
// COSTO: O(1).

int firstQ(Queue q);
// PROPÓSITO: Devuelve el primer elemento.
// COSTO: O(1).

void Enqueue(int x, Queue q);
// PROPÓSITO: Agrega un elemento al final de la cola.
// COSTO: O(1).

void Dequeue(Queue q);
// PROPÓSITO: Quita el primer elemento de la cola.
// COSTO: O(1).

int lengthQ(Queue q);
// PROPÓSITO: Devuelve la cantidad de elementos de la cola.
// COSTO: O(1).

void MergeQ(Queue q1, Queue q2);
// PROPÓSITO: Anexa Q2 al final de Q1, liberando la memoria inservible de Q2 en el proceso.
// NOTA: Si bien se libera memoria de Q2, no necesariamente la de sus nodos.
// COSTO: O(1).

void DestroyQ(Queue q);
// PROPÓSITO: Libera la memoria ocupada por la cola.
// COSTO: O(N).