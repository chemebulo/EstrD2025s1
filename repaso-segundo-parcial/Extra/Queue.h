#include <iostream>
using namespace std;

// #################################################################################################################### \\

struct QueueNode{
// INVARIANTES DE REPRESENTACIÓN:
//  * El next del nodo no puede ser si mismo.
    int value;          // Representa el valor del nodo actual.
    QueueNode* next;    // Representa el próximo nodo del nodo actual.
};

struct QueueStr{
// INVARIANTES DE REPRESENTACIÓN:
//  * El orden de entrada o salida de nodos en la Queue es en base a FIFO (First In - First Out).
//  * Accediendo sucesivamente al next del nodo, partiendo del first de la Queue, debería poder llegarse al nodo last de la misma. 
//  * El next del nodo last de la Queue debe ser un puntero NULL.
//  * Si el first de la Queue es un puntero NULL, el last también deberá serlo.
//  * El amount describe la cantidad de nodos hasta alcanzar el último nodo en la Queue.
    int amount;         // Es la cantidad de valores en la Queue.
    QueueNode* first;   // Representa el primer nodo en la Queue.
    QueueNode* last;    // Representa el último nodo en la Queue.
};

typedef QueueStr* Queue;

// #################################################################################################################### \\

Queue emptyQ();
// PROPÓSITO: Describe una Queue vacía.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

bool isEmptyQ(Queue q);
// PROPÓSITO: Indica si la Queue dada se encuentra vacía.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

int firstQ(Queue q);
// PROPÓSITO: Describe el primer nodo de la Queue dada.
// PRECONDICIÓN: La Queue no se encuentra vacía.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void enqueue(Queue q, int x);
// PROPÓSITO: Coloca en la Queue dada al nodo con el valor dado en la última posición de la misma.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void dequeue(Queue q);
// PROPÓSITO: Quita un nodo de la Queue dada, siendo el primero de la misma.
// PRECONDICIÓN: La Queue no se encuentra vacía.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void destroyQ(Queue q);
// PROPÓSITO: Borra la Queue dada.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).

// #################################################################################################################### \\