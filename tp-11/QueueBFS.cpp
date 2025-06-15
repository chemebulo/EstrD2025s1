#include <iostream>
#include "QueueBFS.h"
using namespace std;

Queue emptyQ(){
// PROPÓSITO: Crea una cola vacía.
// COSTO: O(1).
    QueueSt* q = new QueueSt();
    q->cantidad = 0;
    q->primero = NULL;
    q->ultimo = NULL;
    return q;
}

bool isEmptyQ(Queue q){
// PROPÓSITO: Indica si la cola está vacía.
// COSTO: O(1).
    return q->cantidad == 0;
}

Tree firstQ(Queue q){
// PROPÓSITO: Devuelve el primer elemento.
// PRECONDICIÓN: La cola no está vacía.
// COSTO: O(1).
    return q->primero->elem;
}

void Enqueue(Tree t, Queue q){
// PROPÓSITO: Agrega un elemento al final de la cola.
// COSTO: O(1).
    NodoQ* nuevo = new NodoQ();
    nuevo->elem = t;
    nuevo->siguiente = NULL;

    if (q->ultimo == NULL){
        q->primero = nuevo;
        q->ultimo = nuevo;
    } else {
        q->ultimo->siguiente = nuevo;
        q->ultimo = nuevo;
    }

    q->cantidad++;
}

void Dequeue(Queue q){
// PROPÓSITO: Quita el primer elemento de la cola.
// COSTO: O(1).
    if (q->primero != NULL){
        NodoQ* aBorrar = q->primero;
        q->primero = q->primero->siguiente;
        delete aBorrar;
        q->cantidad--;

        if (q->primero == NULL) {
            q->ultimo = NULL;
        }
    }
}

int lengthQ(Queue q){
// PROPÓSITO: Devuelve la cantidad de elementos de la cola.
// COSTO: O(1).
    return q->cantidad;
}

void MergeQ(Queue q1, Queue q2){
// PROPÓSITO: Anexa Q2 al final de Q1, liberando la memoria inservible de Q2 en el proceso.
// NOTA: Si bien se libera memoria de Q2, no necesariamente la de sus nodos.
// COSTO: O(1).
    if (q2->primero != NULL){
        if (q1->ultimo != NULL){
            q1->ultimo->siguiente = q2->primero;
        } else {
            q1->primero = q2->primero;
        }

        q1->ultimo = q2->ultimo;
        q1->cantidad += q2->cantidad;
    }

    delete q2;
}

void DestroyQ(Queue q){
// PROPÓSITO: Libera la memoria ocupada por la cola.
// COSTO: O(N).
    NodoQ* actual = q->primero;

    while(actual != NULL){
        NodoQ* aBorrar = actual;
        actual = actual->siguiente;
        delete aBorrar;
    }

    delete q;
}