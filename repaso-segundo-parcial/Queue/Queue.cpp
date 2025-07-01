#include <iostream>
#include "Queue.h"
using namespace std;

// #################################################################################################################### \\

Queue emptyQ(){
// PROPÓSITO: Crea una cola vacía.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante, ya que solamente crea un registro y se le asignan valores, lo cual es constante en cualquier caso.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante, ya que solamente se utiliza un frame en la memoria stack en cualquier caso.
    QueueSt* q = new QueueSt;
    q->cantidad = 0;
    q->primero = NULL;
    q->ultimo = NULL;
    return q;
}

bool isEmptyQ(Queue q){
// PROPÓSITO: Indica si la cola está vacía.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante, ya que solamente se accede a un campo del registro, lo cual es constante en cualquier caso.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante, ya que solamente se utiliza un frame en la memoria stack en cualquier caso.
    return q->cantidad == 0;
}

int firstQ(Queue q){
// PROPÓSITO: Devuelve el primer elemento de la cola.
// PRECONDICIÓN: La cola no está vacía.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante, ya que solamente se accede a un campo del registro, lo cual es constante en cualquier caso.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante, ya que solamente se utiliza un frame en la memoria stack en cualquier caso.
    return q->primero->elem;
}

void Enqueue(int x, Queue q){
// PROPÓSITO: Agrega un elemento al final de la cola.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante, ya que solamente se realiza una comparación y se modifican campos del registro, lo cual
    // es constante en cualquier caso.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante, ya que solamente se utiliza un frame en la memoria stack en cualquier caso.
    NodoQ* nuevo = new NodoQ;
    nuevo->elem = x;
    nuevo->siguiente = NULL;

    if(q->primero == NULL){
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
// COSTO TIEMPO: O(1).
    // Siendo de costo constante, ya que solamente se realiza una comparación y se modifican campos del registro, lo cual
    // es constante en cualquier caso.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante, ya que solamente se utiliza un frame en la memoria stack en cualquier caso.
    if(q->primero != NULL){
        NodoQ* borrar = q->primero;
        q->primero = q->primero->siguiente;
        delete borrar;
        q->cantidad--;
        
        if(q->primero == NULL){
            q->ultimo = q->primero;
        }
    }
}

int lengthQ(Queue q){
// PROPÓSITO: Devuelve la cantidad de elementos de la cola.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante, ya que solamente se accede a un campo del registro, lo cual es constante en cualquier caso.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante, ya que solamente se utiliza un frame en la memoria stack en cualquier caso.
    return q->cantidad;
}

void MergeQ(Queue q1, Queue q2){
// PROPÓSITO: Anexa Q2 al final de Q1, liberando la memoria inservible de Q2 en el proceso.
// NOTA: Si bien se libera memoria de Q2, no necesariamente la de sus nodos.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante, ya que solamente se realiza una comparación y se modifican campos del registro, lo cual
    // es constante en cualquier caso.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante, ya que solamente se utiliza un frame en la memoria stack en cualquier caso.
    if(q1->primero == NULL){
        q1->primero = q2->primero;
        q1->ultimo = q2->ultimo;
    } else {
        q1->ultimo->siguiente = q2->primero;
        q1->ultimo = q2->ultimo;
    }
    q1->cantidad += q2->cantidad;
    delete q2;
}

void DestroyQ(Queue q){
// PROPÓSITO: Libera la memoria ocupada por la cola.
// COSTO TIEMPO: O(n).
    // Siendo de costo lineal, ya que por cada elemento de la cola se realiza una comparación y una asignación de valor
    // de la variable "temp", ambas de costo constante.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante, ya que solamente se utiliza un frame en la memoria stack en cualquier caso.
    NodoQ* temp = q->primero;

    while(temp != NULL){
        NodoQ* borrar = temp;
        temp = temp->siguiente;
        delete borrar;
    }

    delete q;
}

// #################################################################################################################### \\