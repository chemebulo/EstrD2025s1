#include <iostream>
#include "Queue.h"
using namespace std;

// #################################################################################################################### \\

Queue emptyQ(){
// PROPÓSITO: Describe una Queue vacía.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    QueueStr* q = new QueueStr;
    q->first = NULL;
    q->last = NULL;
    q->amount = 0;
    return q;
}

bool isEmptyQ(Queue q){
// PROPÓSITO: Indica si la Queue dada se encuentra vacía.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    return q->first == NULL;
}

int firstQ(Queue q){
// PROPÓSITO: Describe el primer nodo de la Queue dada.
// PRECONDICIÓN: La Queue no se encuentra vacía.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    return q->first->value;
}

void enqueue(Queue q, int x){
// PROPÓSITO: Coloca en la Queue dada al nodo con el valor dado en la última posición de la misma.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    QueueNode* nuevo = new QueueNode;
    nuevo->value = x;
    nuevo->next = NULL;
    
    if(q->first != NULL){
        // En caso que haya al menos un nodo en la Queue, se lo agrega al final.
        q->last->next = nuevo;
        q->last = nuevo;
    } else {
        // En caso que no haya ningún nodo en la Queue, se lo agrega como primero y último de la misma.
        q->first = nuevo;  
        q->last = nuevo;
    }

    q->amount++;
}

void dequeue(Queue q){
// PROPÓSITO: Quita un nodo de la Queue dada, siendo el primero de la misma.
// PRECONDICIÓN: La Queue no se encuentra vacía.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    if(q->first != NULL){
        // En caso que haya al menos un nodo en la Queue, se lo quita normalmente.
        QueueNode* temp = q->first;
        
        if(q->first == q->last){
            // En caso que haya solo un nodo en la Queue, el first y last es un puntero NULL.
            q->first = NULL;
            q->last = NULL;
        } else {
            // En caso que haya más que un nodo en la Queue, solamente se actualiza el first.            
            q->first = q->first->next;
        }
        
        delete temp;
        q->amount--;
    }
    // En caso que no haya ningún nodo en la Queue, se deja como está.
}

void destroyQ(Queue q){
// PROPÓSITO: Borra la Queue dada.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).
    QueueNode* temp;
    
    while(q->first != NULL){
        // En caso de que la Queue no esté vacía, se eliminan todos los nodos de la misma.
        temp = q->first;
        q->first = q->first->next;
        delete temp;
    }

    q->amount = 0;
    delete q;
}

// #################################################################################################################### \\