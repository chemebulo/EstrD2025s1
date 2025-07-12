#include <iostream>
#include "Queue.h"
using namespace std;

// #################################################################################################################### \\

int lengthQ(Queue q){
// PROPÓSITO: Describe la cantidad de elementos de la cola.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    return q->amount;
}

Queue copyQ(Queue q){
// PROPÓSITO: Devuelve una nueva Queue con los mismos elementos y en el mismo orden de la dada sin modificarla.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).
    Queue qCopy = emptyQ();
    Queue aux = emptyQ();

    while (!isEmptyQ(q)){
        int elem = firstQ(q);
        enqueue(qCopy, elem);
        enqueue(aux, elem);
        dequeue(q);
    }

    while (!isEmptyQ(aux)){
        enqueue(q, firstQ(aux));
        dequeue(aux);
    }

    destroyQ(aux);
    return qCopy;
}

int* queueToList(Queue q){
// PROPÓSITO: Describe una array con los elementos de la Queue dada en el orden dado.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).
    int* array = new int[lengthQ(q)];
    Queue copy = copyQ(q);

    for(int i = 0; i < lengthQ(copy); i++){
        array[i] = firstQ(copy);
        dequeue(copy);
    }

    return array;
}

Queue unionQ(Queue q1, Queue q2){
// PROPÓSITO: Describe una Queue resultante de unir ambas Queues dadas en el orden correcto.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).
    Queue qResultante = emptyQ();
    Queue copyQ1 = copyQ(q1);
    Queue copyQ2 = copyQ(q2);


    while(!isEmptyQ(copyQ1)){
        enqueue(qResultante, firstQ(copyQ1));
        dequeue(copyQ1);
    }

    while(!isEmptyQ(copyQ2)){
        enqueue(qResultante, firstQ(copyQ2));
        dequeue(copyQ2);
    }
    
    return qResultante;
}

void PrintQueue(Queue q){
// PROPÓSITO: Imprime todos los elementos en la Queue.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).
    Queue aux = emptyQ();

    while(!isEmptyQ(q)){
        int elem = firstQ(q);
        cout << elem << (lengthQ(q) > 1 ? ", " : "");
        enqueue(aux, elem);
        dequeue(q);
    }

    while(!isEmptyQ(aux)){
        enqueue(q, firstQ(aux));
        dequeue(aux);
    }

    destroyQ(aux);
}

// #################################################################################################################### \\

/* INTERFAZ QUEUE:
    * Queue emptyQ();
    * bool isEmptyQ(Queue q);
    * int firstQ(Queue q);
    * void enqueue(Queue q, int x);
    * void dequeue(Queue q);
    * void destroyQ(Queue q);
*/

// #################################################################################################################### \\

int main(){
    Queue primeraQueue = emptyQ();
    Queue segundaQueue = emptyQ();
    Queue terceraQueue = emptyQ();
    enqueue(primeraQueue, 1);
    enqueue(primeraQueue, 3);
    enqueue(primeraQueue, 5);
    enqueue(primeraQueue, 7);
    enqueue(primeraQueue, 9);
    enqueue(segundaQueue, 2);
    enqueue(segundaQueue, 4);
    enqueue(segundaQueue, 6);
    enqueue(segundaQueue, 8);
    enqueue(terceraQueue, 20);
    enqueue(terceraQueue, 30);
    enqueue(terceraQueue, 40);
    enqueue(terceraQueue, 50);
    enqueue(terceraQueue, 60);
    enqueue(terceraQueue, 70);
    cout << endl; 
    cout << "Los elementos en la primera Queue son: "; 
    PrintQueue(primeraQueue); cout << "."; 
    cout << endl;
    cout << "Los elementos en la segunda Queue son: "; 
    PrintQueue(segundaQueue); cout << "."; 
    cout << endl;
    cout << "Los elementos en la tercera Queue son: "; 
    PrintQueue(terceraQueue); cout << "."; 
    cout << endl;
    cout << endl;
    cout << "La cantidad de elementos en la primera Queue es: " << lengthQ(primeraQueue) << "." << endl;
    cout << "La cantidad de elementos en la segunda Queue es: " << lengthQ(segundaQueue) << "." << endl;
    cout << "La cantidad de elementos en la tercera Queue es: " << lengthQ(terceraQueue) << "." << endl;
    dequeue(terceraQueue);
    dequeue(terceraQueue);
    dequeue(terceraQueue);
    cout << endl;
    cout << "Despues de haber quitado elementos en la tercera Queue..." << endl;
    cout << endl;
    cout << "La cantidad de elementos en la primera Queue es: " << lengthQ(primeraQueue) << "." << endl;
    cout << "La cantidad de elementos en la segunda Queue es: " << lengthQ(segundaQueue) << "." << endl;
    cout << "La cantidad de elementos en la tercera Queue es: " << lengthQ(terceraQueue) << "." << endl;
    Queue cuartaQueue = unionQ(primeraQueue, segundaQueue);
    cout << endl;
    cout << "Despues de haber creado una cuarta Queue, la cual es una union entre la primera y la segunda..." << endl;
    cout << endl;
    cout << "Los elementos en la cuarta Queue son: "; 
    PrintQueue(cuartaQueue); cout << "."; 
    cout << endl;
    cout << endl;
    cout << "La cantidad de elementos en la primera Queue es: " << lengthQ(primeraQueue) << "." << endl;
    cout << "La cantidad de elementos en la segunda Queue es: " << lengthQ(segundaQueue) << "." << endl;
    cout << "La cantidad de elementos en la tercera Queue es: " << lengthQ(terceraQueue) << "." << endl;
    cout << "La cantidad de elementos en la cuarta Queue es: " << lengthQ(cuartaQueue) << "." << endl;
    cout << endl;
    cout << "Despues de haber eliminado la segunda y la tercera Queue queda..." << endl;
    cout << endl;
    destroyQ(segundaQueue);
    destroyQ(terceraQueue);
    cout << "La cantidad de elementos en la segunda Queue es: " << lengthQ(segundaQueue) << ". Lo cual no tiene sentido." << endl;
    cout << "La cantidad de elementos en la tercera Queue es: " << lengthQ(terceraQueue) << ". Lo cual no tiene sentido." << endl;
    return 0;
}

// #################################################################################################################### \\