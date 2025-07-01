#include <iostream>
#include "Queue.h"

// #################################################################################################################### \\

/* INTERFAZ DE QUEUE:
    * Queue emptyQ()
    * bool isEmptyQ(Queue q)
    * int firstQ(Queue q)
    * void Enqueue(int x, Queue q)
    * void Dequeue(Queue q)
    * int lengthQ(Queue q)
    * void MergeQ(Queue q1, Queue q2)
    * void DestroyQ(Queue q)
*/

int main(){
    Queue q1 = emptyQ();
    Queue q2 = emptyQ();
    cout << "La longitud de la primera cola es " << lengthQ(q1) << "." << endl; 
    cout << "La longitud de la segunda cola es " << lengthQ(q2) << "." << endl; 
    Enqueue(5, q1);
    cout << endl;
    cout << boolalpha << "La primera cola esta vacia: " << isEmptyQ(q1) << endl; 
    cout << boolalpha << "La segunda cola esta vacia: " << isEmptyQ(q2) << endl; 
    Enqueue(15, q1);
    Enqueue(25, q1);
    Enqueue(4, q2);
    Enqueue(3, q2);
    Enqueue(2, q2);
    Enqueue(1, q2);
    cout << endl;
    cout << "El primer elemento de la primera cola es " << firstQ(q1) << "." << endl;
    cout << "El primer elemento de la segunda cola es " << firstQ(q2) << "." << endl;
    cout << endl;
    cout << "Despues de haber agregado elementos, la longitud de la primera cola es " << lengthQ(q1) << "." << endl; 
    cout << "Despues de haber agregado elementos, la longitud de la segunda cola es " << lengthQ(q2) << "." << endl; 
    cout << endl;
    Dequeue(q1);
    cout << "Despues de haber quitado un elemento, la longitud de la primera cola es " << lengthQ(q1) << "." << endl; 
    cout << "El primer elemento de la primera cola es " << firstQ(q1) << "." << endl;
    cout << endl;
    MergeQ(q1, q2);
    cout << "Despues de haber mergeado ambas colas, la longitud de la cola resultante es " << lengthQ(q1) << "." << endl; 
    cout << "Despues de haber mergeado ambas colas, la longitud de la segunda cola es " << lengthQ(q2) << ", lo cual no tiene sentido porque fue borrada." << endl; 
    cout << endl;
    DestroyQ(q1);
    cout << "La longitud de la primera cola es " << lengthQ(q1) << ", lo cual no tiene sentido porque fue borrada." << endl;
    return 0;
}