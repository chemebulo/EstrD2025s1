#include <iostream>
#include "BinaryHeap.h"
using namespace std;

BinHeap crearHeap(int* elements, int cant){
// PROPÓSITO: Crea una Heap Binaria a partir del Array dado con la capacidad dada.
// PRECONDICIÓN: Los elementos del Array dado tienen que estar ubicados a partir de la primera 
//               posición del mismo y sin dejar espacios vacíos entremedio.
// COSTO O(N log N).
    BinHeapHeaderSt* h = emptyHeap();

    for(int i = 0; i < cant; i++) {
        InsertH(elements[i], h);
    }

    return h;
}

/* INTERFAZ DE BINARY HEAP:
    * BinHeap emptyHeap();
    * void InsertH(int x, BinHeap h);
    * bool isEmptyHeap(BinHeap h);
    * int findMin(BinHeap h);
    * void deleteMin(BinHeap h);
*/

int main(){
    BinHeap h = emptyHeap();
    cout << boolalpha << "La Heap Binaria se encuentra vacia: " << isEmptyHeap(h) << endl;
    InsertH(50, h);
    InsertH(4, h);
    InsertH(7, h);
    InsertH(21, h);
    InsertH(18, h);
    InsertH(99, h);
    InsertH(2, h);
    cout << endl;
    cout << boolalpha << "Despues de haber agregado elementos, la Heap Binaria se encuentra vacia: " << isEmptyHeap(h) << endl;
    cout << endl;
    cout << "El elemento minimo en la Heap Binaria es " << findMin(h) << "." << endl;
    cout << endl;
    deleteMin(h);
    deleteMin(h);
    cout << "Despues de haber borrado dos veces el minimo, el elemento minimo en la Heap Binaria es " << findMin(h) << "." << endl;
    return 0;
}