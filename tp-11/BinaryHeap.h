#include <iostream>
using namespace std;

struct BinHeapHeaderSt{
/* 
    PROPÓSITO: Representa la Heap Binaria mediante dos campos:
      * maxSize: describe el tamaño máximo que tiene la Heap Binaria. 
      * curSize: describe el tamaño actual de la Heap Binaria. 
      * elems: describe al Array que conforma la Heap Binaria.
    INV.REP.: 
      * El valor que se encuentra en "curSize" es menor al valor de "maxSize".
      * En la posición 0 del Array, debe existir un centinela que sea el elemento MÁS minimo del Array.
      * Cada valor en la Heap Binaria no puede ser menor a 0.
*/
    int maxSize;
    int curSize;
    int* elems;
};

typedef BinHeapHeaderSt* BinHeap; // INV.REP.: El puntero NO es NULL.

BinHeap emptyHeap();
// PROPÓSITO: Describe una Heap Binaria vacía.

void InsertH(int x, BinHeap h);
// PROPÓSITO: Inserta el elemento dado en la Heap Binaria dada.

bool isEmptyHeap(BinHeap h);
// PROPÓSITO: Indica si la Heap Binaria se encuentra vacía. 

int findMin(BinHeap h);
// PROPÓSITO: Devuelve el elemento mínimo de la Heap Binaria dada.
// PRECONDICIÓN: La Heap Binaria debe tener al menos un elemento.

void deleteMin(BinHeap h);
// PROPÓSITO: Elimina el elemento mínimo de la Heap Binaria dada.
// PRECONDICIÓN: La Heap Binaria debe tener al menos un elemento.