#include <iostream>
#include "BinaryHeap.h"
using namespace std;

BinHeap emptyHeap(){
// PROPÓSITO: Describe una Heap Binaria vacía.
// COSTO O(1).
    BinHeapHeaderSt* h = new BinHeapHeaderSt();
    h->curSize = 0;
    h->maxSize = 16;
    h->elems = new int[h->maxSize];
    h->elems[0] = 0; // A criterio, aunque en este caso se utiliza este.
    return h;
}

void AumentarEspacio(BinHeap h){
    int* newElements = new int[h->maxSize * 2];

    for(int i = 0; i <= h->curSize; i++){
        newElements[i] = h->elems[i];
    }

    delete[] h->elems;
    h->maxSize *= 2;
    h->elems = newElements;
}

void InsertH(int x, BinHeap h){
// PROPÓSITO: Inserta el elemento dado en la Heap Binaria dada.
// COSTO O(log N).
    if (h->curSize == h->maxSize - 1){
        AumentarEspacio(h);
    }

    int curNode = ++h->curSize;

    while(x < h->elems[curNode / 2]){
        h->elems[curNode] = h->elems[curNode / 2];
        curNode /= 2;
    }

    h->elems[curNode] = x;
}

bool isEmptyHeap(BinHeap h){
// PROPÓSITO: Indica si la Heap Binaria se encuentra vacía. 
// COSTO O(1).
    return h->curSize == 0;
}

int findMin(BinHeap h){
// PROPÓSITO: Devuelve el elemento mínimo de la Heap Binaria dada.
// PRECONDICIÓN: La Heap Binaria debe tener al menos un elemento.
// COSTO O(1).
    return h->elems[1];
}

void deleteMin(BinHeap h){
// PROPÓSITO: Elimina el elemento mínimo de la Heap Binaria dada.
// PRECONDICIÓN: La Heap Binaria debe tener al menos un elemento.
// COSTO O(log N).
    int child;
    int curNode;
    int last = h->elems[h->curSize--];

    for(curNode = 1; curNode * 2 <= h->curSize; curNode = child){
        child = curNode * 2;

        if ((child != h->curSize) && (h->elems[child + 1] < h->elems[child])){
            child++;
        }

        if (last > h->elems[child]){
            h->elems[curNode] = h->elems[child];
        } else {
            break;
        }
    }

    h->elems[curNode] = last;
}