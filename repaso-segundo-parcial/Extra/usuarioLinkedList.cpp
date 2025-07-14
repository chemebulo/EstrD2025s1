#include <iostream>
#include "LinkedList.h"
using namespace std;

// #################################################################################################################### \\

// IMPLEMENTACIÓN FUNCIONAL:

int sumar(LinkedList xs){
// PROPÓSITO: Describe la suma de todos los elementos de la LinkedList dada.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).
    ListIterator ixs = getIterator(xs);
    int sumatoria = 0;

    while(!atEnd(ixs)){
        sumatoria += current(ixs);
        Next(ixs);
    }

    DisposeIterator(ixs);
    return sumatoria;
}

LinkedList mapSucc(LinkedList xs){
// PROPÓSITO: Describe una LinkedList donde cada elemento es el sucesor del correspondiente la LinkedList dada.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(N).
    LinkedList ys = nil();
    ListIterator ixs = getIterator(xs);

    while(!atEnd(ixs)){
        Snoc(current(ixs) + 1, ys);
        Next(ixs);
    }

    DisposeIterator(ixs);
    return ys;
}

LinkedList take(int length, LinkedList xs){
// PROPÓSITO: Describe una LinkedList con los primeros length elementos de la LinkedList dada.
// COSTO TIEMPO: O(length).
// COSTO ESPACIO: O(N).
    LinkedList ys = nil();
    ListIterator ixs = getIterator(xs);

    while(!atEnd(ixs) && length > 0){
        Snoc(current(ixs), ys);
        Next(ixs);
        length--;
    }

    DisposeIterator(ixs);
    return ys;
}

LinkedList drop(int length, LinkedList xs){
// PROPÓSITO: Describe una LinkedList sin los primeros length elementos de la LinkedList dada.
// COSTO TIEMPO: O(length).
// COSTO ESPACIO: O(N).
    LinkedList ys = nil();
    ListIterator ixs = getIterator(xs);

    // Avanzo el iterador hasta la posición dada.
    while(length > 0 && !atEnd(ixs)){
        Next(ixs);
        length--;
    }

    // Copio todo desde la posición length dada.
    while(!atEnd(ixs)){
        Snoc(current(ixs), ys);
        Next(ixs);
    }

    DisposeIterator(ixs);
    return ys;
}

void PrintLinkedList(LinkedList xs){
// PROPÓSITO: Imprime todos los valores de la LinkedList dada.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).
    ListIterator ixs = getIterator(xs);

    if(!atEnd(ixs)){
        cout << current(ixs);
        Next(ixs);
    }

    while(!atEnd(ixs)){
        cout << ", " << current(ixs);
        Next(ixs);
    }

    cout << "." << endl;
    DisposeIterator(ixs);
}

// #################################################################################################################### \\

// IMPLEMENTACIÓN DESTRUCTIVA:

int sumarDSTR(LinkedList xs){
// PROPÓSITO: Describe la suma de todos los elementos de la LinkedList dada.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).
    int sumatoria = 0;

    while(!isEmpty(xs)){
        sumatoria += head(xs);
        Tail(xs);
    }

    return sumatoria;
}

int lengthDSTR(LinkedList xs){
// PROPÓSITO: Describe la longitud de la LinkedList dada.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).
    int longitud = 0;

    while(!isEmpty(xs)){
        longitud++;
        Tail(xs);
    }

    return longitud;
}

void mapSuccDSTR(LinkedList xs){
// PROPÓSITO: Describe la LinkedList dada con el sucesor de cada elemento.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).
    ListIterator ixs = getIterator(xs);

    while(!atEnd(ixs)){
        SetCurrent(current(ixs) + 1, ixs);
        Next(ixs);
    }

    DisposeIterator(ixs);
}

void takeDSTR(int lengthP, LinkedList xs){
// PROPÓSITO: Describe la LinkedList dada con los primeros length elementos.
// COSTO TIEMPO: O(length).
// COSTO ESPACIO: O(N).
    if(!isEmpty(xs)){

        // Se mueven hacia el fondo los primeros length elementos a conservar.
        for(int i = 0; i < lengthP && !isEmpty(xs); i++){
            Snoc(head(xs), xs);
            Tail(xs);
        }

        int borrar = length(xs) - lengthP;

        // Se borran todos los elementos de la LinkedList hasta volver a encontrar al antigüo primero.
        for(int i = 0; i < borrar; i++){
            Tail(xs);
        }
    }
}

void dropDSTR(int length, LinkedList xs){
// PROPÓSITO: Describe la LinkedList dada sin los primeros length elementos.
// COSTO TIEMPO: O(length).
// COSTO ESPACIO: O(N).

    // Se eliminan los primeros length elementos de la LinkedList.
    for(int i = 0; i < length && !isEmpty(xs); i++){
        Tail(xs);
    }
}

// #################################################################################################################### \\

/* INTERFAZ LINKEDLIST:
    * LinkedList nil();
    * bool isEmpty(LinkedList xs);
    * int head(LinkedList xs);
    * void Cons(int x, LinkedList xs);
    * void Tail(LinkedList xs);
    * int length(LinkedList xs);
    * void Snoc(int x, LinkedList xs);

    * ListIterator getIterator(LinkedList xs);
    * int current(ListIterator ixs);
    * void SetCurrent(int x, ListIterator ixs);
    * void Next(ListIterator ixs);
    * bool atEnd(ListIterator ixs);
    * void DisposeIterator(ListIterator ixs);
    * void DestroyL(LinkedList xs);
*/

// #################################################################################################################### \\

int main(){
    LinkedList lista1 = nil();
    LinkedList lista2 = nil();
    Snoc(2, lista1);
    Snoc(4, lista1);
    Snoc(6, lista1);
    Snoc(8, lista1);
    Snoc(5, lista2);
    Snoc(10, lista2);
    Snoc(15, lista2);
    Snoc(15, lista2);
    Snoc(15, lista2);
    cout << endl;
    cout << "El primer elemento de la primera lista es " << head(lista1) << "." << endl;
    cout << "El primer elemento de la segunda lista es " << head(lista2) << "." << endl;
    cout << endl;
    cout << "Los elementos de la primera lista son: "; PrintLinkedList(lista1);
    cout << "Los elementos de la segunda lista son: "; PrintLinkedList(lista2);
    cout << endl;
    cout << "La longitud de la primera lista es de " << length(lista1) << "." << endl;
    cout << "La longitud de la segunda lista es de " << length(lista2) << "." << endl;
    Tail(lista1);
    Tail(lista2);
    cout << endl;
    cout << "Despues de quitar un elemento en ambas listas queda..." << endl;
    cout << endl;
    cout << "Los elementos de la primera lista son: "; PrintLinkedList(lista1);
    cout << "Los elementos de la segunda lista son: "; PrintLinkedList(lista2);
    cout << endl;
    cout << "La longitud de la primera lista es de " << length(lista1) << "." << endl;
    cout << "La longitud de la segunda lista es de " << length(lista2) << "." << endl;
    cout << endl;
    mapSuccDSTR(lista1);
    mapSuccDSTR(lista2);
    cout << "Se actualizaron todos los elementos de la primera lista a sus sucesores, su primer elemento es " << head(lista1) << "." << endl;
    cout << "Se actualizaron todos los elementos de la segunda lista a sus sucesores, su primer elemento es " << head(lista2) << "." << endl;
    cout << endl;
    cout << "Los elementos de la primera lista son: "; PrintLinkedList(lista1);
    cout << "Los elementos de la segunda lista son: "; PrintLinkedList(lista2);
    cout << endl;
    cout << "Despues de quitar los primeros dos en la primera lista y los ultimos dos de la segunda lista queda..." << endl;
    takeDSTR(2, lista1);
    dropDSTR(2, lista2);
    cout << endl;
    cout << "Los elementos de la primera lista son: "; PrintLinkedList(lista1);
    cout << "Los elementos de la segunda lista son: "; PrintLinkedList(lista2);
    cout << endl;
    cout << "La sumatoria de la primera lista es " << sumar(lista1) << "." << endl;
    cout << "La sumatoria de la segunda lista es " << sumar(lista2) << "." << endl;
    cout << endl;
    return 0;
}

// #################################################################################################################### \\