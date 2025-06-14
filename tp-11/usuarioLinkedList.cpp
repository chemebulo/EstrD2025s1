#include <iostream>
#include "LinkedList.h"
using namespace std;

int sumatoria(LinkedList xs){
// PROPÓSITO: Devuelve la suma de todos los elementos.
// COSTO: O(N).
// Siendo N la cantidad de elementos en la LinkedList, por cada N se realizan las operaciones "current" y "Next" de costo
// constante. Es por eso que el costo total de la función es lineal. 
    int resultado = 0;
    ListIterator ixs = getIterator(xs);

    while(!atEnd(ixs)){
        resultado += current(ixs);
        Next(ixs);
    }

    DisposeIterator(ixs);
    return resultado;
}

void Sucesores(LinkedList xs){
// PROPÓSITO: Incrementa en uno todos los elementos.
// COSTO: O(N).
// Siendo N la cantidad de elementos en la LinkedList, por cada N se realizan las operaciones "current", "SetCurrent" y 
// "Next" de costo constante. Es por eso que el costo total de la función es lineal.
    ListIterator ixs = getIterator(xs);
    
    while(!atEnd(ixs)){
        SetCurrent(current(ixs) + 1, ixs);
        Next(ixs);
    }

    DisposeIterator(ixs);
}

bool pertenece(int x, LinkedList xs){
// PROPÓSITO: Indica si el elemento pertenece a la lista.
// COSTO: O(N).
// Siendo N la cantidad de elementos en la LinkedList, por cada N se realizan las operaciones "current", "atEnd" y 
// "Next" de costo constante. Es por eso que el costo total de la función es lineal.
    ListIterator ixs = getIterator(xs);
    
    while(!atEnd(ixs)){
        if(current(ixs) == x){
            break;
        }
        Next(ixs);
    }

    bool encontrado = !atEnd(ixs) && current(ixs) == x;
    DisposeIterator(ixs);
    return encontrado;
}

int apariciones(int x, LinkedList xs){
// PROPÓSITO: Indica la cantidad de elementos iguales a x.
// COSTO: O(N).
// Siendo N la cantidad de elementos en la LinkedList, por cada N se realizan las operaciones "current", "atEnd" y 
// "Next" de costo constante. Es por eso que el costo total de la función es lineal.
    int apariciones = 0;
    ListIterator ixs = getIterator(xs);
    
    while(!atEnd(ixs)){
        if(current(ixs) == x){
            apariciones++;
        }
        Next(ixs);
    }

    DisposeIterator(ixs);
    return apariciones;
}

int minimo(LinkedList xs){
// PROPÓSITO: Devuelve el elemento más chico de la lista.
// PRECONDICIÓN: La lista no es vacía.
// COSTO: O(N).
// Siendo N la cantidad de elementos en la LinkedList, por cada N se realizan las operaciones "current", "atEnd" y 
// "Next" de costo constante. Es por eso que el costo total de la función es lineal.
    ListIterator ixs = getIterator(xs);
    int minimo = current(ixs);

    while(!atEnd(ixs)){
        if(current(ixs) < minimo){
            minimo = current(ixs);
        }
        Next(ixs);
    }

    DisposeIterator(ixs);
    return minimo;
}

LinkedList copy(LinkedList xs){
// PROPÓSITO: Dada una lista genera otra con los mismos elementos, en el mismo orden.
// COSTO: O(N).
// Siendo N la cantidad de elementos en la LinkedList, por cada N se realizan las operaciones "atEnd", "current", "next"
// y "Snoc" de costo constante. Es por eso que el costo total de la función es lineal.
    ListIterator ixs = getIterator(xs);
    LinkedList ys = nil(); 

    while(!atEnd(ixs)){
        Snoc(current(ixs), ys);
        Next(ixs);
    }

    DisposeIterator(ixs);
    return ys;
}

void Append(LinkedList xs, LinkedList ys){
// PROPÓSITO: Agrega todos los elementos de la segunda lista al final de los de la primera. La segunda lista se destruye.
// COSTO: O(N).
// Siendo N la cantidad de elementos en la LinkedList, por cada N se realizan las operaciones "atEnd", "current", "next"
// y "Snoc" de costo constante. Es por eso que el costo total de la función es lineal.
    ListIterator ixs = getIterator(ys);

    while(!atEnd(ixs)){
        Snoc(current(ixs), xs);
        Next(ixs);
    }

    DisposeIterator(ixs);
    DestroyL(ys);
}

/* INTERFAZ DE LINKEDLIST:
    * LinkedList nil()
    * bool isEmpty(LinkedList xs)
    * int head(LinkedList xs)
    * void Cons(int x, LinkedList xs)
    * void Tail(LinkedList xs)
    * int length(LinkedList xs)
    * void Snoc(int x, LinkedList xs)
    * ListIterator getIterator(LinkedList xs)
    * int current(ListIterator ixs)
    * void SetCurrent(int x, ListIterator ixs)
    * void Next(ListIterator ixs)
    * bool atEnd(ListIterator ixs)
    * void DisposeIterator(ListIterator ixs)
    * void DestroyL(LinkedList xs)
*/

int main() {
    LinkedList lista1 = nil();
    LinkedList lista2 = nil();
    Snoc(4, lista1);
    Snoc(10, lista1);
    Snoc(22, lista1);
    Snoc(55, lista1);
    Snoc(1, lista2);
    Snoc(15, lista2);
    Snoc(25, lista2);
    Snoc(25, lista2);
    Snoc(25, lista2);
    cout << "El primer elemento de la primera lista es " << head(lista1) << "." << endl;
    cout << "El primer elemento de la segunda lista es " << head(lista2) << "." << endl;
    Tail(lista1);
    Tail(lista2);
    cout << "El despues de quitar primer elemento de la primera lista, su primer elemento es " << head(lista1) << "." << endl;
    cout << "El despues de quitar primer elemento de la segunda lista, su primer elemento es " << head(lista2) << "." << endl;
    Sucesores(lista1);
    Sucesores(lista2);
    cout << endl;
    cout << "El sucesor despues de quitar primer elemento de la primera lista, su primer elemento es " << head(lista1) << "." << endl;
    cout << "El sucesor despues de quitar primer elemento de la segunda lista, su primer elemento es " << head(lista2) << "." << endl;
    cout << endl;
    cout << "La longitud de la primera lista es de " << length(lista1) << "." << endl;
    cout << "La longitud de la segunda lista es de " << length(lista2) << "." << endl;
    cout << endl;
    cout << "La sumatoria de la primera lista es " << sumatoria(lista1) << "." << endl;
    cout << "La sumatoria de la segunda lista es " << sumatoria(lista2) << "." << endl;
    cout << endl;
    cout << boolalpha << "Existe el 11 en la primera lista: " << pertenece(11, lista1) << "." << endl;
    cout << boolalpha << "Existe el 20 en la segunda lista: " << pertenece(20, lista2) << "." << endl;
    cout << endl;
    cout << "La cantidad de apariciones del 56 en la primera lista " << apariciones(56, lista1) << "." << endl;
    cout << "La cantidad de apariciones del 26 en la segunda lista " << apariciones(26, lista2) << "." << endl;
    cout << endl;
    cout << "El minimo de la primera lista es " << minimo(lista1) << "." << endl;
    cout << "El minimo de la segunda lista es " << minimo(lista2) << "." << endl;
    cout << endl;
    Append(lista1, lista2);
    cout << "La longitud de la primera lista es de " << length(lista1) << "." << endl;
    cout << "La sumatoria de la primera lista es " << sumatoria(lista1) << "." << endl;
    cout << "La longitud de la segunda lista es de " << length(lista2) << "." << endl;
    return 0;
}