#include <iostream>
#include "LinkedList.h"
using namespace std;

struct NodoS {
/*
    PROPÓSITO: Representa a un nodo mediante dos campos: 
        * elem: indica el valor que tiene el nodo.
        * siguiente: describe el puntero al siguiente nodo.
*/
    int elem;
    NodoS* siguiente;
};

struct SetSt {
/* 
    PROPÓSITO: Representa el Set mediante dos campos:
      * cantidad: indica la cantidad de elementos. 
      * primero: describe el puntero al primer nodo. 
    INV.REP.: 
      * Cantidad describe la cantidad de nodos que se pueden recorrer desde primero 
        por siguiente hasta alcanzar a NULL, siendo cada elemento único.
      * Primero debe apuntar al primer nodo del Set.
*/
    int cantidad;
    NodoS* primero;
};

typedef SetSt* Set;

Set emptyS();
// PROPÓSITO: Crea un conjunto vacío.

bool isEmptyS(Set s);
// PROPÓSITO: Indica si el conjunto está vacío.

bool belongsS(int x, Set s);
// PROPÓSITO: Indica si el elemento pertenece al conjunto.

void AddS(int x, Set s);
// PROPÓSITO: Agrega un elemento al conjunto.

void RemoveS(int x, Set s);
// PROPÓSITO: Quita un elemento dado.

int sizeS(Set s);
// PROPÓSITO: Devuelve la cantidad de elementos.

LinkedList setToList(Set s);
// PROPÓSITO: Devuelve una lista con los elementos del conjunto.

void DestroyS(Set s);
// PROPÓSITO: Libera la memoria ocupada por el conjunto.