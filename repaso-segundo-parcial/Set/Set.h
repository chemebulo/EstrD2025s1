#include <iostream>
#include "../LinkedList/LinkedList.h"
using namespace std;

// #################################################################################################################### \\

struct NodoS {
    int elem;         // Valor del nodo.
    NodoS* siguiente; // Puntero al siguiente nodo.
};

struct SetSt {
// INVARIANTES DE REPRESENTACIÓN: 
//  * Cantidad describe la cantidad de nodos que se pueden recorrer desde primero 
//    por siguiente hasta alcanzar a NULL, siendo cada elemento único.
//  * Primero debe apuntar al primer nodo del Set.
    int cantidad;   // Cantidad de elementos diferentes.
    NodoS* primero; // Puntero al primer nodo.
};

typedef SetSt* Set;

// #################################################################################################################### \\

Set emptyS();
// PROPÓSITO: Crea un conjunto vacío.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

bool isEmptyS(Set s);
// PROPÓSITO: Indica si el conjunto está vacío.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

bool belongsS(int x, Set s);
// PROPÓSITO: Indica si el elemento pertenece al conjunto.
// COSTO TIEMPO: O(n).
// COSTO ESPACIO: O(1).

void AddS(int x, Set s);
// PROPÓSITO: Agrega un elemento al conjunto.
// COSTO TIEMPO: O(n).
// COSTO ESPACIO: O(1).

void RemoveS(int x, Set s);
// PROPÓSITO: Quita un elemento dado.
// COSTO TIEMPO: O(n).
// COSTO ESPACIO: O(1).

int sizeS(Set s);
// PROPÓSITO: Devuelve la cantidad de elementos.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

LinkedList setToList(Set s);
// PROPÓSITO: Devuelve una lista con los elementos del conjunto.
// COSTO TIEMPO: O(n^2).
// COSTO ESPACIO: O(1).

void DestroyS(Set s);
// PROPÓSITO: Libera la memoria ocupada por el conjunto.
// COSTO TIEMPO: O(n).
// COSTO ESPACIO: O(1).