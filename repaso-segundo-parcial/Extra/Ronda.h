#include <iostream>
using namespace std;

// #################################################################################################################### \\

struct RondaNode{
// INVARIANTES DE REPRESENTACIÓN:
//  * Next y Prev no pueden ser NULL.
    int value;       // Es el valor del nodo.
    RondaNode* next; // Es el nodo siguiente al nodo actual.
    RondaNode* prev; // Es el nodo anterior al nodo actual.
};

struct RondaStr{
// INVARIANTES DE REPRESENTACIÓN:
//  * La ronda no puede ser NULL.
//  * El next de cualquier nodo tiene como prev al mismo.
//  * El prev de cualquier nodo tiene como next al mismo.
//  * Existe una secuencia de nodos que parte del nodo actual y permite alcanzarlo nuevamente navegando el atributo
//    next de los nodos, y cuyo reverso permite hacer lo mismo con el atributo prev.
    RondaNode* current; // Es el nodo actual de la ronda.
};

typedef RondaStr* Ronda;

// #################################################################################################################### \\

Ronda mkRonda();
// PROPÓSITO: Crea una nueva ronda vacía.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

int current(Ronda ronda);
// PROPÓSITO: Describe el nodo actual de la ronda.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void move(int steps, Ronda ronda);
// PROPÓSITO: Mueve el puntero del nodo actual hacia adelante la cantidad de veces dada.
// COSTO TIEMPO: O(length).
// COSTO ESPACIO: O(1).

void insert(int value, Ronda ronda);
// PROPÓSITO: Añade un nuevo nodo con el valor dado a la ronda. Admite repetidos esta solución.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void remove(Ronda ronda);
// PROPÓSITO: Elimina el nodo actual de la ronda.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

int length(Ronda ronda);
// PROPÓSITO: Describe la longitud de la ronda.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).

// #################################################################################################################### \\