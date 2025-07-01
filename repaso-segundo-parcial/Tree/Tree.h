#include <iostream>
using namespace std;

struct NodeT {
    int elem;     // Es el elemento que se encuentra en el nodo.
    NodeT* left;  // Es el nodo izquierdo del nodo.
    NodeT* right; // Es el nodo derecho del nodo.
};

typedef NodeT* Tree;

// #################################################################################################################### \\

Tree emptyT();
// PROPÓSITO: Describe un árbol vacío.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

Tree nodeT(int elem, Tree left, Tree right);
// PROPÓSITO: Describe un nodo de un árbol con sus partes.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

bool isEmptyT(Tree t);
// PROPÓSITO: Indica si el árbol está vacío.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

int rootT(Tree t);
// PROPÓSITO: Describe la raíz del árbol dado.
// PRECONDICIÓN: El árbol no está vacío.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

Tree left(Tree t);
// PROPÓSITO: Describe el nodo izquierdo del árbol dado.
// PRECONDICIÓN: El árbol no está vacío.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

Tree right(Tree t);
// PROPÓSITO: Describe el nodo derecho del árbol dado.
// PRECONDICIÓN: El árbol no está vacío.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

// #################################################################################################################### \\