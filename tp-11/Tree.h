#include <iostream>
using namespace std;

struct NodeT {
    /* 
    PROPÓSITO: Representa el Nodo mediante tres campos:
      * elem: describe el elemento que se encuentra en el nodo. 
      * left: describe el nodo izquierdo del nodo. 
      * right: describe el nodo derecho del nodo. 
    INV.REP.: 
      * Ninguna.
*/
    int elem;
    NodeT* left;
    NodeT* right;
};

typedef NodeT* Tree;

Tree emptyT();
// PROPÓSITO: Describe un árbol vacío.

Tree nodeT(int elem, Tree left, Tree right);
// PROPÓSITO: Describe un nodo de un árbol con sus partes.

bool isEmptyT(Tree t);
// PROPÓSITO: Indica si el árbol dado está vacío.

int rootT(Tree t);
// PROPÓSITO: Describe la raíz del árbol dado.
// PRECONDICIÓN: El árbol no está vacío.

Tree left(Tree t);
// PROPÓSITO: Describe el nodo izquierdo del árbol dado.
// PRECONDICIÓN: El árbol no está vacío.

Tree right(Tree t);
// PROPÓSITO: Describe el nodo derecho del árbol dado.
// PRECONDICIÓN: El árbol no está vacío.