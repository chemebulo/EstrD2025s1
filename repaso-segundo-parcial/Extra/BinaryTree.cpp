#include <iostream>
#include "BinaryTree.h"
using namespace std;

// #################################################################################################################### \\

Tree emptyT(){
// PROPÓSITO: Describe un árbol vacío.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    return NULL;
}

Tree nodeT(int elem, Tree left, Tree right){
// PROPÓSITO: Describe un nodo de un árbol con sus partes.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    NodeT* t = new NodeT;
    t->elem = elem;
    t->left = left;
    t->right = right;
    return t;
}

bool isEmptyT(Tree t){
// PROPÓSITO: Indica si el árbol dado está vacío.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    return t == NULL;
}

int rootT(Tree t){
// PROPÓSITO: Describe la raíz del árbol dado.
// PRECONDICIÓN: El árbol no está vacío.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    return t->elem;
}

Tree left(Tree t){
// PROPÓSITO: Describe el nodo izquierdo del árbol dado.
// PRECONDICIÓN: El árbol no está vacío.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    return t->left;
}

Tree right(Tree t){
// PROPÓSITO: Describe el nodo derecho del árbol dado.
// PRECONDICIÓN: El árbol no está vacío.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    return t->right;
}

// #################################################################################################################### \\