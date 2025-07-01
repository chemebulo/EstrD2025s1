#include <iostream>
#include "Tree.h"
using namespace std;

// #################################################################################################################### \\

Tree emptyT(){
// PROPÓSITO: Describe un árbol vacío.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante, ya que solamente se devuelve un puntero nulo, lo cual en cualquier caso es constante.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante, ya que solamente se utiliza un frame en la memoria stack en cualquier caso.
    return NULL;
}

Tree nodeT(int elem, Tree left, Tree right){
// PROPÓSITO: Describe un nodo de un árbol con sus partes.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante, ya que solamente se inicializa y se modifican los campos de un registro, lo cual en cualquier caso es constante.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante, ya que solamente se utiliza un frame en la memoria stack en cualquier caso.
    NodeT* t = new NodeT;
    t->elem = elem;
    t->left = left;
    t->right = right;
    return t;
}

bool isEmptyT(Tree t){
// PROPÓSITO: Indica si el árbol está vacío.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante, ya que solamente se realiza una comparación, lo cual en cualquier caso es constante.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante, ya que solamente se utiliza un frame en la memoria stack en cualquier caso.
    return t == NULL;
}

int rootT(Tree t){
// PROPÓSITO: Describe la raíz del árbol dado.
// PRECONDICIÓN: El árbol no está vacío.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante, ya que solamente se accede a un campo del registro, lo cual en cualquier caso es constante.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante, ya que solamente se utiliza un frame en la memoria stack en cualquier caso.
    return t->elem;
}

Tree left(Tree t){
// PROPÓSITO: Describe el nodo izquierdo del árbol dado.
// PRECONDICIÓN: El árbol no está vacío.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante, ya que solamente se accede a un campo del registro, lo cual en cualquier caso es constante.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante, ya que solamente se utiliza un frame en la memoria stack en cualquier caso.
    return t->left;
}

Tree right(Tree t){
// PROPÓSITO: Describe el nodo derecho del árbol dado.
// PRECONDICIÓN: El árbol no está vacío.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante, ya que solamente se accede a un campo del registro, lo cual en cualquier caso es constante.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante, ya que solamente se utiliza un frame en la memoria stack en cualquier caso.
    return t->right;
}

// #################################################################################################################### \\