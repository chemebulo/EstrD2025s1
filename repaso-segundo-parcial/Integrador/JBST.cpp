#include <iostream>
#include "JBST.h"
using namespace std;

JerarquiaBST fundar(string p){
// PROPÓSITO: Crea la jerarquía, colocando a la persona p como presidente.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    JBSTStr* t = new JBSTStr;
    JBSTNodeStr* nuevo = new JBSTNodeStr;
    nuevo->name = p;
    nuevo->left = NULL;
    nuevo->right = NULL; 
    nuevo->parent = NULL;
    t->root = nuevo;
    return t;
}

JBSTNodeStr* find(string nombre, JBSTNodeStr* t){
// PROPÓSITO: Describe el nodo en el que está la persona del nombre dado, en la jerarquía que comienza con el nodo dado.
// PRECONDICIONES: 
//   * Existe una persona en la jerarquía con el nombre del superior dado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    JBSTNodeStr* actual = t;

    while (actual != NULL) {
        if (nombre == actual->name) {
            return actual;
        } else if (nombre < actual->name) {
            actual = actual->left;
        } else {
            actual = actual->right;
        }
    }
}

void insertar(string nuevo, string superior, JerarquiaBST t){
// PROPÓSITO: Modifica la jerarquía t agregando a la persona de nombre nuevo como subordinado directo
//            de la persona cuyo nombre es superior.
// PRECONDICIONES: 
//   * Existe una persona en la jerarquía con el nombre del superior dado.
//   * No existe una persona en la jararquía con el nombre nuevo.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).
    JBSTNodeStr* supNode = find(superior, t->root);
    JBSTNodeStr* nuevoNodo = new JBSTNodeStr;
    nuevoNodo->name = nuevo;
    nuevoNodo->left = NULL;
    nuevoNodo->right = NULL;
    nuevoNodo->parent = supNode;

    JBSTNodeStr* actual = supNode;
    JBSTNodeStr* padre = NULL;

    while (actual != NULL) {
        padre = actual;
        if (nuevo < actual->name) {
            actual = actual->left;
        } else {
            actual = actual->right;
        }
    }

    if (nuevo < padre->name) {
        padre->left = nuevoNodo;
    } else {
        padre->right = nuevoNodo;
    }
}

bool esSubordinadoDe(string empleado, string superior, JerarquiaBST bst){
// PROPÓSITO: Indica si la persona con el nombre empleado es subordinada (directa o indirectamente) de la
//            persona con el nombre superior.
// PRECONDICIONES:
//   * Existe una persona en la jararquía con el nombre del superior dado.
//   * Existe una persona en la jerarquía con el nombre del empleado dado.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).
    JBSTNodeStr* empleadoNode = find(empleado, bst->root);
    JBSTNodeStr* superiorNode = find(superior, bst->root);

    JBSTNodeStr* actual = empleadoNode;
    
    while (actual != NULL) {
        if (actual == superiorNode) {
            return true;
        }
        actual = actual->parent;
    }
    
    return false;
}