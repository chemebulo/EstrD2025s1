#include <iostream>
#include "UFSet.h"

struct UFNode{
    ELEM_TYPE valor;
    UFNode* padre;
};

UFSet createUFS(ELEM_TYPE value){
// PROPÓSITO: Inicializa el UFSet ufset, cuyo valor asociado será value.
// COSTO TIEMPO: O().
// COSTO ESPACIAL: O().
    UFNode* ufset = new UFNode;
    ufset->padre = ufset;
    return ufset;
}

UFSet findUFS(UFSet elem){
// PROPÓSITO: Encuentra el elemento distinguido para el UFSet dado.
// COSTO TIEMPO: O(n).
// COSTO ESPACIAL: O(n).
    
    if (elem->padre != elem) {
        elem->padre = findUFS(elem->padre);
    }

    return elem->padre;
}

void unionUFS(UFSet ufset1, UFSet ufset2){
// PROPÓSITO: Calcula la unión entre los conjuntos ufset1 y ufset2.
// COSTO TIEMPO: O().
// COSTO ESPACIAL: O().
    UFSet root1 = findUFS(ufset1);
    UFSet root2 = findUFS(ufset2);

    if(root1 != root2){
        root2->padre = root1;
    }
}

ELEM_TYPE elemUFS(UFSet ufset){
// PROPÓSITO: Devuelve el valor asociado a elemento de tipo UFSet.
// COSTO TIEMPO: O().
// COSTO ESPACIAL: O().
    return ufset->valor;
}