#include <iostream>
#include "UFSet.h"

struct UFNode{
    ELEM_TYPE valor;
    UFNode* padre;
    int rango;
};

UFSet createUFS(ELEM_TYPE value){
// PROPÓSITO: Inicializa el UFSet ufset, cuyo valor asociado será value.
// COSTO TIEMPO: O(1).
// COSTO ESPACIAL: O(1).
    UFNode* ufset = new UFNode;
    ufset->valor = value;
    ufset->padre = ufset;
    ufset->rango = 0;
    return ufset;
}

UFSet findUFS(UFSet elem){
// PROPÓSITO: Encuentra el elemento distinguido para el UFSet dado.
// COSTO TIEMPO: O(1 amortizado).
// COSTO ESPACIAL: O(log N).
    if (elem->padre != elem){
        elem->padre = findUFS(elem->padre);
    }
    return elem->padre;
}

void unionUFS(UFSet ufset1, UFSet ufset2){
// PROPÓSITO: Calcula la unión entre los conjuntos ufset1 y ufset2.
// COSTO TIEMPO: O(1 amortizado).
// COSTO ESPACIAL: O(1).
    UFSet root1 = findUFS(ufset1);
    UFSet root2 = findUFS(ufset2);

    if(root1 != root2){
        if(root1->rango < root2->rango){
            root1->padre = root2;
        } else if(root1->rango > root2->rango){
            root2->padre = root1;
        } else {
            root2->padre = root1;
            root1->rango += 1;
        }
    }
}

ELEM_TYPE elemUFS(UFSet ufset){
// PROPÓSITO: Devuelve el valor asociado a elemento de tipo UFSet.
// COSTO TIEMPO: O(1).
// COSTO ESPACIAL: O(1).
    return ufset->valor;
}