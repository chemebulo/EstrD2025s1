#ifndef _UFSET_H
#define _UFSET_H

#include <iostream>
#include "Equipo.h"

using namespace std;

// * UFSet.h define la representación del tipo UFSet (utilizado para modelar tanto los elementos 
//   de un UFSet como el conjunto en sí), y la interfaz de un UFSet.

#define ELEM_TYPE Equipo

struct UFNode;
typedef UFNode* UFSet;

UFSet createUFS(ELEM_TYPE value);
// PROPÓSITO: Inicializa el UFSet ufset, cuyo valor asociado será value.

UFSet findUFS(UFSet elem);
// PROPÓSITO: Encuentra el elemento distinguido para el UFSet dado.

void unionUFS(UFSet ufset1, UFSet ufset2);
// PROPÓSITO: Calcula la unión entre los conjuntos ufset1 y ufset2.

ELEM_TYPE elemUFS(UFSet ufset);
// PROPÓSITO: Devuelve el valor asociado a elemento de tipo UFSet.

#endif