#include <iostream>
using namespace std;

// #################################################################################################################### \\

struct FSNode{
// INVARIANTES DE REPRESENTACIÓN:
//	* Si isFile es true:
//		* children es true.
//		* childcount es cero.
//	* Si isFile es false:
//		* children es un array de tamaño capacidad.
//		* childcount es menor a capacidad.
//	* Dos nodos no pueden ser ancestros entre sí (no puede haber ciclos).
	string nombre;		// Representa al nombre del nodo.
	bool isFile;		// Representa si el nodo es un archivo o directorio. 
	FSNode** children;	// Representa un array de los hijos del nodo (si es un directorio), o NULL si es un archivo.
	int childCount;		// Representa la cantidad de hijos que tiene el nodo.
};

struct StackStr{
// INVARIANTE DE REPRESENTACIÓN:
//  * El orden de entrada o salida de "elems" es en base a LIFO (Last In - First Out).
//  * El size del Stack es menor o igual a la capacity del mismo.
    int capacity;       // Representa la capacidad total del Stack.
    ELEM_TYPE* elems;   // Representa el array con los elementos del Stack.
    int size;           // Representa la cantidad actual de elementos en el Stack.
};

typedef StackStr* Stack;
typedef FSNode* ELEM_TYPE;

// #################################################################################################################### \\

Stack emptyS();
// PROPÓSITO: Describe un Stack vacío.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

bool isEmptyS(Stack s);
// PROPÓSITO: Indica si el Stack dado está vacío.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void push(ELEM_TYPE x, int index, Stack s);
// PROPÓSITO: Inserta un elemento en el tope del Stack dado.
// COSTO TIEMPO: O(1) Amortizado.
// COSTO ESPACIO: O(1).

ELEM_TYPE top(Stack s);
// PROPÓSITO: Describe el elemento en el tope del Stack dado sin eliminarlo.
// PRECONDICIÓN: El Stack no debe estar vacío.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void pop(Stack s);
// PROPÓSITO: Elimina el elemento en el tope del Stack dado.
// PRECONDICIÓN: El Stack no debe estar vacío.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

int size(Stack s);
// PROPÓSITO: Describe el tamaño del Stack dado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void destroyS(Stack s);
// PROPÓSITO: Libera la memoria reservada por el Stack dado.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).

// #################################################################################################################### \\