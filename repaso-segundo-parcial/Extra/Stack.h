#include <iostream>
using namespace std;

// #################################################################################################################### \\

struct StackStr{
// INVARIANTE DE REPRESENTACIÓN:
//  * El orden de entrada o salida de "elems" es en base a LIFO (Last In - First Out).
//  * El size del Stack es menor o igual a la capacity del mismo.
    int capacity;   // Representa la capacidad total del Stack.
    int* elems;     // Representa el array con los elementos del Stack.
    int size;       // Representa la cantidad actual de elementos en el Stack.
};

typedef StackStr* Stack;

// #################################################################################################################### \\

Stack emptyS();
// PROPÓSITO: Describe un Stack vacío.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

bool isEmptyS(Stack s);
// PROPÓSITO: Indica si el Stack dado está vacío.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void push(int x, Stack s);
// PROPÓSITO: Inserta un elemento en el tope del Stack dado.
// COSTO TIEMPO: O(1) Amortizado.
// COSTO ESPACIO: O(1).

int top(Stack s);
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