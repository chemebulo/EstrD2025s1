#include <iostream>
using namespace std;

struct ArrayListSt {
    int cantidad;   // Cantidad de elementos.
    int* elementos; // Array de elementos.
    int capacidad;  // Tamaño del array.
};

typedef ArrayListSt* ArrayList;

ArrayList newArrayList();
// PROPÓSITO: Crea una lista con 0 elementos.
// NOTA: Empezar el array list con capacidad 16.

ArrayList newArrayListWith(int capacidad);
// PROPÓSITO: Crea una lista con 0 elementos y una capacidad dada por parámetro.

int lengthAL(ArrayList xs);
// PROPÓSITO: Devuelve la cantidad de elementos existentes.

int get(int i, ArrayList xs);
// PROPÓSITO: Devuelve el iésimo elemento de la lista.

void set(int i, int x, ArrayList xs);
// PROPÓSITO: Reemplaza el iésimo elemento por otro dado.

void resize(int capacidad, ArrayList xs);
// PROPÓSITO: Decrementa o aumenta la capacidad del array.
// NOTA: En caso de decrementarla, se pierden los elementos del final de la lista.

void add(int x, ArrayList xs);
// PROPÓSITO: Agrega un elemento al final de la lista.

void remove(ArrayList xs);
// PROPÓSITO: Borra el último elemento de la lista.