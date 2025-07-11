#include <iostream>
using namespace std;

// #################################################################################################################### \\

struct MaybeIntStr{
// INVARIANTES DE REPRESENTACIÓN:
//  * Todo maybe con haveValue true debe tener un valor asignado.
    bool haveValue; // Representa si el dato tiene un valor o no.
    int value;      // Representa el valor que tenga el dato, que puede tenerlo o no.
};

typedef MaybeIntStr* MaybeInt; // No puede ser un puntero a NULL.

// #################################################################################################################### \\

MaybeInt nothing();
// PROPÓSITO: Describe un dato sin un valor asignado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

MaybeInt just(int x);
// PROPÓSITO: Describe un dato con el valor asignado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

int fromJust(MaybeInt m);
// PROPÓSITO: Describe el valor del dato dado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

bool isNothing(MaybeInt m);
// PROPÓSITO: Indica si el dato dado no tiene valor.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

// #################################################################################################################### \\