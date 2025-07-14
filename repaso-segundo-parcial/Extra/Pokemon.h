#include <iostream>
using namespace std;

// #################################################################################################################### \\

struct PokeStr{
// INVARIANTES DE REPRESENTACIÓN:
//  * La vida no puede ser menor a 0.
//  * El rango de cada pókemon está determinado por la jerarquía de su nombre.
    int vida;       // Representa la vida del pókemon.
    string nombre;  // Representa el nombre del pókemon.
};

typedef PokeStr* Pokemon;

// #################################################################################################################### \\

Pokemon crearPokemon(string nombre);
// PROPÓSITO: Describe un pókemon con 100 de vida y con el nombre dado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void restarVida(Pokemon p);
// PROPÓSITO: Le resta 10 de vida al pókemon dado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void cambiarNombre(Pokemon p, string nombre);
// PROPÓSITO: Le cambia el nombre al pókemon dado por el string dado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

bool estaVivo(Pokemon p);
// PROPÓSITO: Indica si el pókemon dado se encuentra vivo.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

string getNombre(Pokemon p);
// PROPÓSITO: Describe el nombre del pókemon dado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

int getVida(Pokemon p);
// PROPÓSITO: Describe la vida del pókemon dado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void luchar(Pokemon p, Pokemon r);
// PROPÓSITO: Dos pókemon dados luchan entre sí, quitándole 10 de vida al que tenga un nombre de menor rango.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void lucharN(int n, Pokemon p, Pokemon r);
// PROPÓSITO: Dos pókemon dados luchan entre sí la cantidad de veces dadas.
// COSTO TIEMPO: O(N). Siendo n el número dado por parámetro.
// COSTO ESPACIO: O(1).

void destruir(Pokemon p);
// PROPÓSITO: Destruye al pókemon dado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

// #################################################################################################################### \\